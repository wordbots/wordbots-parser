package wordbots

import com.workday.montague.ccg.CcgCat
import com.workday.montague.parser.SemanticParseNode
import com.workday.montague.semantics.{Form, Lambda, Nonsense}
import scalaz.Memo

import java.util.Collections
import scala.util.{Failure, Success}
import scala.collection.JavaConversions._

case class ParserError(description: String, suggestions: Seq[String] = Seq.empty, stats: ErrorAnalyzerStats = ErrorAnalyzerStats())

case class ErrorAnalyzerStats(
    syntacticParsesTried: Int = 0,
    syntacticParsesSucceeded: Int = 0,
    semanticParsesTried: Int = 0,
    semanticParsesSucceeded: Int = 0,
    timeSpentSyntacticParsingNs: Long = 0L,
    timeSpentSemanticParsingNs: Long = 0L
) {
  // scalastyle:off method.name
  def +(other: ErrorAnalyzerStats): ErrorAnalyzerStats = ErrorAnalyzerStats(
    syntacticParsesTried + other.syntacticParsesTried,
    syntacticParsesSucceeded + other.syntacticParsesSucceeded,
    semanticParsesTried + other.semanticParsesTried,
    semanticParsesSucceeded + other.semanticParsesSucceeded,
    timeSpentSyntacticParsingNs + other.timeSpentSyntacticParsingNs,
    timeSpentSemanticParsingNs + other.timeSpentSemanticParsingNs
  )
  // scalastyle:on method.name

  override def toString(): String = {
    s"Spent ${timeSpentSyntacticParsingNs / 1000000} ms syntactic parsing ($syntacticParsesSucceeded/$syntacticParsesTried) and ${timeSpentSemanticParsingNs / 1000000} ms semantic parsing ($semanticParsesSucceeded/$semanticParsesTried)"
  }
}

case class ValidEdits(
    edits: Stream[Edit],
    stats: ErrorAnalyzerStats
)

case class Suggestions(
    suggestions: Seq[String] = Seq.empty,
    stats: ErrorAnalyzerStats = ErrorAnalyzerStats()
)

object ErrorAnalyzer {
  import Semantics._

  val MAX_NUM_SUGGESTIONS = 10

  /** Time a block (as a by-name argument), returning the result as well as the time it took to executed it. */
  def time[R](block: => R): (R, Long) = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    (result, t1 - t0)
  }

  // Note: "fast mode" disables finding syntax/semantics suggestions and just does the bare minimum to diagnose the error
  def diagnoseError(input: String, parseResult: Option[SemanticParseNode[CcgCat]], isFastMode: Boolean = false)
                   (implicit validationMode: ValidationMode = ValidateUnknownCard): Option[ParserError] = {
    parseResult.map(_.semantic) match {
      case Some(Form(ast: AstNode)) =>
        // Handle successful semantic parse.
        handleSuccessfulParse(input, parseResult, ast)
      case Some(f: Form[_]) =>
        // Handle a semantic parse that finishes but produces an unexpected result.
        val suggestions: Suggestions = if (isFastMode) Suggestions() else getSyntacticSuggestions(input)
        Some(
          ParserError(
            s"Parser did not produce a valid expression - expected an AstNode, got: $f",
            suggestions.suggestions,
            suggestions.stats
          )
        )
      case Some(l: Lambda[_]) =>
        // Handle successful syntactic parse but incomplete semantic parse.
        val firstArgType = l.k.toString.split(": ")(1).split(" =>")(0)
        Some(ParserError(s"Parse failed (missing $firstArgType)"))
      case Some(Nonsense(_)) =>
        // Handle successful syntactic parse but failed semantic parse.
        Some(diagnoseSemanticsError(input, parseResult, isFastMode))
      case _ =>
        // Handle failed parse.
        if (findUnrecognizedTokens(input).nonEmpty) {
          Some(ParserError(s"Unrecognized word(s): ${findUnrecognizedTokens(input).mkString(", ")}"))
        } else {
          Some(diagnoseSyntaxError(input, isFastMode))
        }
    }
  }

  def findUnrecognizedTokens(input: String): Seq[String] = {
    val tokens = Parser.tokenizer(input)
    val lexicon = Lexicon.lexicon

    tokens.filter { token =>
      !lexicon.map.keys.exists(_.split(' ').contains(token)) && lexicon.funcs.forall(_(token).isEmpty)
    }
  }

  private def handleSuccessfulParse(input: String, parseResult: Option[SemanticParseNode[CcgCat]], ast: AstNode)
                                   (implicit validationMode: ValidationMode = ValidateUnknownCard): Option[ParserError] = {
    // Does the parse produce a sentence (CCG category S)?
    parseResult.map(_.syntactic.category).getOrElse("None") match {
      case "S" =>
        // Does the parse produce a valid AST?
        AstValidator(validationMode).validate(ast) match {
          case Success(_) => None
          case Failure(ex: Throwable) =>
            val suggestions: Seq[String] = ex match {
              case ValidationError("Not a valid passive, triggered, or activated ability.") => Seq(s"Startup: $input")
              case _ => Seq.empty
            }
            Some(ParserError(ex.getMessage, suggestions))
        }
      case category => Some(ParserError(s"Parser did not produce a complete sentence - expected category: S, got: $category"))
    }
  }

  private def diagnoseSyntaxError(input: String, isFastMode: Boolean = false): ParserError = {
    if (isFastMode) {
      ParserError(s"Parse failed (syntax error)")
    } else {
      val words = input.split(" ")
      val validEdits = findValidEdits(words)
      val error: Option[String] = validEdits.edits.headOption.map(_.description(words))
      val suggestions: Suggestions = getSyntacticSuggestions(input, Some(validEdits))

      ParserError(s"Parse failed (${error.getOrElse("syntax error")})", suggestions.suggestions, suggestions.stats)
    }
  }

  private def diagnoseSemanticsError(input: String, parseResult: Option[SemanticParseNode[CcgCat]], isFastMode: Boolean = false): ParserError = {
    val exceptions: Set[String] = parseResult.map(_.exs).getOrElse(Set()).map(
      _.getMessage
        .replace("cannot be cast to", "is not a")
        .replaceAllLiterally("Semantics$", "")
        .replaceAllLiterally("$", "")
        .replaceAllLiterally("wordbots.", "")
    )
    val errorMsg = if (exceptions.nonEmpty) exceptions.mkString(" - ", ", ", "") else ""

    val suggestions: Suggestions = {
      if (isFastMode) {
        Suggestions()
      } else {
        val semanticSuggestions = getSemanticSuggestions(input)
        if (semanticSuggestions.suggestions.isEmpty) getSyntacticSuggestions(input) else semanticSuggestions
      }
    }

    ParserError(s"Parse failed (semantics mismatch$errorMsg)", suggestions.suggestions, suggestions.stats)
  }

  private def getSyntacticSuggestions(input: String, precomputedValidEdits: Option[ValidEdits] = None): Suggestions = {
    var semanticParsesTried = 0
    var semanticParsesSucceeded = 0
    var timeSpentSemanticParsingNs = 0L

    def checkIfSemanticallyValidAndUpdateStats(candidate: String): Boolean = {
      semanticParsesTried += 1
      val (result, timeInNs) = time { isSemanticallyValid(candidate) }
      semanticParsesSucceeded += (if (result) 1 else 0)
      timeSpentSemanticParsingNs += timeInNs
      result
    }

    val words = input.split(" ")
    val validEdits = precomputedValidEdits.getOrElse(findValidEdits(words))
    val syntacticStats = validEdits.stats

    val phrasesToTry: Stream[String] = validEdits.edits.flatMap(_(words))
    val validPhrases: Seq[String] = phrasesToTry.distinct.filter(checkIfSemanticallyValidAndUpdateStats).take(MAX_NUM_SUGGESTIONS).toIndexedSeq

    val semanticStats = ErrorAnalyzerStats(semanticParsesTried = semanticParsesTried, semanticParsesSucceeded = semanticParsesSucceeded, timeSpentSemanticParsingNs = timeSpentSemanticParsingNs)
    Suggestions(validPhrases, syntacticStats + semanticStats)
  }

  private def getSemanticSuggestions(input: String): Suggestions = {
    var semanticParsesTried = 0
    var semanticParsesSucceeded = 0
    var timeSpentSemanticParsingNs = 0L

    def checkIfSemanticallyValidAndUpdateStats(candidate: String): Boolean = {
      semanticParsesTried += 1
      val (result, timeInNs) = time { isSemanticallyValid(candidate) }
      semanticParsesSucceeded += (if (result) 1 else 0)
      timeSpentSemanticParsingNs += timeInNs
      result
    }

    def semanticReplacements(terminal: SemanticParseNode[CcgCat]): Seq[String] = {
      val token = terminal.parseTokenString
      val alternatives = Lexicon.termsInCategory(terminal.syntactic)
      alternatives.map(alternative => s" ${input.toLowerCase} ".replaceFirst(s" $token ", s" $alternative ").trim.capitalize)
    }

    val terminalNodes: Seq[SemanticParseNode[CcgCat]] = syntacticParse(input).get.terminals
    val suggestions = terminalNodes.flatMap(semanticReplacements).toStream.distinct.filter(checkIfSemanticallyValidAndUpdateStats).take(MAX_NUM_SUGGESTIONS).toIndexedSeq

    val semanticStats = ErrorAnalyzerStats(semanticParsesTried = semanticParsesTried, semanticParsesSucceeded = semanticParsesSucceeded, timeSpentSemanticParsingNs = timeSpentSemanticParsingNs)
    Suggestions(suggestions, semanticStats)
  }

  //scalastyle:off
  private def findValidEdits(words: Seq[String]): ValidEdits = {
    var syntacticParsesTried = 0
    var syntacticParsesSucceeded = 0
    var semanticParsesTried = 0
    var semanticParsesSucceeded = 0
    var timeSpentSyntacticParsingNs = 0L
    var timeSpentSemanticParsingNs = 0L

    def checkIfSyntacticallyValidAndUpdateStats(candidate: String): Boolean = {
      syntacticParsesTried += 1
      val (result, timeInNs) = time { isSyntacticallyValid(candidate) }
      syntacticParsesSucceeded += (if (result) 1 else 0)
      timeSpentSyntacticParsingNs += timeInNs
      result
    }

    def checkIfSemanticallyValidAndUpdateStats(candidate: String): Boolean = {
      semanticParsesTried += 1
      val (result, timeInNs) = time { isSemanticallyValid(candidate) }
      semanticParsesSucceeded += (if (result) 1 else 0)
      timeSpentSemanticParsingNs += timeInNs
      result
    }

    // The time complexity of findValidEdits() is O(W*C) where W is the # of words and C is the # of CCG categories to try.
    // So ...
    val categories: Map[String, CcgCat] = {
      if (words.length <= 6) {
        // ... for shorter inputs, try all categories.
        Lexicon.categoriesMap.mapValues(_.head._1)
      } else if (words.length <= 15) {
        // ... for medium-length inputs, only try terminal categories.
        Lexicon.terminalCategoriesMap.mapValues(_.head._1)
      } else {
        // ... for very long inputs, don't try any categories (only attempt deletions).
        Map()
      }
    }

    val insertions: Stream[Edit] = for {
      i <- words.indices.inclusive.toStream
      (cat, pos) <- categories.toStream
      candidate = words.slice(0, i).mkString(" ") + s" $cat " + words.slice(i, words.length).mkString(" ")
      if checkIfSyntacticallyValidAndUpdateStats(candidate)
    } yield Insert(i, pos)

    val deletions: Stream[Edit] = for {
      i <- words.indices.toStream
      candidate = words.slice(0, i).mkString(" ") + " " + words.slice(i + 1, words.length).mkString(" ")
      if checkIfSyntacticallyValidAndUpdateStats(candidate)
    } yield Delete(i)

    // For phrases that are expansions of a single keyword, consider deleting the full expansion as a single "delete" operation
    val phraseDeletions: Seq[Edit] = for {
      phrase <- Seq("When this object is played,", "When this object is destroyed,")  // expansions for "Startup:" and "Shutdown:", respectively (see constants.ts in wordbots-core)
      wordsInPhrase = phrase.split(" ").toSeq
      startIdx = Collections.indexOfSubList(words, wordsInPhrase)
      if startIdx > -1
      endIdx = startIdx + wordsInPhrase.length - 1

      candidate = words.slice(0, startIdx).mkString(" ") + " " + words.slice(endIdx + 1, words.length).mkString(" ")
      if checkIfSyntacticallyValidAndUpdateStats(candidate)
    } yield DeleteChunk(startIdx, endIdx)

    val replacements: Stream[Edit] = {
      // Don't look for replacements if there are any (syntactically and semantically) valid deletions!!
      // This is because the token being deleted could be replaced with any identity term, resulting in a lot of nonsense candidates.
      // e.g. "Discard your opponent's hand" -> "Discard your hand" -> "Discard your it deals hand", "Discard your takes hand", etc.
      if (deletions.flatMap(_(words)).exists(checkIfSemanticallyValidAndUpdateStats)) {
        Stream.empty
      } else {
        for {
          i <- words.indices.toStream
          (cat, pos) <- categories.toStream
          candidate = words.slice(0, i).mkString(" ") + s" $cat " + words.slice(i + 1, words.length).mkString(" ")
          if checkIfSyntacticallyValidAndUpdateStats(candidate)
        } yield Replace(i, pos)
      }
    }

    val singleWordReplacements: Stream[Edit] = {
      // If no valid edits have been found so far AND the string is short enough (<10 words) ...
      // as a last-ditch effort, also try just replacing one word at a time with a common single-word token
      if (words.length >= 10 || (insertions ++ deletions ++ replacements).flatMap(_(words)).exists(checkIfSemanticallyValidAndUpdateStats)) {
        Stream.empty
      } else {
        for {
          i <- words.indices.toStream
          term <- Lexicon.mapOfTermsToUsages.filter(_._2 >= 10).keys.filter((t) => t.split(" ").length == 1).toStream
        } yield ExactReplace(i, term)
      }
    }

    ValidEdits(
      insertions ++ deletions ++ phraseDeletions ++ replacements ++ singleWordReplacements,
      ErrorAnalyzerStats(syntacticParsesTried, syntacticParsesSucceeded, semanticParsesTried, semanticParsesSucceeded, timeSpentSyntacticParsingNs, timeSpentSemanticParsingNs)
    )
  }

  private val isSyntacticallyValid: String => Boolean = {
    Memo.mutableHashMapMemo { candidate =>
      candidate.nonEmpty && syntacticParse(candidate).isDefined
    }
  }

  private val isSemanticallyValid: String => Boolean = {
    Memo.mutableHashMapMemo { candidate =>
      val parseResult = Parser.parse(candidate).bestParse
      parseResult.map(_.semantic) match {
        // Is the semantic parse successful?
        case Some(Form(v: AstNode)) =>
          // Does the parse produce a sentence (CCG category S), and are the semantics valid?
          parseResult.get.syntactic.category == "S" && AstValidator().validate(v).isSuccess
        case _ => false
      }
    }
  }

  private def syntacticParse(input: String): Option[SemanticParseNode[CcgCat]] = {
    Parser.parseWithLexicon(input, Lexicon.syntaxOnlyLexicon).bestParse
  }
}
