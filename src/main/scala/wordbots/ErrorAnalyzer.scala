package wordbots

import com.workday.montague.ccg.CcgCat
import com.workday.montague.parser.SemanticParseNode
import com.workday.montague.semantics.{Form, Lambda, Nonsense}

import scala.util.{Failure, Success}

case class ParserError(description: String, suggestions: Set[String] = Set.empty)

object ErrorAnalyzer {
  import Semantics._

  // Note: "fast mode" disables finding syntax/semantics suggestions and just does to bare minimum to diagnose the error
  def diagnoseError(input: String, parseResult: Option[SemanticParseNode[CcgCat]], isFastMode: Boolean = false)
                   (implicit validationMode: ValidationMode = ValidateUnknownCard): Option[ParserError] = {
    parseResult.map(_.semantic) match {
      case Some(Form(ast: AstNode)) =>
        // Handle successful semantic parse.
        handleSuccessfulParse(input, parseResult, ast)
      case Some(f: Form[_]) =>
        // Handle a semantic parse that finishes but produces an unexpected result.
        Some(ParserError(s"Parser did not produce a valid expression - expected an AstNode, got: $f"))
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
            val suggestions: Set[String] = ex match {
              case ValidationError("Not a valid passive, triggered, or activated ability.") => Set(s"Startup: $input")
              case _ => Set()
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
      val edits = findValidEdits(words)
      val error: Option[String] = edits.headOption.map(_.description(words))

      ParserError(s"Parse failed (${error.getOrElse("syntax error")})", getSyntacticSuggestions(input))
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

    val suggestions: Set[String] = {
      if (isFastMode) {
        val semanticSuggestions = getSemanticSuggestions(input)
        if (semanticSuggestions.isEmpty) getSyntacticSuggestions(input) else semanticSuggestions
      } else {
        Set.empty
      }
    }

    ParserError(s"Parse failed (semantics mismatch$errorMsg)", suggestions)
  }

  private def getSyntacticSuggestions(input: String): Set[String] = {
    val words = input.split(" ")
    val edits = findValidEdits(words)
    edits.flatMap(_(words)).toSet.filter(isSemanticallyValid)
  }

  private def getSemanticSuggestions(input: String): Set[String] = {
    def semanticReplacements(terminal: SemanticParseNode[CcgCat]): Seq[String] = {
      val token = terminal.parseTokenString
      val alternatives = Lexicon.termsInCategory(terminal.syntactic)
      alternatives.map(alternative => s" ${input.toLowerCase} ".replaceFirst(s" $token ", s" $alternative ").trim.capitalize)
    }

    val terminalNodes: Seq[SemanticParseNode[CcgCat]] = syntacticParse(input).get.terminals
    terminalNodes.flatMap(semanticReplacements).toSet.filter(isSemanticallyValid)
  }

  private def findValidEdits(words: Seq[String]): Stream[Edit] = {
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
      if isSyntacticallyValid(candidate)
    } yield Insert(i, pos)

    val deletions: Stream[Edit] = for {
      i <- words.indices.toStream
      candidate = words.slice(0, i).mkString(" ") + " " + words.slice(i + 1, words.length).mkString(" ")
      if isSyntacticallyValid(candidate)
    } yield Delete(i)

    val replacements: Stream[Edit] = {
      // Don't look for replacements if there are any (syntactically and semantically) valid deletions!!
      // This is because the token being deleted could be replaced with any identity term, resulting in a lot of nonsense candidates.
      // e.g. "Discard your opponent's hand" -> "Discard your hand" -> "Discard your it deals hand", "Discard your takes hand", etc.
      if (deletions.flatMap(_(words)).toSet.filter(isSemanticallyValid).size > 0) {
        Stream.empty
      } else {
        for {
          i <- words.indices.toStream
          (cat, pos) <- categories.toStream
          candidate = words.slice(0, i).mkString(" ") + s" $cat " + words.slice(i + 1, words.length).mkString(" ")
          if isSyntacticallyValid(candidate)
        } yield Replace(i, pos)
      }
    }

    insertions ++ deletions ++ replacements
  }

  private def isSyntacticallyValid(candidate: String): Boolean = {
    candidate.nonEmpty && syntacticParse(candidate).isDefined
  }

  private def isSemanticallyValid(candidate: String): Boolean = {
    val parseResult = Parser.parse(candidate).bestParse
    parseResult.map(_.semantic) match {
      // Is the semantic parse successful?
      case Some(Form(v: AstNode)) =>
        // Does the parse produce a sentence (CCG category S), and are the semantics valid?
        parseResult.get.syntactic.category == "S" && AstValidator().validate(v).isSuccess
      case _ => false
    }
  }

  private def syntacticParse(input: String): Option[SemanticParseNode[CcgCat]] = {
    Parser.parseWithLexicon(input, Lexicon.syntaxOnlyLexicon).bestParse
  }
}
