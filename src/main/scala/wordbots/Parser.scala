package wordbots

import com.workday.montague.ccg._
import com.workday.montague.parser.{ParserDict, SemanticParser, SemanticParseResult, SemanticParseNode}
import com.workday.montague.semantics._

import scala.util.{Failure, Success, Try}

case class ParserError(description: String, suggestions: Seq[String] = Seq())

object Parser extends SemanticParser[CcgCat](Lexicon.lexicon) {
  override def main(args: Array[String]): Unit = {
    val input = args.mkString(" ")
    val result: SemanticParseResult[CcgCat] = parse(input)

    val output: String = result.bestParse.map(p => s"${p.semantic.toString} [${p.syntactic.toString}]").getOrElse("(failed to parse)")
    val code: Option[String] = result.bestParse.map(_.semantic).flatMap {
      case Form(v: AstNode) => Try(CodeGenerator.generateJS(v)).toOption
      case _ => None
    }

    // scalastyle:off regex
    println(s"Input: $input")
    println(s"Tokens: ${tokenizer(input).mkString("[\"", "\", \"", "\"]")}")
    println(s"Parse result: $output")
    println(s"Error diagnosis: ${diagnoseError(input, result.bestParse)}")
    println(s"Generated JS code: $code")
    // scalastyle:on regex

    // For debug purposes, output the best parse tree (if one exists) to SVG.
    // result.bestParse.foreach(result => new PrintWriter("test.svg") { write(result.toSvg); close() })
  }

  def parse(input: String): SemanticParseResult[CcgCat] = parse(input, tokenizer)

  def parseWithLexicon(input: String, lexicon: ParserDict[CcgCat]): SemanticParseResult[CcgCat] = {
    new SemanticParser[CcgCat](lexicon).parse(input, tokenizer)
  }

  def findUnrecognizedTokens(input: String): Seq[String] = {
    val tokens = tokenizer(input)
    val lexicon = Lexicon.lexicon

    tokens.filter { token =>
      !lexicon.map.keys.exists(_.split(' ').contains(token)) && lexicon.funcs.forall(_(token).isEmpty)
    }
  }

  def diagnoseError(input: String, parseResult: Option[SemanticParseNode[CcgCat]])
                   (implicit validationMode: ValidationMode = ValidateUnknownCard): Option[ParserError] = {
    parseResult.map(_.semantic) match {
      case Some(Form(v: AstNode)) =>
        // Handle successful semantic parse.
        // Does the parse produce a sentence (CCG category S)?
        parseResult.map(_.syntactic.category).getOrElse("None") match {
          case "S" =>
            // Does the parse produce a valid AST?
            AstValidator(validationMode).validate(v) match {
              case Success(_) => None
              case Failure(ex: Throwable) => Some(ParserError(ex.getMessage))
            }
          case category => Some(ParserError(s"Parser did not produce a complete sentence - expected category: S, got: $category"))
        }
      case Some(f: Form[_]) =>
        // Handle a semantic parse that finishes but produces an unexpected result.
        Some(ParserError(s"Parser did not produce a valid expression - expected an AstNode, got: $f"))
      case Some(l: Lambda[_]) =>
        // Handle successful syntactic parse but incomplete semantic parse.
        val firstArgType = l.k.toString.split(": ")(1).split(" =>")(0)
        Some(ParserError(s"Parse failed (missing $firstArgType)"))
      case Some(Nonsense(_)) =>
        // Handle successful syntactic parse but failed semantic parse.
        Some(ParserError(s"Parse failed (${diagnoseSemanticsError(parseResult)})"))
      case _ =>
        // Handle failed parse.
        if (findUnrecognizedTokens(input).nonEmpty) {
          Some(ParserError(s"Unrecognized word(s): ${findUnrecognizedTokens(input).mkString(", ")}"))
        } else {
          Some(diagnoseSyntaxError(input))
        }
    }
  }

  private def tokenizer(str: String): IndexedSeq[String] = {
    str.trim
      .toLowerCase
      .replaceAllLiterally("\' ", " \' ")
      .replaceAllLiterally("\'s", " \'s ")
      .replaceAllLiterally("\"", " \" ")
      .split("\\s+|[.?!,\\(\\)]")
      .filter("" !=)
  }

  private def diagnoseSyntaxError(input: String): ParserError = {
    def isSemanticallyValid(candidate: String): Boolean = {
      val parseResult = parse(candidate).bestParse
      parseResult.map(_.semantic) match {
        // Is the semantic parse successful?
        case Some(Form(v: AstNode)) =>
          // Does the parse produce a sentence (CCG category S)?
          parseResult.map(_.syntactic.category) == Some("S")
        case _ => false
      }
    }

    val words = input.split(" ")
    val edits = findValidEdits(input)

    val error: Option[String] = edits.headOption.map(_.description(words))
    val suggestions: Seq[String] = edits.flatMap(_(words)).toSet.filter(isSemanticallyValid).toSeq

    ParserError(s"Parse failed (${error.getOrElse("syntax error")})", suggestions)
  }

  private def diagnoseSemanticsError(parseResult: Option[SemanticParseNode[CcgCat]]): String = {
    parseResult.map(_.exs.nonEmpty) match {
      case Some(true) =>
        val msgs = parseResult.get.exs.map (
          _.getMessage
            .replace("cannot be cast to", "is not a")
            .replaceAllLiterally("$", "")
            .replaceAllLiterally("wordbots.", "")
        )

        s"semantics mismatch - ${msgs.mkString(", ")}"
      case _ => "semantics mismatch"
    }
  }

  private def findValidEdits(input: String): Stream[Edit] = {
    def isSyntacticallyValid(candidate: String): Boolean = {
      candidate.nonEmpty && parseWithLexicon(candidate, Lexicon.syntaxOnlyLexicon).bestParse.isDefined
    }

    val words = input.split(" ")

    val categories: Map[String, CcgCat] = Lexicon.categoriesMap.mapValues(_.head._1)

    val insertions: Stream[Edit] = for {
      i <- (0 until words.length + 1).toStream
      (cat, pos) <- categories.toStream
      candidate = words.slice(0, i).mkString(" ") + s" $cat " + words.slice(i, words.length).mkString(" ")
      if isSyntacticallyValid(candidate)
    } yield Insert(i, pos)

    val deletions: Stream[Edit] = for {
      i <- (0 until words.length).toStream
      candidate = words.slice(0, i).mkString(" ") + " " + words.slice(i + 1, words.length).mkString(" ")
      if isSyntacticallyValid(candidate)
    } yield Delete(i)

    val replacements: Stream[Edit] = for {
      i <- (0 until words.length).toStream
      (cat, pos) <- categories.toStream
      candidate = words.slice(0, i).mkString(" ") + s" $cat " + words.slice(i + 1, words.length).mkString(" ")
      if isSyntacticallyValid(candidate)
    } yield Replace(i, pos)

    insertions ++ deletions ++ replacements
  }
}
