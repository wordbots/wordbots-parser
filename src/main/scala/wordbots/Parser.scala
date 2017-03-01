package wordbots

import com.workday.montague.ccg._
import com.workday.montague.parser._
import com.workday.montague.semantics._
import com.workday.montague.semantics.FunctionReaderMacro.λ

import scala.util.{Failure, Success, Try}

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
    // println(s"Tokens: ${tokenizer(input).mkString("[\"", "\", \"", "\"]")}")
    // println(s"Unrecognized tokens: ${findUnrecognizedTokens(input).mkString("[\"", "\", \"", "\"]")}")
    println(s"Parse result: $output")
    println(s"Error diagnosis: ${diagnoseError(input, result.bestParse)}")
    println(s"Generated JS code: $code")
    // scalastyle:on regex

    // For debug purposes, output the best parse tree (if one exists) to SVG.
    // result.bestParse.foreach(result => new PrintWriter("test.svg") { write(result.toSvg); close() })
  }

  def parse(input: String): SemanticParseResult[CcgCat] = parse(input, tokenizer)

  def findUnrecognizedTokens(input: String): Seq[String] = {
    val tokens = tokenizer(input)
    val lexicon = Lexicon.lexicon

    tokens.filter { token =>
      !lexicon.map.keys.exists(_.split(' ').contains(token)) && lexicon.funcs.forall(_(token).isEmpty)
    }
  }

  def diagnoseError(input: String, parseResult: Option[SemanticParseNode[CcgCat]]): Option[String] = {
    parseResult.map(_.semantic) match {
      case Some(Form(v: AstNode)) =>
        // Handle successful parse.
        parseResult.map(_.syntactic.category).getOrElse("None") match {
          case "S" =>
            AstValidator.validate(v) match {
              case Success(_) => None
              case Failure(ex: Throwable) => Some(ex.getMessage)
            }
          case category => Some(s"Parser did not produce a complete sentence - expected category: S, got: $category")
        }
      case _ =>
        // Handle failed parse.
        if (findUnrecognizedTokens(input).nonEmpty) {
          Some(s"Unrecognized word(s): ${findUnrecognizedTokens(input).mkString(", ")}")
        } else if (parseWithLexicon(input, Lexicon.syntaxLexicon).bestParse.isEmpty) {
          Some(s"parse failed (${diagnoseSyntaxError(input)})")
        } else {
          parseResult.map(_.exs.nonEmpty) match {
            case Some(true) =>
              val msgs = parseResult.get.exs.map (
                _.getMessage
                  .replace("cannot be cast to", "is not a")
                  .replaceAllLiterally("$", "")
                  .replaceAllLiterally("wordbots.", "")
              )

              Some(s"Parse failed (semantic mismatch - ${msgs.mkString(", ")})")
            case _ => Some("Parse failed (semantic mismatch)")
          }
        }
    }
  }

  private def parseWithLexicon(input: String, lexicon: ParserDict[CcgCat]): SemanticParseResult[CcgCat] = {
    new SemanticParser[CcgCat](lexicon).parse(input, tokenizer)
  }

  private def tokenizer(str: String): IndexedSeq[String] = {
    str.trim.toLowerCase.split("\\s+|[.?!,]").filter("" !=)
  }

  sealed trait Edit
  case class Delete(idx: Int) extends Edit
  case class Replace(idx: Int, pos: String) extends Edit
  case class Insert(idx: Int, pos: String) extends Edit

  private def diagnoseSyntaxError(input: String): String = {
    val words = input.split(" ")

    findValidEdits(input).headOption match {
      case Some(Delete(idx)) =>
        s"syntax error - unexpected word '${words(idx)}'"
      case Some(Replace(idx, pos)) =>
        val context = if (idx > 0) s"after '${words(idx - 1)}'" else s"before '${words(idx + 1)}'"
        s"syntax error - expected $pos $context but got '${words(idx)}' instead"
      case Some(Insert(idx, pos)) =>
        s"syntax error - '${words(idx)}' should be followed by $pos"
      case None => "syntax error"
    }
  }

  private def findValidEdits(input: String): Stream[Edit] = {
    val words = input.split(" ")
    val testLexicon = Lexicon.syntaxLexicon +
      ("(n)" -> N) + ("(np)" -> NP) + ("(num)" -> Num) + ("(adj)" -> Adj) + ("(adv)" -> Adv) + ("(rel)" -> Rel) + ("(s)" -> S)

    for {
      i <- (0 until words.length).toStream
      (cat, pos) <- Map(
        "n" -> "a noun", "np" -> "a noun phrase", "num" -> "a number",
        "adj" -> "an adjective", "adv" -> "an adverb", "rel" -> "a relative clause", "s" -> "a sentence"
      ).toStream
      deleted = words.slice(0, i).mkString(" ") + words.slice(i + 1, words.length).mkString(" ")
      replaced = words.slice(0, i).mkString(" ") + " (" + cat + ") " + words.slice(i + 1, words.length).mkString(" ")
      inserted = words.slice(0, i + 1).mkString(" ") + " (" + cat + ") " + words.slice(i + 1, words.length).mkString(" ")
      (candidate, edit) <- Seq((deleted, Delete(i)), (replaced, Replace(i, pos)), (inserted, Insert(i, pos)))
      if parseWithLexicon(candidate, testLexicon).bestParse.isDefined
    } yield edit
  }

  /* Not used right now because it's too computationally expensive - N^2 or N^3 syntactic parses for a length-N input.
  private def findValidSubstrings(input: String): Stream[(Int, Int)] = {
    val words = input.split(" ")

    for {
      length <- (words.length to 1 by -1).toStream
      startIdx <- (0 to (words.length - length)).toStream
      endIdx = startIdx + length
      substring = words.slice(startIdx, endIdx).mkString(" ")
      if parseWithLexicon(substring, Lexicon.syntaxLexicon).bestParse.isDefined
      // if findValidEdits(substring).nonEmpty
    } yield (startIdx, endIdx)
  }
  */
}
