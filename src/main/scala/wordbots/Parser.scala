package wordbots

import com.workday.montague.ccg._
import com.workday.montague.parser.{ParserDict, SemanticParseResult, SemanticParser}
import com.workday.montague.semantics._

import scala.language.postfixOps
import scala.util.Try

object Parser extends SemanticParser[CcgCat](Lexicon.lexicon) {
  val VERSION = s"v${BuildInfo.version.split("-SNAPSHOT")(0)}"

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
    println(s"Parse result: $output")
    println(s"Error diagnosis: ${ErrorAnalyzer.diagnoseError(input, result.bestParse)}")
    println(s"Generated JS code: ${code.getOrElse("None")}")
    // scalastyle:on regex

    // For debug purposes, output the best parse tree (if one exists) to SVG.
    //result.bestParse.foreach(result => new java.io.PrintWriter("test.svg") { write(result.toSvg); close() })
  }

  def parse(input: String): SemanticParseResult[CcgCat] = parse(input, tokenizer)

  def parseWithLexicon(input: String, lexicon: ParserDict[CcgCat]): SemanticParseResult[CcgCat] = {
    new SemanticParser[CcgCat](lexicon).parse(input, tokenizer)
  }

  override val tokenizer: String => IndexedSeq[String] = {
    _.trim
      .toLowerCase
      .replaceAll("""[\u202F\u00A0]""", " ")
      .replaceAllLiterally("\' ", " \' ")
      .replaceAllLiterally("\'s", " \'s ")
      .replaceAllLiterally("\"", " \" ")
      .split("\\s+|[.?!,\\(\\)]")
      .filter("" !=)
  }
}
