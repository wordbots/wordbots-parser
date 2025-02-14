package wordbots

import com.workday.montague.ccg._
import com.workday.montague.parser.{ParserDict, SemanticParseNode, SemanticParseResult, SemanticParser}
import com.workday.montague.semantics._

import scala.language.postfixOps
import scala.util.matching.Regex.Match
import scala.util.{Failure, Try}

object Parser extends SemanticParser[CcgCat](Lexicon.lexicon) {
  val VERSION = s"v${BuildInfo.version.split("-SNAPSHOT")(0)}"

  /** Entry point from the console (see [[wordbots.parse]])
    * Parses and outputs debug information.
    */
  override def main(args: Array[String]): Unit = {
    val input = args.mkString(" ")
    val result: SemanticParseResult[CcgCat] = parse(input)

    val bestParse: Option[SemanticParseNode[CcgCat]] = ErrorAnalyzer.bestValidParse(result)
    val allSemanticallyCompleteParses: List[String] = result.semanticCompleteParses.map(p => s"${p.semantic.toString} [${p.syntactic.toString}]")

    val output: String = bestParse.map(p => s"${p.semantic.toString} [${p.syntactic.toString}]").getOrElse("(failed to parse)")
    val code: Try[String] = Try { bestParse.get.semantic }.flatMap {
      case Form(v: Semantics.AstNode) => CodeGenerator.generateJS(v)
      case _ => Failure(new RuntimeException("Parser did not produce a valid expression"))
    }

    // scalastyle:off regex
    println(s"Input: $input")
    println(s"Tokens: ${tokenizer(input).mkString("[\"", "\", \"", "\"]")}")
    println(s"Parse result: $output")
    println(s"All semantically complete parse results:${allSemanticallyCompleteParses.mkString("\n  ", "\n  ", "")}")
    println(s"Error diagnosis: ${ErrorAnalyzer.diagnoseError(input, bestParse)}")
    println(s"Generated JS code: ${code.getOrElse(code.failed.get)}")
    // scalastyle:on regex

    // For debug purposes, output the best parse tree (if one exists) to SVG.
    //bestParse.foreach(result => new java.io.PrintWriter("test.svg") { write(result.toSvg); close() })
  }

  /** Parses the input. */
  def parse(input: String): SemanticParseResult[CcgCat] = parse(input, tokenizer)

  /** Parses the input with a given [[lexicon]] (used, e.g., in [[ErrorAnalyzer.syntacticParse]]). */
  def parseWithLexicon(input: String, lexicon: ParserDict[CcgCat]): SemanticParseResult[CcgCat] = {
    new SemanticParser[CcgCat](lexicon).parse(input, tokenizer)
  }

  override val tokenizer: String => IndexedSeq[String] = { str: String =>
    simpleTokenizer(
      str
        .replaceAllWith("""named "(.*)"""", m => s"named name:${encBase36(m.group(1))}")  // e.g. 'robot named "Test Bot"' => 'robot named name:1a7bu6u4ve1ro'
        .replaceAllWith(""""([^"]*)" with "([^"]*)"""", m => s"text:${encBase36(m.group(1))} with text:${encBase36(m.group(2))}")  // e.g. 'Replace "a robot" with "all robots"'
    )
  }

  private def encBase36(str: String): String = NameConverters.encodeBase36(str)

  implicit class RichString(str: String) {
    def replaceAllWith(regex: String, replacer: Match => String): String = regex.r.replaceAllIn(str, replacer)
  }
}
