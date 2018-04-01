package wordbots

import org.scalatest._

class ErrorAnalyzerSpec extends FlatSpec with Matchers {
  def analyze(input: String): Option[ParserError] = {
    val parseResult = Parser.parse(input).bestParse
    ErrorAnalyzer.diagnoseError(input, parseResult)
  }

  it should "come up with suggestions for correcting syntactic errors" in {
    analyze("Destroy a robot 2 attack").get.suggestions shouldEqual Set("Give a robot 2 attack", "Destroy a robot with 2 attack")
  }
}
