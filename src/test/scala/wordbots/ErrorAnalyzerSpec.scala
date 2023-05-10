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

  it should "not suggest replacements when a valid deletion is possible" in {
    // We don't want to see spurious suggestions replacing "that" with identity terms like "it deals", "takes", etc
    analyze("Destroy all that robots").get.suggestions shouldEqual Set("Destroy all robots")
  }
}
