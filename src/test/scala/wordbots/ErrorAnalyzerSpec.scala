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

//  it should "handle invalid filters" in {
//    analyze("Destroy a robot with your hand") shouldEqual "some error"//TODO make this produce a useful error - like "'your hand' is not a vaild condition"
//    analyze("Destroy a robot with +3 attack") shouldEqual "semantics mismatch - 3 attack is not a thingy."
//  }
}
