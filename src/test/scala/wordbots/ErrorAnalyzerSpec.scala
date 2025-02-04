package wordbots

import org.scalatest._

class ErrorAnalyzerSpec extends FlatSpec with Matchers {
  def analyze(input: String): Option[ParserError] = {
    val parseResult = ErrorAnalyzer.bestValidParse(Parser.parse(input))
    ErrorAnalyzer.diagnoseError(input, parseResult)
  }

  it should "come up with suggestions for correcting syntactic errors" in {
    analyze("Destroy a robot 2 attack").get.suggestions.toSet shouldEqual Set("Give a robot 2 attack", "Destroy a robot with 2 attack")
  }

  it should "not suggest replacements when a valid deletion is possible" in {
    // We don't want to see spurious suggestions replacing "that" with identity terms like "it deals", "takes", etc
    analyze("Destroy all that robots").get.suggestions shouldEqual Seq("Destroy all robots")
  }

  it should "try single-word replacement as a last-ditch effort when all else fails" in {
    // We don't want to see spurious suggestions replacing "that" with identity terms like "it deals", "takes", etc
    analyze("After the end of your turn draw 4 cards").get.suggestions shouldEqual Seq("At the end of your turn draw 4 cards")
  }

  it should "try deleting multi-word keyword expansions like 'Startup:' as a single delete operation" in {
    // Startup: Every time this robot moves this robot gains 3 attack
    analyze("When this object is played, Every time this robot moves this robot gains 3 attack").get.suggestions shouldEqual Seq("Every time this robot moves this robot gains 3 attack")
  }
}
