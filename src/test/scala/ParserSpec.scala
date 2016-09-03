package wordbots

import com.workday.montague.semantics.Form
import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {
  def parse(input: String) = {
    Parser.parse(input).bestParse.get.semantic match {
      case Form(value) => value
    }
  }

  it should "parse simple actions" in {
    parse("Draw a card") should be (Draw(Self, 1))
    parse("Destroy a robot") should be (Destroy(Choose(Robot, NoCondition)))
    parse("Gain 2 energy") should be (EnergyDelta(Self, Plus(2)))
    parse("Deal 2 damage to a robot") should be (DealDamage(Choose(Robot, NoCondition), 2))
    parse("Deal 2 damage to yourself") should be (DealDamage(Self, 2))
    parse("Discard a card") should be (Discard(Self, 1))
    parse("Your opponent must discard a card") should be (Discard(Opponent, 1))
  }

  it should "parse more complex actions with relative clauses" in {
    parse("Deal 2 damage to a robot that has 3 or less speed") should be (
      DealDamage(Choose(Robot, AttributeComparison(Speed, LessThanOrEqualTo(3))), 2)
    )
  }
}