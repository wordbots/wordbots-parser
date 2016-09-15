package wordbots

import com.workday.montague.semantics.Form
import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {
  def parse(input: String) = {
    println(s"Parsing $input...")
    Parser.parse(input).bestParse.get.semantic match {
      case Form(v) => {
        println(s"    $v")
        v
      }
    }
  }

  def generateJS(input: String): String = {
    CodeGenerator.generateJS(parse(input).asInstanceOf[AstNode])
  }

  it should "parse simple actions" in {
    parse("Draw a card") should be (Draw(Self, Scalar(1)))
    parse("Destroy a robot") should be (Destroy(Choose(Robot, NoCondition)))
    parse("Gain 2 energy") should be (EnergyDelta(Self, Plus(Scalar(2))))
    parse("Deal 2 damage to a robot") should be (DealDamage(Choose(Robot, NoCondition), Scalar(2)))
    parse("Deal 2 damage to yourself") should be (DealDamage(Self, Scalar(2)))
    parse("Discard a card") should be (Discard(Self, Scalar(1)))
    parse("Your opponent must discard a card") should be (Discard(Opponent, Scalar(1)))
    parse("Give a robot +1 speed") should be (AttributeDelta(Choose(Robot, NoCondition), Speed, Plus(Scalar(1))))
  }

  it should "parse more complex actions" in {
    parse("Deal 2 damage to a robot that has 3 or less speed") should be
      DealDamage(Choose(Robot, AttributeComparison(Speed, LessThanOrEqualTo(Scalar(3)))), Scalar(2))

    //* The following action texts were provided by James:
    parse("Set all stats of all creatures in play to 3") should be
      SetAttribute(All(Robot, NoCondition), AllAttributes, Scalar(3))
    // "Draw cards equal to the number of creatures you control"
    // "Deal damage to a creature equal to the total power of all creatures you control"
    // "Double the attack and halve the life (rounded up) of all creatures in play"
  }

  it should "parse triggers for creatures" in {
    //* The following trigger texts were provided by James:
    // "This creature gains a second move action after attacking"
    // "At the beginning of each of your turns, this creature gains 1 attack"
    // "At the end of each turn, each creature takes 1 damage"
    // "When this creature attacks, it deals damage to all adjacent creatures"
    // "When this creature is summoned, reduce the cost of a card in your hand by 3"
    // "Whenever this creature takes damage, draw a card"
  }

  it should "generate JS code for actions" in {
    generateJS("Draw a card") should be ("(function () { actions['draw'](targets['self'](), 1); })")
    generateJS("Destroy a robot") should be ("(function () { actions['destroy'](targets['choose']('robot', null)); })")
    generateJS("Gain 2 energy") should be ("(function () { actions['energyDelta'](targets['self'](), 2); })")
    generateJS("Give a robot +1 speed") should be ("(function () { actions['attributeDelta'](targets['choose']('robot', null), 'speed', 1); })")
  }
}
