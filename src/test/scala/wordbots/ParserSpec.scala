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
    parse("Draw a card") should equal (Draw(Self, Scalar(1)))
    parse("Destroy a robot") should equal (Destroy(Choose(ObjectsInPlay(Robot))))
    parse("Gain 2 energy") should equal (ModifyEnergy(Self, Plus(Scalar(2))))
    parse("Deal 2 damage to a robot") should equal (DealDamage(Choose(ObjectsInPlay(Robot)), Scalar(2)))
    parse("Deal 2 damage to yourself") should equal (DealDamage(Self, Scalar(2)))
    parse("Discard a card") should equal (Discard(Self, Scalar(1)))
    parse("Your opponent must discard a card") should equal (Discard(Opponent, Scalar(1)))
    parse("Give a robot +1 speed") should equal (ModifyAttribute(Choose(ObjectsInPlay(Robot)), Speed, Plus(Scalar(1))))
  }

  it should "parse more complex actions" in {
    parse("Deal 2 damage to a robot that has 3 or less speed") shouldEqual
      DealDamage(Choose(ObjectsMatchingCondition(Robot, AttributeComparison(Speed, LessThanOrEqualTo(Scalar(3))))), Scalar(2))

    // The following action texts were provided by James:
    parse("Set all stats of all creatures in play to 3") shouldEqual
      SetAttribute(All(ObjectsInPlay(Robot)), AllAttributes, Scalar(3))
    parse("Draw cards equal to the number of creatures you control") shouldEqual
      Draw(Self, Count(ObjectsMatchingCondition(Robot, ControlledBy(Self))))
    parse("Deal damage to a creature equal to the total power of creatures you control") shouldEqual
      DealDamage(Choose(ObjectsInPlay(Robot)), AttributeSum(ObjectsMatchingCondition(Robot, ControlledBy(Self)), Attack))
    parse("Double the attack of all creatures in play") shouldEqual
      ModifyAttribute(All(ObjectsInPlay(Robot)),Attack,Multiply(Scalar(2)))
    // "Double the attack and halve the life (rounded up) of all creatures in play"
  }

  it should "deal with ambiguous uses of 'all'" in {
    parse("Draw cards equal to the total power of all creatures you control") shouldEqual
      parse("Draw cards equal to the total power of creatures you control")

    parse("Deal damage to a creature equal to the total power of creatures you control") shouldEqual
      parse("Deal damage to a creature equal to the total power of all creatures you control")
  }

  /*it should "parse triggers for creatures" in {
    // The following trigger texts were provided by James:
    // "This creature gains a second move action after attacking"
    // "At the beginning of each of your turns, this creature gains 1 attack"
    // "At the end of each turn, each creature takes 1 damage"
    // "When this creature attacks, it deals damage to all adjacent creatures"
    // "When this creature is summoned, reduce the cost of a card in your hand by 3"
    // "Whenever this creature takes damage, draw a card"
  }*/

  it should "generate JS code for actions" in {
    generateJS("Draw a card") should be ("(function () { actions['draw'](targets['self'](), 1); })")
    generateJS("Destroy a robot") should be ("(function () { actions['destroy'](targets['choose'](objectsInPlay('robot'))); })")
    generateJS("Gain 2 energy") should be ("(function () { actions['modifyEnergy'](targets['self'](), function (x) { return x + 2; }); })")
    generateJS("Give a robot +1 speed") should be ("(function () { actions['modifyAttribute'](targets['choose'](objectsInPlay('robot')), 'speed', function (x) { return x + 1; }); })")
  }
}
