package wordbots

import com.workday.montague.semantics.{Form, Nonsense}
import org.scalatest._

import scala.util.{Success, Failure}

// scalastyle:off line.size.limit
class ParserSpec extends FlatSpec with Matchers {
  def parse(input: String): Any = {
    println(s"Parsing $input...")
    Parser.parse(input).bestParse match {
      case Some(parse) => parse.semantic match {
        case Form(v: AstNode) => {
          println(s"    $v")
          CodeGenerator.generateJS(v.asInstanceOf[AstNode])  // Make sure that valid JS can be generated!
          AstValidator().validate(v) match {  // Make sure the AstValidator successfully validates the parsed ast!
            case Success(_) => v
            case f: Failure[_] => f
          }
        }
        case _ => Nonsense
      }
      case _ => Nonsense
    }
  }

  def generateJS(input: String): String = {
    CodeGenerator.generateJS(parse(input).asInstanceOf[AstNode])
  }

  it should "parse simple actions" in {
    parse("Draw a card") should equal (Draw(Self, Scalar(1)))
    parse("Destroy a robot") should equal (Destroy(Choose(ObjectsInPlay(Robot))))
    parse("Destroy a random robot") should equal (Destroy(Random(Scalar(1), ObjectsInPlay(Robot))))
    parse("Discard a random card") should equal (Discard(Random(Scalar(1), CardsInHand(Self, AnyCard))))
    parse("Gain two energy") should equal (ModifyEnergy(Self, Plus(Scalar(2))))
    parse("Deal 2 damage to a robot") should equal (DealDamage(Choose(ObjectsInPlay(Robot)), Scalar(2)))
    parse("Deal 2 damage to yourself") should equal (DealDamage(Self, Scalar(2)))
    parse("Give a robot +1 speed") should equal (ModifyAttribute(Choose(ObjectsInPlay(Robot)), Speed, Plus(Scalar(1))))

    // (From 4/10/17 playtest session:)
    parse("Give a robot 0 attack") shouldEqual
      SetAttribute(Choose(ObjectsInPlay(Robot)), Attack, Scalar(0))
    parse("Set a robot's attack to 0") shouldEqual
      SetAttribute(Choose(ObjectsInPlay(Robot)), Attack, Scalar(0))
  }

  it should "parse more complex actions" in {
    parse("Deal 2 damage to a robot that has 3 or less speed") shouldEqual
      DealDamage(Choose(ObjectsMatchingConditions(Robot, Seq(AttributeComparison(Speed, LessThanOrEqualTo(Scalar(3)))))), Scalar(2))
    parse ("Deal 1 damage to all robots adjacent to a tile") shouldEqual
      DealDamage(All(ObjectsMatchingConditions(Robot, Seq(AdjacentTo(Choose(AllTiles))))), Scalar(1))
    parse("Discard a robot card") shouldEqual
      Discard(Choose(CardsInHand(Self, Robot)))
    parse("Gain life equal to its health") shouldEqual
      ModifyAttribute(All(ObjectsMatchingConditions(Kernel, Seq(ControlledBy(Self)))), Health, Plus(AttributeValue(ItO, Health)))

    // (The following action texts were provided by James:)
    parse("Set all stats of all robots in play to 3") shouldEqual
      SetAttribute(All(ObjectsInPlay(Robot)), AllAttributes, Scalar(3))
    parse("Draw cards equal to the number of robots you control") shouldEqual
      Draw(Self, Count(ObjectsMatchingConditions(Robot, Seq(ControlledBy(Self)))))
    parse("Deal damage to a robot equal to the total power of robots you control") shouldEqual
      DealDamage(Choose(ObjectsInPlay(Robot)), AttributeSum(ObjectsMatchingConditions(Robot, Seq(ControlledBy(Self))), Attack))
    parse("Double the attack of all robots in play") shouldEqual
      ModifyAttribute(All(ObjectsInPlay(Robot)), Attack, Multiply(Scalar(2)))
    parse("Double the attack and halve the life (rounded up) of all robots in play") shouldEqual
      And(
        ModifyAttribute(All(ObjectsInPlay(Robot)), Attack, Multiply(Scalar(2))),
        ModifyAttribute(All(ObjectsInPlay(Robot)), Health, Divide(Scalar(2), RoundedUp))
      )
    parse("Destroy all robots with energy cost three or greater") shouldEqual
      Destroy(All(ObjectsMatchingConditions(Robot, Seq(AttributeComparison(Cost, GreaterThanOrEqualTo(Scalar(3)))))))

    // (From 4/10/17 playtest session:)
    parse("Destroy a robot with 4 attack or more") shouldEqual
      Destroy(Choose(ObjectsMatchingConditions(Robot, Seq(AttributeComparison(Attack, GreaterThanOrEqualTo(Scalar(4)))))))
    parse("Double the health of a robot") shouldEqual
      ModifyAttribute(Choose(ObjectsInPlay(Robot)), Health, Multiply(Scalar(2)))
  }

  it should "deal with ambiguous uses of 'all'" in {
    parse("Draw cards equal to the total power of robots you control") shouldEqual
      parse("Draw cards equal to the total power of all robots you control")

    parse("Deal damage to a robot equal to the total power of robots you control") shouldEqual
      parse("Deal damage to a robot equal to the total power of all robots you control")
  }

  it should "parse keyword abilities" in {
    // Defender
    parse("This robot can't attack") shouldEqual
      ApplyEffect(ThisRobot, CannotAttack)

    // Haste
    parse("This robot can move and attack immediately after it is played") shouldEqual
      TriggeredAbility(AfterPlayed(ItO), CanMoveAgain(ThisRobot))

    // Jump
    parse("This robot can move over other objects") shouldEqual
      ApplyEffect(ThisRobot, CanMoveOverObjects)

    // Taunt
    parse("Your opponent's adjacent robots can only attack this object") shouldEqual
      ApplyEffect(All(ObjectsMatchingConditions(Robot, List(AdjacentTo(ThisRobot), ControlledBy(Opponent)))), CanOnlyAttack(ThisRobot))
  }

  it should "parse triggers for robots" in {
    // (The following trigger texts were provided by James:)
    parse("At the end of each turn, each robot takes 1 damage") shouldEqual
      TriggeredAbility(EndOfTurn(AllPlayers), DealDamage(All(ObjectsInPlay(Robot)), Scalar(1)))
    parse("This robot gains a second move action after attacking") shouldEqual
      TriggeredAbility(AfterAttack(ThisRobot), CanMoveAgain(ThisRobot))
    parse("At the beginning of each of your turns, this robot gains 1 attack") shouldEqual
      TriggeredAbility(BeginningOfTurn(Self), ModifyAttribute(ThisRobot, Attack, Plus(Scalar(1))))
    parse("When this robot attacks, it deals damage to all adjacent robots") shouldEqual
      TriggeredAbility(AfterAttack(ThisRobot), DealDamage(All(ObjectsMatchingConditions(Robot, Seq(AdjacentTo(ThisRobot)))), AttributeValue(ThisRobot, Attack)))
    parse("When this robot is played, reduce the cost of a card in your hand by 3") shouldEqual
      TriggeredAbility(AfterPlayed(ThisRobot), ModifyAttribute(Choose(CardsInHand(Self)), Cost, Minus(Scalar(3))))
    parse("Whenever this robot takes damage, draw a card") shouldEqual
      TriggeredAbility(AfterDamageReceived(ThisRobot), Draw(Self, Scalar(1)))
    parse("When this robot is played, reduce the cost of a card in your hand by 2") shouldEqual
      TriggeredAbility(AfterPlayed(ThisRobot), ModifyAttribute(Choose(CardsInHand(Self, AnyCard)), Cost, Minus(Scalar(2))))
    parse("Whenever a robot is destroyed in combat, deal 1 damage to its controller.") shouldEqual
      TriggeredAbility(AfterDestroyed(All(ObjectsInPlay(Robot)), Combat), DealDamage(ControllerOf(ItO), Scalar(1)))
    parse("When this robot is destroyed, take control of all adjacent robots.") shouldEqual
      TriggeredAbility(AfterDestroyed(ThisRobot), TakeControl(Self, All(ObjectsMatchingConditions(Robot, Seq(AdjacentTo(ThisRobot))))))
    parse("When this structure comes into play, draw a card for each adjacent robot or structure") shouldEqual
      TriggeredAbility(AfterPlayed(ThisRobot), Draw(Self, Count(ObjectsMatchingConditions(MultipleObjectTypes(Seq(Robot, Structure)), Seq(AdjacentTo(ThisRobot))))))

    // (From 4/10/17 playtest session:)
    parse("At the end of each turn, destroy all of your opponent's adjacent robots") shouldEqual
      TriggeredAbility(EndOfTurn(AllPlayers), Destroy(All(ObjectsMatchingConditions(Robot, List(AdjacentTo(ThisRobot), ControlledBy(Opponent))))))
    parse("When this robot comes into play, discard 2 random cards") shouldEqual
      TriggeredAbility(AfterPlayed(ThisRobot), Discard(Random(Scalar(2), CardsInHand(Self))))

    parse("At the start of each player's turn, that player gains 1 energy if they control an adjacent creature") shouldEqual
      TriggeredAbility(BeginningOfTurn(AllPlayers), If(CollectionExists(ObjectsMatchingConditions(Robot, List(AdjacentTo(ThisRobot), ControlledBy(ItP)))), ModifyEnergy(ItP, Plus(Scalar(1)))))
    parse("Whenever this robot attacks a kernel, draw a card") shouldEqual
      TriggeredAbility(AfterAttack(ThisRobot, Kernel), Draw(Self, Scalar(1)))
  }

  it should "understand that terms like 'a robot' suggest choosing a target in action text but NOT in trigger text" in {
    parse("Destroy a robot") shouldEqual
      Destroy(Choose(ObjectsInPlay(Robot)))

    parse("When a robot attacks, draw a card") shouldEqual
      TriggeredAbility(AfterAttack(All(ObjectsInPlay(Robot))), Draw(Self, Scalar(1)))
  }

  it should "parse passive abilities for robots" in {
    // (The following ability texts were provided by James:)
    parse("Your adjacent robots have +1 attack") shouldEqual
      AttributeAdjustment(All(ObjectsMatchingConditions(Robot, Seq(AdjacentTo(ThisRobot), ControlledBy(Self)))), Attack, Plus(Scalar(1)))
    parse("This robot can't attack") shouldEqual
      ApplyEffect(ThisRobot, CannotAttack)
    parse("This robot's stats can't be changed") shouldEqual
      FreezeAttribute(ThisRobot, AllAttributes)
    parse("Robots you play cost 2 less") shouldEqual
      AttributeAdjustment(All(CardsInHand(Self, Robot)), Cost, Minus(Scalar(2)))

    // (From 4/10/17 playtest session:)
    parse("All cards in your hand cost 1 less energy") shouldEqual
      AttributeAdjustment(All(CardsInHand(Self, AnyCard)), Cost, Minus(Scalar(1)))
    parse("All robots in your hand cost 1") shouldEqual
      AttributeAdjustment(All(CardsInHand(Self, Robot)), Cost, Constant(Scalar(1)))

    parse("All of your robots have \"Activate: Draw a card\"") shouldEqual
      GiveAbility(All(ObjectsMatchingConditions(Robot, Seq(ControlledBy(Self)))), ActivatedAbility(Draw(Self, Scalar(1))))
  }

  it should "parse activated abilities for robots" in {
    parse("Activate: Destroy this robot") shouldEqual
      ActivatedAbility(Destroy(ThisRobot))
  }

  it should "generate JS code for actions" in {
    generateJS("Draw a card") should be ("(function () { actions['draw'](targets['self'](), 1); })")
    generateJS("Destroy a robot") should be ("(function () { actions['destroy'](targets['choose'](objectsInPlay('robot'))); })")
    generateJS("Gain 2 energy") should be ("(function () { actions['modifyEnergy'](targets['self'](), function (x) { return x + 2; }); })")
    generateJS("Give a robot +1 speed") should be ("(function () { actions['modifyAttribute'](targets['choose'](objectsInPlay('robot')), 'speed', function (x) { return x + 1; }); })")
  }

  it should "disallow choosing targets inside a triggered action, *except* for AfterPlayed triggers" in {
    parse("When this robot is destroyed, destroy a robot.") shouldEqual
      Failure(ValidationError("Choosing targets not allowed for triggered actions."))

    parse("When this robot is played, destroy a robot.") should not equal
      Failure(ValidationError("Choosing targets not allowed for triggered actions."))

  }
}
// scalastyle:on line.size.limit
