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
    parse("Destroy a robot") should equal (Destroy(ChooseO(ObjectsInPlay(Robot))))
    parse("Destroy a random robot") should equal (Destroy(RandomO(Scalar(1), ObjectsInPlay(Robot))))
    parse("Discard a robot card") shouldEqual Discard(ChooseC(CardsInHand(Self, Robot)))
    parse("Discard a random card") should equal (Discard(RandomC(Scalar(1), CardsInHand(Self, AnyCard))))
    parse("Gain two energy") should equal (ModifyEnergy(Self, Plus(Scalar(2))))
    parse("Deal 2 damage to a robot") should equal (DealDamage(ChooseO(ObjectsInPlay(Robot)), Scalar(2)))
    parse("Deal 2 damage to yourself") should equal (DealDamage(Self, Scalar(2)))
    parse("Give a robot +1 speed") should equal (ModifyAttribute(ChooseO(ObjectsInPlay(Robot)), Speed, Plus(Scalar(1))))

    // (From 4/10/17 playtest session:)
    parse("Give a robot 0 attack") shouldEqual SetAttribute(ChooseO(ObjectsInPlay(Robot)), Attack, Scalar(0))
    parse("Set a robot's attack to 0") shouldEqual SetAttribute(ChooseO(ObjectsInPlay(Robot)), Attack, Scalar(0))

    parse("Your opponent draws a card") should equal (Draw(Opponent, Scalar(1)))
    parse("Your opponent discards a random card") should equal (Discard(RandomC(Scalar(1), CardsInHand(Opponent, AnyCard))))
    parse("Lose 1 life") should equal (DealDamage(Self, Scalar(1)))
    parse("A random robot loses 2 health") should equal (ModifyAttribute(RandomO(Scalar(1), ObjectsInPlay(Robot)), Health, Minus(Scalar(2))))
    parse("Halve the life of all robots") should equal (ModifyAttribute(ObjectsInPlay(Robot), Health, Divide(Scalar(2), RoundedDown)))
  }

  it should "parse more complex actions" in {
    parse("Deal 2 damage to a robot that has 3 or less speed") shouldEqual
      DealDamage(ChooseO(ObjectsMatchingConditions(Robot, Seq(AttributeComparison(Speed, LessThanOrEqualTo(Scalar(3)))))), Scalar(2))
    parse ("Deal 1 damage to all robots adjacent to a tile") shouldEqual
      DealDamage(ObjectsMatchingConditions(Robot, Seq(AdjacentTo(ChooseO(AllTiles)))), Scalar(1))
    parse("Gain life equal to its health") shouldEqual
      ModifyAttribute(ObjectsMatchingConditions(Kernel, Seq(ControlledBy(Self))), Health, Plus(AttributeValue(ItO, Health)))
    parse("Give all robots you control +2 attack") shouldEqual
      ModifyAttribute(ObjectsMatchingConditions(Robot, Seq(ControlledBy(Self))), Attack, Plus(Scalar(2)))

    // (The following action texts were provided by James:)
    parse("Set all stats of all robots in play to 3") shouldEqual
      SetAttribute(ObjectsInPlay(Robot), AllAttributes, Scalar(3))
    parse("Draw cards equal to the number of robots you control") shouldEqual
      Draw(Self, Count(ObjectsMatchingConditions(Robot, Seq(ControlledBy(Self)))))
    parse("Deal damage to a robot equal to the total power of robots you control") shouldEqual
      DealDamage(ChooseO(ObjectsInPlay(Robot)), AttributeSum(ObjectsMatchingConditions(Robot, Seq(ControlledBy(Self))), Attack))
    parse("Double the attack of all robots in play") shouldEqual
      ModifyAttribute(ObjectsInPlay(Robot), Attack, Multiply(Scalar(2)))
    parse("Double the attack and halve the life (rounded up) of all robots in play") shouldEqual
      MultipleActions(Seq(
        SaveTarget(ObjectsInPlay(Robot)),
        ModifyAttribute(SavedTargetObject, Attack, Multiply(Scalar(2))),
        ModifyAttribute(SavedTargetObject, Health, Divide(Scalar(2), RoundedUp))
      ))
    parse("Destroy all robots with energy cost three or greater") shouldEqual
      Destroy(ObjectsMatchingConditions(Robot, Seq(AttributeComparison(Cost, GreaterThanOrEqualTo(Scalar(3))))))

    // (From 4/10/17 playtest session:)
    parse("Destroy a robot with 4 attack or more") shouldEqual
      Destroy(ChooseO(ObjectsMatchingConditions(Robot, Seq(AttributeComparison(Attack, GreaterThanOrEqualTo(Scalar(4)))))))
    parse("Double the health of a robot") shouldEqual
      ModifyAttribute(ChooseO(ObjectsInPlay(Robot)), Health, Multiply(Scalar(2)))

    parse("Give a robot +1 attack and +1 health") shouldEqual
      MultipleActions(Seq(
        SaveTarget(ChooseO(ObjectsInPlay(Robot))),
        ModifyAttribute(SavedTargetObject, Attack, Plus(Scalar(1))),
        ModifyAttribute(SavedTargetObject, Health, Plus(Scalar(1)))
    ))
    parse("Give a robot -1 speed and -1 attack and +1 health") shouldEqual
      MultipleActions(Seq(
        SaveTarget(ChooseO(ObjectsInPlay(Robot))),
        ModifyAttribute(SavedTargetObject, Speed, Minus(Scalar(1))),
        ModifyAttribute(SavedTargetObject, Attack, Minus(Scalar(1))),
        ModifyAttribute(SavedTargetObject, Health, Plus(Scalar(1)))
      ))
    parse("Reduce the cost of robot cards in your hand by 1") shouldEqual
      ModifyAttribute(AllC(CardsInHand(Self, Robot)), Cost, Minus(Scalar(1)))
    parse("Destroy all robots with cost equal to this structure's health") shouldEqual
      Destroy(ObjectsMatchingConditions(Robot, Seq(AttributeComparison(Cost, EqualTo(AttributeValue(ThisObject, Health))))))

    // New terms for alpha v0.4:
    parse("Draw 3 cards, then immediately end your turn") shouldEqual
      MultipleActions(Seq(Draw(Self, Scalar(3)), EndTurn))
    parse("Deal 1 damage to each robot that attacked last turn") shouldEqual
      DealDamage(ObjectsMatchingConditions(Robot, Seq(HasProperty(AttackedLastTurn))), Scalar(1))
    parse("Destroy a damaged robot") shouldEqual
      Destroy(ChooseO(ObjectsMatchingConditions(Robot, Seq(HasProperty(IsDamaged)))))
    parse("Draw cards equal to your energy") shouldEqual
      Draw(Self, EnergyAmount(Self))
    parse("Give a robot \"Activate: Deal 1 damage to a random robot\"") shouldEqual
      GiveAbility(ChooseO(ObjectsInPlay(Robot)), parse("Activate: Deal 1 damage to a random robot").asInstanceOf[ActivatedAbility])
    parse("Give a robot +1 speed and \"This robot can move over other objects\"") shouldEqual // ("Give a robot +1 speed and Jump")
      MultipleActions(Seq(
        SaveTarget(ChooseO(ObjectsInPlay(Robot))),
        ModifyAttribute(SavedTargetObject, Speed, Plus(Scalar(1))),
        GiveAbility(SavedTargetObject, ApplyEffect(ThisObject, CanMoveOverObjects))
      ))
    parse("Swap the health and attack of all robots in play") shouldEqual
      SwapAttributes(ObjectsInPlay(Robot), Health, Attack)
    parse("Set the attack and speed of all robots in play to 0") shouldEqual
      MultipleActions(Seq(
        SaveTarget(ObjectsInPlay(Robot)),
        SetAttribute(SavedTargetObject, Attack, Scalar(0)),
        SetAttribute(SavedTargetObject, Speed, Scalar(0))
      ))
    parse("Restore 1 health to all adjacent friendly robots") shouldEqual
      RestoreAttribute(ObjectsMatchingConditions(Robot, Seq(AdjacentTo(ThisObject), ControlledBy(Self))), Health, Some(Scalar(1)))

    parse("If that robot is destroyed, deal 1 damage to all robots.") shouldEqual
      If(TargetHasProperty(That, IsDestroyed), DealDamage(ObjectsMatchingConditions(Robot, Seq()), Scalar(1)))
    parse("Each of your robots gets +2 attack until end of turn") shouldEqual
      Until(TurnsPassed(1), ModifyAttribute(ObjectsMatchingConditions(Robot, Seq(ControlledBy(Self))), Attack, Plus(Scalar(2))))

    // New terms for alpha v0.8:
    parse("Move a robot up to 2 spaces") shouldEqual
      MultipleActions(Seq(
        SaveTarget(ChooseO(ObjectsInPlay(Robot))),
        MoveObject(SavedTargetObject, ChooseO(TilesMatchingConditions(Seq(WithinDistanceOf(Scalar(2), SavedTargetObject)))))
      ))
    parse("Remove all abilities from all robots") shouldEqual
      RemoveAllAbilites(ObjectsInPlay(Robot))
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
      ApplyEffect(ThisObject, CannotAttack)

    // Haste
    parse("This robot can move and attack immediately after it is played") shouldEqual
      TriggeredAbility(AfterPlayed(ItO), CanMoveAndAttackAgain(ThisObject))

    // Jump
    parse("This robot can move over other objects") shouldEqual
      ApplyEffect(ThisObject, CanMoveOverObjects)

    // Taunt
    parse("Your opponent's adjacent robots can only attack this object") shouldEqual
      ApplyEffect(ObjectsMatchingConditions(Robot, Seq(AdjacentTo(ThisObject), ControlledBy(Opponent))), CanOnlyAttack(ThisObject))
  }

  it should "parse triggers for robots" in {
    // (The following trigger texts were provided by James:)
    parse("At the end of each turn, each robot takes 1 damage") shouldEqual
      TriggeredAbility(EndOfTurn(AllPlayers), DealDamage(ObjectsInPlay(Robot), Scalar(1)))
    parse("This robot gains a second move action after attacking") shouldEqual
      TriggeredAbility(AfterAttack(ThisObject), CanMoveAgain(ThisObject))
    parse("At the beginning of each of your turns, this robot gains 1 attack") shouldEqual
      TriggeredAbility(BeginningOfTurn(Self), ModifyAttribute(ThisObject, Attack, Plus(Scalar(1))))
    parse("When this robot attacks, it deals damage to all adjacent robots") shouldEqual
      TriggeredAbility(AfterAttack(ThisObject), DealDamage(ObjectsMatchingConditions(Robot, Seq(AdjacentTo(ThisObject))), AttributeValue(ThisObject, Attack)))
    parse("When this robot is played, reduce the cost of a card in your hand by 3") shouldEqual
      TriggeredAbility(AfterPlayed(ThisObject), ModifyAttribute(ChooseC(CardsInHand(Self)), Cost, Minus(Scalar(3))))
    parse("Whenever this robot takes damage, draw a card") shouldEqual
      TriggeredAbility(AfterDamageReceived(ThisObject), Draw(Self, Scalar(1)))
    parse("When this robot is played, reduce the cost of a card in your hand by 2") shouldEqual
      TriggeredAbility(AfterPlayed(ThisObject), ModifyAttribute(ChooseC(CardsInHand(Self, AnyCard)), Cost, Minus(Scalar(2))))
    parse("Whenever a robot is destroyed in combat, deal 1 damage to its controller.") shouldEqual
      TriggeredAbility(AfterDestroyed(AllO(ObjectsInPlay(Robot)), Combat), DealDamage(ControllerOf(ItO), Scalar(1)))
    parse("When this robot is destroyed, take control of all adjacent robots.") shouldEqual
      TriggeredAbility(AfterDestroyed(ThisObject), TakeControl(Self, ObjectsMatchingConditions(Robot, Seq(AdjacentTo(ThisObject)))))
    parse("When this structure comes into play, draw a card for each adjacent robot or structure") shouldEqual
      TriggeredAbility(AfterPlayed(ThisObject), Draw(Self, Count(ObjectsMatchingConditions(MultipleObjectTypes(Seq(Robot, Structure)), Seq(AdjacentTo(ThisObject))))))

    // (From 4/10/17 playtest session:)
    parse("At the end of each turn, destroy all of your opponent's adjacent robots") shouldEqual
      TriggeredAbility(EndOfTurn(AllPlayers), Destroy(ObjectsMatchingConditions(Robot, List(AdjacentTo(ThisObject), ControlledBy(Opponent)))))
    parse("When this robot comes into play, discard 2 random cards") shouldEqual
      TriggeredAbility(AfterPlayed(ThisObject), Discard(RandomC(Scalar(2), CardsInHand(Self))))
    parse("When this robot is played, destroy all other robots") shouldEqual
      TriggeredAbility(AfterPlayed(ThisObject), Destroy(Other(ObjectsInPlay(Robot))))
    parse("Whenever you play an event, draw a card") shouldEqual
      TriggeredAbility(AfterCardPlay(Self, Event), Draw(Self, Scalar(1)))

    parse("At the start of each player's turn, that player gains 1 energy if they control an adjacent robot") shouldEqual
      TriggeredAbility(BeginningOfTurn(AllPlayers), If(CollectionExists(ObjectsMatchingConditions(Robot, List(AdjacentTo(ThisObject), ControlledBy(ItP)))), ModifyEnergy(ItP, Plus(Scalar(1)))))
    parse("Whenever this robot attacks a kernel, draw a card") shouldEqual
      TriggeredAbility(AfterAttack(ThisObject, Kernel), Draw(Self, Scalar(1)))
    parse("When this robot is played, destroy all robots and gain 2 life") shouldEqual
      TriggeredAbility(AfterPlayed(ThisObject), And(Destroy(ObjectsInPlay(Robot)), ModifyAttribute(ObjectsMatchingConditions(Kernel, Seq(ControlledBy(Self))), Health, Plus(Scalar(2)))))
    parse("When this robot is played, all of your other robots can move again") shouldEqual
      TriggeredAbility(AfterPlayed(ThisObject), CanMoveAgain(Other(ObjectsMatchingConditions(Robot, Seq(ControlledBy(Self))))))

    // New terms for alpha v0.4:
    parse("Whenever this robot moves, it takes 1 damage") shouldEqual
      TriggeredAbility(AfterMove(ThisObject), DealDamage(ItO, Scalar(1)))
    parse("Whenever an enemy robot moves, gain 1 life") shouldEqual
      TriggeredAbility(AfterMove(AllO(ObjectsMatchingConditions(Robot, Seq(ControlledBy(Opponent))))), ModifyAttribute(ObjectsMatchingConditions(Kernel, Seq(ControlledBy(Self))), Health, Plus(Scalar(1))))
    parse("When this robot is played, swap all robots' health and attack.") shouldEqual
      TriggeredAbility(AfterPlayed(ThisObject), SwapAttributes(ObjectsInPlay(Robot), Health, Attack))
    parse("When this robot is destroyed, deal 2 damage to all objects within 2 spaces.") shouldEqual
      TriggeredAbility(AfterDestroyed(ThisObject), DealDamage(ObjectsMatchingConditions(AllObjects, Seq(WithinDistanceOf(Scalar(2), ThisObject))), Scalar(2)))

    parse("When this robot is played, if it is adjacent to an enemy robot, it gains 5 health.") shouldEqual
      TriggeredAbility(AfterPlayed(ThisObject), If(CollectionExists(ObjectsMatchingConditions(Robot, List(ControlledBy(Opponent), AdjacentTo(ItO)))), ModifyAttribute(ItO, Health, Plus(Scalar(5)))))
    parse("When this robot attacks, it can attack again.") shouldEqual
      TriggeredAbility(AfterAttack(ThisObject), CanAttackAgain(ItO))
    parse("When this robot attacks a robot, destroy that robot instead") shouldEqual
      TriggeredAbility(AfterAttack(ThisObject, Robot), Instead(Destroy(That)))

    // New terms for alpha v0.8:
    parse("Whenever you play a robot, this structure becomes a copy of it") shouldEqual
      TriggeredAbility(AfterCardPlay(Self, Robot), BecomeACopy(ThisObject, ItO))
  }

  it should "understand that terms like 'a robot' suggest choosing a target in action text but NOT in trigger text" in {
    parse("Destroy a robot") shouldEqual
      Destroy(ChooseO(ObjectsInPlay(Robot)))

    parse("When a robot attacks, draw a card") shouldEqual
      TriggeredAbility(AfterAttack(AllO(ObjectsInPlay(Robot))), Draw(Self, Scalar(1)))
  }

  it should "parse passive abilities for robots" in {
    // (The following ability texts were provided by James:)
    parse("Your adjacent robots have +1 attack") shouldEqual
      AttributeAdjustment(ObjectsMatchingConditions(Robot, Seq(AdjacentTo(ThisObject), ControlledBy(Self))), Attack, Plus(Scalar(1)))
    parse("This robot can't attack") shouldEqual
      ApplyEffect(ThisObject, CannotAttack)
    parse("This robot's stats can't be changed") shouldEqual
      Failure(ValidationError("FreezeAttribute is not implemented yet."))
      // FreezeAttribute(ThisObject, AllAttributes)
    parse("Robots you play cost 2 less") shouldEqual
      AttributeAdjustment(AllC(CardsInHand(Self, Robot)), Cost, Minus(Scalar(2)))

    // (From 4/10/17 playtest session:)
    parse("All cards in your hand cost 1 less energy") shouldEqual
      AttributeAdjustment(AllC(CardsInHand(Self, AnyCard)), Cost, Minus(Scalar(1)))
    parse("All robots in your hand cost 1") shouldEqual
      AttributeAdjustment(AllC(CardsInHand(Self, Robot)), Cost, Constant(Scalar(1)))

    parse("All of your robots have \"Activate: Draw a card\"") shouldEqual
      HasAbility(ObjectsMatchingConditions(Robot, Seq(ControlledBy(Self))), ActivatedAbility(Draw(Self, Scalar(1))))

    // New terms for alpha v0.4:
    parse("All friendly robots within 2 spaces have +1 speed") shouldEqual
      AttributeAdjustment(ObjectsMatchingConditions(Robot, Seq(ControlledBy(Self), WithinDistanceOf(Scalar(2), ThisObject))), Speed, Plus(Scalar(1)))
    parse("This robot only deals damage when attacking.") shouldEqual
      ApplyEffect(ThisObject, CannotFightBack)
    parse("Adjacent robots' attributes can't be changed.") shouldEqual
      Failure(ValidationError("FreezeAttribute is not implemented yet."))
      // FreezeAttribute(ObjectsMatchingConditions(Robot, Seq(AdjacentTo(ThisObject))), AllAttributes)
    parse("All events cost 1 energy") shouldEqual
      AttributeAdjustment(AllC(CardsInHand(AllPlayers, Event)), Cost, Constant(Scalar(1)))
  }

  it should "parse activated abilities for robots" in {
    parse("Activate: Destroy this robot") shouldEqual
      ActivatedAbility(Destroy(ThisObject))
    parse("Activate: Draw a card, then discard a card") shouldEqual
      ActivatedAbility(And(Draw(Self, Scalar(1)), Discard(ChooseC(CardsInHand(Self, AnyCard)))))

    // New terms for alpha v0.4:
    parse("Activate: Restore an adjacent object's health.") shouldEqual
      ActivatedAbility(RestoreAttribute(ChooseO(ObjectsMatchingConditions(AllObjects, Seq(AdjacentTo(ThisObject)))), Health))
  }

  it should "generate JS code for actions" in {
    generateJS("Draw a card") should be ("(function () { actions['draw'](targets['self'](), 1); })")
    generateJS("Destroy a robot") should be ("(function () { actions['destroy'](targets['choose'](objectsMatchingConditions('robot', []))); })")
    generateJS("Gain 2 energy") should be ("(function () { actions['modifyEnergy'](targets['self'](), function (x) { return x + 2; }); })")
    generateJS("Give a robot +1 speed") should be ("(function () { actions['modifyAttribute'](targets['choose'](objectsMatchingConditions('robot', [])), 'speed', function (x) { return x + 1; }); })")
  }

  it should "disallow choosing targets inside a triggered action, *except* for AfterPlayed triggers" in {
    parse("When this robot is destroyed, destroy a robot.") shouldEqual
      Failure(ValidationError("Choosing targets not allowed for triggered actions."))

    parse("When this robot is played, destroy a robot.") should not equal
      Failure(ValidationError("Choosing targets not allowed for triggered actions."))
  }
}
// scalastyle:on line.size.limit
