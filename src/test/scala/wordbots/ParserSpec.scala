package wordbots

import com.workday.montague.semantics.{ Form, Nonsense }
import org.mozilla.javascript.EvaluatorException
import org.scalatest._

import scala.util.{ Failure, Success }

// scalastyle:off line.size.limit
class ParserSpec extends FlatSpec with Matchers {
  import Semantics._

  //scalastyle:off regex
  def parse(input: String, validationMode: ValidationMode = ValidateUnknownCard): Any = {
    println(s"Parsing $input...")
    ErrorAnalyzer.bestValidParse(Parser.parse(input)) match {
      case Some(parse) => parse.semantic match {
        case Form(v: AstNode) =>
          println(s"    $v")
          CodeGenerator.generateJS(v.asInstanceOf[AstNode]).get  // Make sure that valid JS can be generated!
          AstValidator(validationMode).validate(v) match {  // Make sure the AstValidator successfully validates the parsed ast!
            case Success(_) => v
            case f: Failure[_] => f
          }
        case _ => Nonsense
      }
      case _ => Nonsense
    }
  }
  //scalastyle:on regex

  def generateJS(input: String): String = {
    CodeGenerator.generateJS(parse(input).asInstanceOf[AstNode]).get
  }

  def attrs(attack: Int, health: Int, speed: Int): Seq[AttributeAmount] = {
    Seq(AttributeAmount(Scalar(attack), Attack), AttributeAmount(Scalar(health), Health), AttributeAmount(Scalar(speed), Speed))
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
    parse("Set a robot's attack to 0") shouldEqual SetAttribute(ChooseO(ObjectsInPlay(Robot)), Attack, Scalar(0))

    parse("Your opponent draws a card") should equal (Draw(Opponent, Scalar(1)))
    parse("Your opponent discards a random card") should equal (Discard(RandomC(Scalar(1), CardsInHand(Opponent, AnyCard))))
    parse("Lose 1 life") should equal (DealDamage(Self, Scalar(1)))
    parse("A random robot loses 2 health") should equal (ModifyAttribute(RandomO(Scalar(1), ObjectsInPlay(Robot)), Health, Minus(Scalar(2))))
    parse("Halve the life of all robots") should equal (ModifyAttribute(ObjectsInPlay(Robot), Health, Divide(Scalar(2), RoundedDown)))

    // alpha v0.13
    parse("Lose 10 energy") shouldEqual ModifyEnergy(Self, Minus(Scalar(10)))
    parse("You lose all energy") shouldEqual ModifyEnergy(Self, Constant(Scalar(0)))
    parse("Your energy becomes 0") shouldEqual ModifyEnergy(Self, Constant(Scalar(0)))
  }

  it should "parse simple conditions" in {
    parse("Deal 2 damage to a robot that has 3 or less speed") shouldEqual
      DealDamage(ChooseO(ObjectsMatchingConditions(Robot, Seq(AttributeComparison(Speed, LessThanOrEqualTo(Scalar(3)))))), Scalar(2))
    parse ("Deal 1 damage to all robots adjacent to a tile") shouldEqual
      DealDamage(ObjectsMatchingConditions(Robot, Seq(AdjacentTo(ChooseT(AllTiles)))), Scalar(1))
    parse("Give all robots you control +2 attack") shouldEqual
      ModifyAttribute(ObjectsMatchingConditions(Robot, Seq(ControlledBy(Self))), Attack, Plus(Scalar(2)))
    //scalastyle:off magic.number
    parse("Destroy a robot with 4 attack or more") shouldEqual
      Destroy(ChooseO(ObjectsMatchingConditions(Robot, Seq(AttributeComparison(Attack, GreaterThanOrEqualTo(Scalar(4)))))))
    //scalastyle:on magic.number
    parse("Destroy all robots with energy cost three or greater") shouldEqual
      Destroy(ObjectsMatchingConditions(Robot, Seq(AttributeComparison(Cost, GreaterThanOrEqualTo(Scalar(3))))))
    parse("Deal 2 damage to a random enemy") shouldEqual
      DealDamage(RandomO(Scalar(1), ObjectsMatchingConditions(AllObjects, Seq(ControlledBy(Opponent)))), Scalar(2))
  }

  it should "parse multiple actions" in {
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
    parse("All robots gain 1 attack and 1 health") shouldEqual
      MultipleActions(Seq(
        SaveTarget(ObjectsInPlay(Robot)),
        ModifyAttribute(SavedTargetObject, Attack, Plus(Scalar(1))),
        ModifyAttribute(SavedTargetObject, Health, Plus(Scalar(1)))
      ))
    parse("Set the attack and speed of all robots in play to 0") shouldEqual  // as of v0.12, this no longer requires MultipleActions
      SetAttribute(ObjectsMatchingConditions(Robot, Seq()), MultipleAttributes(Seq(Attack, Speed)), Scalar(0))
  }

  it should "parse more complex actions" in {
    parse("Gain life equal to its health") shouldEqual
      ModifyAttribute(ObjectsMatchingConditions(Kernel, Seq(ControlledBy(Self))), Health, Plus(AttributeValue(ItO, Health)))
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
    // (From 4/10/17 playtest session:)
    parse("Double the health of a robot") shouldEqual
      ModifyAttribute(ChooseO(ObjectsInPlay(Robot)), Health, Multiply(Scalar(2)))
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
        MoveObject(SavedTargetObject, ChooseT(TilesMatchingConditions(Seq(WithinDistanceOf(Scalar(2), SavedTargetObject), Unoccupied))))
      ))
    parse("Remove all abilities from all robots") shouldEqual
      RemoveAllAbilities(ObjectsInPlay(Robot))
    parse("Set the attack of all robots equal to their health") shouldEqual
      SetAttribute(ObjectsInPlay(Robot), Attack, AttributeValue(They, Health))
    parse("Give a friendly robot 2 attack") shouldEqual parse("Give a friendly robot +2 attack")
    parse("Return a robot to its owner's hand") shouldEqual
      ReturnToHand(ChooseO(ObjectsInPlay(Robot)))
    parse("Return all structures to their owner's hands") shouldEqual
      ReturnToHand(ObjectsInPlay(Structure))

    // New terms for alpha v0.11:
    parse("Spawn a 1/1/1 robot named \"Test Bot\" adjacent to your kernel") shouldEqual
      SpawnObject(
        GeneratedCard(
          Robot,
          Seq(AttributeAmount(Scalar(1), Attack), AttributeAmount(Scalar(1), Health), AttributeAmount(Scalar(1), Speed)),
          Some("Test Bot")
        ),
        ChooseT(TilesMatchingConditions(Seq(AdjacentTo(ObjectsMatchingConditions(Kernel, Seq(ControlledBy(Self)))))))
      )

    // Alpha v0.12 playtesting:
    parse("All robots get \"When this robot is destroyed, it deals damage equal to its power to the opponent's kernel\"") shouldEqual
      GiveAbility(
        ObjectsMatchingConditions(Robot, Seq()),
        parse("When this robot is destroyed, it deals damage equal to its power to the opponent's kernel").asInstanceOf[Ability]
      )
    parse("Move each robot to a random tile") shouldEqual
      MoveObject(ObjectsMatchingConditions(Robot, Seq()), RandomT(Scalar(1), AllTiles))
    parse("Move all robots to a random adjacent tile") shouldEqual
      MoveObject(ObjectsMatchingConditions(Robot, Seq()), RandomT(Scalar(1), TilesMatchingConditions(Seq(AdjacentTo(They)))))
    /* parse("Move a random robot 1 space") shouldEqual   // see "disallow choosing targets after a random target has been selected" test
      MultipleActions(Seq(
       SaveTarget(RandomO(Scalar(1), ObjectsInPlay(Robot))),
       MoveObject(SavedTargetObject, ChooseO(TilesMatchingConditions(Seq(WithinDistanceOf(Scalar(1), SavedTargetObject), Unoccupied))))
     )) */
    parse("each robot's attack becomes equal to its health") shouldEqual
      SetAttribute(ObjectsMatchingConditions(Robot, Seq()), Attack, AttributeValue(ItO, Health))
    parse("Each robot's attack and speed become equal to its health") shouldEqual
      SetAttribute(ObjectsMatchingConditions(Robot, Seq()), MultipleAttributes(Seq(Attack, Speed)), AttributeValue(ItO, Health))
    parse("Shuffle all events from your discard pile into your deck") shouldEqual
      ShuffleCardsIntoDeck(AllC(CardsInDiscardPile(Self, Event)), Self)
    parse("Return a random robot from your opponent's discard pile to your hand") shouldEqual
      MoveCardsToHand(RandomC(Scalar(1), CardsInDiscardPile(Opponent, Robot)), Self)
    parse("Return a random robot from your discard pile to a random space adjacent to your kernel") shouldEqual
      SpawnObject(
        RandomC(Scalar(1), CardsInDiscardPile(Self, Robot)),
        RandomT(Scalar(1), TilesMatchingConditions(Seq(AdjacentTo(ObjectsMatchingConditions(Kernel, Seq(ControlledBy(Self)))))))
      )
    parse("Deal 3 damage to your kernel for each robot in play") shouldEqual
      ForEach(ObjectsMatchingConditions(Robot, Seq()), DealDamage(ObjectsMatchingConditions(Kernel, Seq(ControlledBy(Self))), Scalar(3)))
    parse("Each player shuffles all cards from their hand into their deck") shouldEqual
      ForEach(AllPlayers, ShuffleCardsIntoDeck(AllC(CardsInHand(TheyP, AnyCard)), TheyP))

    // alpha v0.13
    parse("Deal damage to a robot equal to your maximum energy") shouldEqual
      DealDamage(ChooseO(ObjectsMatchingConditions(Robot, Seq())), MaximumEnergyAmount(Self))
    parse("Play a random robot from your discard pile that costs 3 or less energy on a random tile") shouldEqual
      SpawnObject(
        RandomC(Scalar(1), CardsInDiscardPile(Self, Robot, Seq(AttributeComparison(Cost, LessThanOrEqualTo(Scalar(3)))))),
        RandomT(Scalar(1), AllTiles)
      )
    parse("Your opponent returns a random robot from their discard pile to a random tile") shouldEqual
      SpawnObject(RandomC(Scalar(1), CardsInDiscardPile(TheyP, Robot, Seq())), RandomT(Scalar(1), AllTiles), Opponent)
    parse("Reduce the attack, speed, and health of all robots to 1") shouldEqual
      ModifyAttribute(ObjectsMatchingConditions(Robot, Seq()), MultipleAttributes(Seq(Health, Speed, Attack)), Minus(Scalar(1)))
    parse("Return all robots to your hand") shouldEqual
      ReturnToHand(ObjectsMatchingConditions(Robot, Seq()), Some(Self))
    parse("Give all robots \"Activate: Destroy this robot\"", ValidateEvent) shouldEqual
      GiveAbility(ObjectsInPlay(Robot), ActivatedAbility(Destroy(ThisObject))) // Test that this doesn't fail the NoThis validator
    parse("Move all event cards from your discard pile that cost 2 or more energy to your hand") shouldEqual
      MoveCardsToHand(AllC(CardsInDiscardPile(Self, Event, Seq(AttributeComparison(Cost, GreaterThanOrEqualTo(Scalar(2)))))), Self)
    parse("Move all event cards with more than 2 cost from your discard pile to your hand") shouldEqual
      MoveCardsToHand(AllC(CardsInDiscardPile(Self, Event, Seq(AttributeComparison(Cost, GreaterThan(Scalar(2)))))), Self)
    parse("Move all event cards with >2 cost from your discard pile to your hand") shouldEqual
      MoveCardsToHand(AllC(CardsInDiscardPile(Self, Event, Seq(AttributeComparison(Cost, GreaterThan(Scalar(2)))))), Self)

    // alpha v0.14
    parse("Deal 3 damage to a robot and all adjacent robots") shouldEqual
      DealDamage(
        UnionO(Seq(ChooseO(ObjectsInPlay(Robot)), ObjectsMatchingConditions(Robot, Seq(AdjacentTo(ThisObject))))),
        Scalar(3)
      )

    // alpha v0.16
    parse("Choose a robot") shouldEqual SaveTarget(ChooseO(ObjectsInPlay(Robot)))
    parse("Discard your hand") shouldEqual parse("Discard all cards in your hand")
    parse("Discard your opponent's hand") shouldEqual Nonsense // You can only discard your own cards.
    parse("Deal damage to a robot equal to the energy cost of a card in your hand") shouldEqual
      DealDamage(
        ChooseO(ObjectsInPlay(Robot)),
        AttributeValue(ChooseC(CardsInHand(Self)), Cost)
      )
    parse("Move a robot 3 spaces") shouldEqual
      MultipleActions(Seq(
        SaveTarget(ChooseO(ObjectsInPlay(Robot))),
        MoveObject(SavedTargetObject, ChooseT(TilesMatchingConditions(Seq(ExactDistanceFrom(Scalar(3), SavedTargetObject), Unoccupied))))
      ))
    parse("Swap the positions of two robots") shouldEqual
      SwapPositions(ChooseO(ObjectsInPlay(Robot)), ChooseO(ObjectsInPlay(Robot)))
    parse("Swap the position of your kernel and your opponent's kernel") shouldEqual
      SwapPositions(ObjectsMatchingConditions(Kernel, Seq(ControlledBy(Self))), ObjectsMatchingConditions(Kernel, Seq(ControlledBy(Opponent))))

    // alpha v0.18 - rewrite effects (up to 4 rewrite rules per action)
    parse("Replace \"a robot\" with \"all robots\" on all cards in your hand") shouldEqual
      RewriteText(AllC(CardsInHand(Self)), Map("a robot" -> "all robots"))
    parse("Replace \"robot\" with \"structure\" and \"structure\" with \"robot\" on all cards in your hand") shouldEqual
      RewriteText(AllC(CardsInHand(Self)), Map("robot" -> "structure", "structure" -> "robot"))
    parse("Replace \"1\" with \"2\", \"2\" with \"4\", \"3\" with \"6\", and \"4\" with \"8\" on all cards in your hand") shouldEqual
      RewriteText(AllC(CardsInHand(Self)), Map("1" -> "2", "2" -> "4", "3" -> "6", "4" -> "8"))

    // alpha v0.18
    parse("Swap the positions of 2 robots") shouldEqual parse("Swap the positions of a robot and a robot")
    parse("Swap the positions of a robot with your kernel") shouldEqual parse("Swap the positions of a robot and your kernel")
    // "Replace \"Defender\" with \"Jump\" ..." <- Make sure that there's not an invalid JS error produced due to the apostrophe in the definition of "Defender":
    parse("Replace \"This robot can't attack\" with \"This robot can move over other objects\" on all robots in your hand") shouldNot be(a[Failure[_]])

    // beta v0.20
    parse("Draw cards equal to half your energy") shouldEqual Draw(Self, Half(EnergyAmount(Self), RoundedDown))
    parse("Draw cards equal to half your energy, rounded up") shouldEqual Draw(Self, Half(EnergyAmount(Self), RoundedUp))

    // beta v0.20.1
    parse("If your hand has 1 or fewer cards, draw a card") shouldEqual If(CollectionCountComparison(CardsInHand(Self), LessThanOrEqualTo(Scalar(1))), Draw(Self, Scalar(1)))
    parse("Discard 2 cards") shouldEqual Discard(ChooseC(CardsInHand(Self, AnyCard), Scalar(2)))
    parse("Spawn a 1/1/1 robot named \"Zombie\" on 4 empty tiles") shouldEqual
      SpawnObject(GeneratedCard(Robot, attrs(1, 1, 1), Some("Zombie")), ChooseT(TilesMatchingConditions(List(Unoccupied)), Scalar(4)))
    parse("Deal 3 damage to an enemy robot up to 3 tiles away from your kernel") shouldEqual
      DealDamage(ChooseO(ObjectsMatchingConditions(Robot, List(ControlledBy(Opponent), WithinDistanceOf(Scalar(3), ObjectsMatchingConditions(Kernel, List(ControlledBy(Self))))))), Scalar(3))
    parse("Pay all your energy") shouldEqual PayEnergy(Self, EnergyAmount(Self))
    parse("Spawn a copy of this object on an adjacent tile") shouldEqual SpawnObject(CopyOfC(ThisObject), ChooseT(TilesMatchingConditions(List(AdjacentTo(They))), Scalar(1)), Self)

    // beta v0.20.2
    parse("Deal 1 damage to all objects within 1 space of a tile") shouldEqual DealDamage(ObjectsMatchingConditions(AllObjects, Seq(WithinDistanceOf(Scalar(1), ChooseT(AllTiles)))), Scalar(1))

    // beta v0.20.4
    parse("Draw a card if your kernel moved this turn") shouldEqual If(TargetHasProperty(ObjectsMatchingConditions(Kernel, List(ControlledBy(Self))), MovedThisTurn), Draw(Self, Scalar(1)))
    parse("If your kernel moved this turn, draw a card") shouldEqual If(TargetHasProperty(ObjectsMatchingConditions(Kernel, List(ControlledBy(Self))), MovedThisTurn), Draw(Self, Scalar(1)))
    parse("Deal 2 damage to 2 enemy robots") shouldEqual DealDamage(ChooseO(ObjectsMatchingConditions(Robot, List(ControlledBy(Opponent))), Scalar(2)), Scalar(2))
    parse("Deal 2 damage to 2 of your opponent's robots") shouldEqual DealDamage(ChooseO(ObjectsMatchingConditions(Robot, List(ControlledBy(Opponent))), Scalar(2)), Scalar(2))
    parse("Give 1 health to all robots you control") shouldEqual parse("Give all robots you control 1 health")
    parse("All friendly robots lose 1 speed and 1 health and 1 attack") shouldEqual
      MultipleActions(Seq(
        SaveTarget(ObjectsMatchingConditions(Robot, List(ControlledBy(Self)))),
        ModifyAttribute(SavedTargetObject, Speed, Minus(Scalar(1))),
        ModifyAttribute(SavedTargetObject, Health, Minus(Scalar(1))),
        ModifyAttribute(SavedTargetObject, Attack, Minus(Scalar(1)))
      ))
    parse("Choose a tile") shouldEqual SaveTarget(ChooseT(AllTiles, Scalar(1)))
    parse("Lose life equal to the number of objects destroyed this turn") shouldEqual DealDamage(Self, NumberOfObjectsDestroyedThisTurn)
  }

  it should "treat 'with' as 'that has'" in {
    parse("Deal 1 damage to a robot with 2 health") shouldEqual
      parse("Deal 1 damage to a robot that has 2 health")
    parse("Deal 1 damage to a robot with 2 health and 1 attack") shouldEqual
      parse("Deal 1 damage to a robot that has 2 health and 1 attack")
  }

  it should "select objects satisfying multiple conditions" in {
    parse("Deal 1 damage to a robot with 1 attack and 1 health") shouldEqual
      DealDamage(ChooseO(ObjectsMatchingConditions(Robot,Seq(AttributeComparison(Attack,EqualTo(Scalar(1))),AttributeComparison(Health,EqualTo(Scalar(1)))))),Scalar(1))
    // test mix of ">" and "="
    parse("Deal 1 damage to a robot with greater than 1 attack and 1 health") shouldEqual
      DealDamage(ChooseO(ObjectsMatchingConditions(Robot,Seq(AttributeComparison(Attack,GreaterThan(Scalar(1))),AttributeComparison(Health,EqualTo(Scalar(1)))))),Scalar(1))
    parse("Give a robot with 1 attack and 1 speed and 1 health 1 attack.") shouldEqual
      ModifyAttribute(
        ChooseO(ObjectsMatchingConditions(Robot, Seq(
          AttributeComparison(Attack, EqualTo(Scalar(1))),
          AttributeComparison(Speed, EqualTo(Scalar(1))),
          AttributeComparison(Health, EqualTo(Scalar(1))))
        )),
        Attack,
        Plus(Scalar(1))
      )
  }

  it should "deal with ambiguous uses of 'all'" in {
    parse("Draw cards equal to the total power of robots you control") shouldEqual
      parse("Draw cards equal to the total power of all robots you control")

    parse("Deal damage to a robot equal to the total power of robots you control") shouldEqual
      parse("Deal damage to a robot equal to the total power of all robots you control")
  }

  it should "correctly generate cards" in {
    val generatedRobotCard = GeneratedCard(Robot, Seq(
      AttributeAmount(Scalar(1), Attack),
      AttributeAmount(Scalar(2), Health),
      AttributeAmount(Scalar(1), Speed)
    ))

    parse("A robot becomes a robot with 1 attack, 2 health and 1 speed") shouldEqual
      Become(ChooseO(ObjectsMatchingConditions(Robot, Seq())), generatedRobotCard)

    parse("A robot becomes a 1/2/1 robot") shouldEqual
      Become(ChooseO(ObjectsMatchingConditions(Robot, Seq())), generatedRobotCard)
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

    //scalastyle:off magic.number
    // (two possible parses here, both valid and both achieving the same result)
    parse("When this robot is played, if it is adjacent to an enemy robot, it gains 5 health.") should (
      equal(TriggeredAbility(AfterPlayed(ThisObject), If(CollectionExists(ObjectsMatchingConditions(Robot, List(ControlledBy(Opponent), AdjacentTo(ItO)))), ModifyAttribute(ItO, Health, Plus(Scalar(5))))))
      or equal(TriggeredAbility(AfterPlayed(ThisObject), If(TargetMeetsCondition(ItO, AdjacentTo(ChooseO(ObjectsMatchingConditions(Robot, List(ControlledBy(Opponent))), Scalar(1)))), ModifyAttribute(ItO, Health, Plus(Scalar(5))))))
    )

    //scalastyle:on magic.number
    parse("When this robot attacks, it can attack again.") shouldEqual
      TriggeredAbility(AfterAttack(ThisObject), CanAttackAgain(ItO))
    parse("When this robot attacks a robot, destroy that robot instead") shouldEqual
      TriggeredAbility(AfterAttack(ThisObject, Robot), Instead(Destroy(That)))

    // New terms for alpha v0.8:
    parse("Whenever you play a robot, this structure becomes a copy of it") shouldEqual
      TriggeredAbility(AfterCardPlay(Self, Robot), Become(ThisObject, CopyOfC(ItO)))
    parse("Whenever a card is played, this robot becomes a copy of it") shouldEqual
      TriggeredAbility(AfterCardPlay(AllPlayers, AnyCard), Become(ThisObject, CopyOfC(ItO)))

    // Support for multiple triggered abilities in one sentence.
    parse("Deal 21 damage to the enemy kernel at the end of your turn, and deal 21 damage to your kernel at the end of your turn") shouldEqual
      MultipleAbilities(Seq(
        parse("Deal 21 damage to the enemy kernel at the end of your turn").asInstanceOf[TriggeredAbility],
        parse("deal 21 damage to your kernel at the end of your turn").asInstanceOf[TriggeredAbility]
      ))

    parse("At the start of your turn, if you have a robot on the board with 3 or more health, draw 2 cards.") shouldEqual
      TriggeredAbility(BeginningOfTurn(Self), If(CollectionExists(ObjectsMatchingConditions(Robot, List(AttributeComparison(Health, GreaterThanOrEqualTo(Scalar(3))), ControlledBy(Self)))), Draw(Self, Scalar(2))))

    // Alpha v0.12 playtesting:
    val potatoBot = GeneratedCard(Robot, attrs(1, 1, 1),Some("Potato"))
    parse("When this robot is destroyed, it deals damage equal to its power to your opponent's kernel") shouldEqual
      TriggeredAbility(AfterDestroyed(ThisObject, AnyEvent), DealDamage(ObjectsMatchingConditions(Kernel, Seq(ControlledBy(Opponent))), AttributeValue(ItO, Attack)))
    parse("When this robot is played, spawn a 0/1/0 robot named \"Faygo\" on all tiles adjacent to a random enemy robot") shouldEqual
      TriggeredAbility(
        AfterPlayed(ThisObject),
        SpawnObject(
          GeneratedCard(Robot, attrs(0, 1, 0), Some("Faygo")),
          TilesMatchingConditions(Seq(AdjacentTo(RandomO(Scalar(1), ObjectsMatchingConditions(Robot, Seq(ControlledBy(Opponent)))))))
        )
      )
    parse("At the beginning of your opponent's turn, spawn a copy of this robot on a random tile adjacent to your opponent's kernel") shouldEqual
      TriggeredAbility(
        BeginningOfTurn(Opponent),
        SpawnObject(
          CopyOfC(ThisObject),
          RandomT(Scalar(1), TilesMatchingConditions(List(AdjacentTo(ObjectsMatchingConditions(Kernel, Seq(ControlledBy(Opponent)))))))
        )
      )
    parse("When this robot is played, spawn a 1/1/1 robot named \"Potato\" adjacent to this robot") shouldEqual
      TriggeredAbility(AfterPlayed(ThisObject), SpawnObject(potatoBot, ChooseT(TilesMatchingConditions(Seq(AdjacentTo(ThisObject))))))
    parse("At the beginning of your turn, spawn a 1/1/1 robot named \"Potato\" on a random tile within 2 hexes of this robot") shouldEqual
      TriggeredAbility(BeginningOfTurn(Self), SpawnObject(potatoBot, RandomT(Scalar(1), TilesMatchingConditions(Seq(WithinDistanceOf(Scalar(2), ThisObject))))))
    parse("At the beginning of your turn, spawn a 1/1/1 robot named \"Potato\" on a random tile 2 spaces away from this robot") shouldEqual
      TriggeredAbility(BeginningOfTurn(Self), SpawnObject(potatoBot, RandomT(Scalar(1), TilesMatchingConditions(Seq(ExactDistanceFrom(Scalar(2), ThisObject))))))
    parse("At the end of your turn, your opponent takes control of this robot") shouldEqual
      TriggeredAbility(EndOfTurn(Self), TakeControl(Opponent, ThisObject))
    parse("At the end of your turn, your opponent spawns a 1/1/1 robot named \"Nemesis\" on a random tile adjacent to this robot") shouldEqual
      TriggeredAbility(
        EndOfTurn(Self),
        SpawnObject(
          GeneratedCard(Robot, attrs(1, 1, 1), Some("Nemesis")),
          RandomT(Scalar(1), TilesMatchingConditions(List(AdjacentTo(ThisObject)))),
          Opponent
        )
      )
    parse("Whenever any card enters your discard pile, gain 1 life") shouldEqual
      TriggeredAbility(AfterCardEntersDiscardPile(Self, AnyCard), ModifyAttribute(ObjectsMatchingConditions(Kernel, Seq(ControlledBy(Self))), Health, Plus(Scalar(1))))
    parse("When this robot is played, if your discard pile has five or more cards, this robot gets +3 health") shouldEqual
      TriggeredAbility(AfterPlayed(ThisObject), If(CollectionCountComparison(CardsInDiscardPile(Self, AnyCard), GreaterThanOrEqualTo(Scalar(5))), ModifyAttribute(ThisObject, Health, Plus(Scalar(3)))))
    parse("Whenever this robot destroys an enemy robot, draw a card") shouldEqual
      TriggeredAbility(AfterDestroysOtherObject(ThisObject, Robot),Draw(Self, Scalar(1)))
    parse("Whenever an enemy robot is destroyed by this robot, draw a card") shouldEqual
      TriggeredAbility(AfterDestroysOtherObject(ThisObject, Robot),Draw(Self, Scalar(1)))

    // alpha v0.13
    parse("Whenever a player draws a card, that player discards a random card") shouldEqual
      TriggeredAbility(AfterCardDraw(AllPlayers, AnyCard), Discard(RandomC(Scalar(1), CardsInHand(ItP, AnyCard))))
    parse("Whenever this robot attacks, it gains health equal to its attack") shouldEqual
      TriggeredAbility(AfterAttack(ThisObject, AllObjects), ModifyAttribute(ItO, Health, Plus(AttributeValue(ItO, Attack))))
    parse("Whenever this robot attacks, it gains health equal to the defending robot's attack") shouldEqual
      TriggeredAbility(AfterAttack(ThisObject, AllObjects), ModifyAttribute(ItO, Health, Plus(AttributeValue(That, Attack))))
    parse("At the start of your turn, if there is an adjacent enemy robot, draw a card") shouldEqual
      TriggeredAbility(BeginningOfTurn(Self), If(CollectionExists(ObjectsMatchingConditions(Robot, Seq(AdjacentTo(ThisObject), ControlledBy(Opponent)))), Draw(Self, Scalar(1))))
    parse("At the start of your turn, if an enemy robot is adjacent to this robot, draw a card") shouldEqual
      TriggeredAbility(BeginningOfTurn(Self), If(CollectionExists(ObjectsMatchingConditions(Robot, Seq(AdjacentTo(ThisObject), ControlledBy(Opponent)))), Draw(Self, Scalar(1))))

    // alpha v0.16
    parse("Whenever a robot takes damage, increase its power by 1 instead") shouldEqual
      TriggeredAbility(
        AfterDamageReceived(AllO(ObjectsInPlay(Robot))),
        Instead(ModifyAttribute(ItO, Attack, Plus(Scalar(1))))
      )
    parse("Whenever a robot takes damage, it gains that much attack and speed") shouldEqual
      TriggeredAbility(
        AfterDamageReceived(AllO(ObjectsInPlay(Robot))),
        ModifyAttribute(ItO, MultipleAttributes(List(Attack, Speed)), Plus(ThatMuch))
      )

    // alpha v0.18
    parse("Whenever this kernel takes damage, draw a card") shouldEqual parse("Whenever this object takes damage, draw a card")
    parse("Whenever a robot deals damage to this kernel, that robot takes that much damage") shouldEqual
      TriggeredAbility(AfterDamageReceived(ThisObject, Robot), DealDamage(That, ThatMuch))
    parse("Whenever a robot attacks this structure, draw a card") shouldEqual
      TriggeredAbility(AfterAttackedBy(ThisObject, Robot), Draw(Self, Scalar(1)))
    parse("Whenever a robot attacks this structure, that robot gains \"This robot can move over other objects\" and +1 attack") shouldEqual
      TriggeredAbility(
        AfterAttackedBy(ThisObject, Robot),
        MultipleActions(
          Seq(
            SaveTarget(That),
            ModifyAttribute(SavedTargetObject, Attack, Plus(Scalar(1))),
            GiveAbility(SavedTargetObject, ApplyEffect(ThisObject, CanMoveOverObjects))
          )
        )
      )
    parse("Whenever your opponent draws a card, their kernel takes one damage") shouldEqual
      TriggeredAbility(AfterCardDraw(Opponent, AnyCard), DealDamage(ObjectsMatchingConditions(Kernel, List(ControlledBy(TheyP))), Scalar(1)))
    parse("Whenever a robot is destroyed, spawn a 1/1/1 robot named \"Zombie\" on a random empty tile adjacent to this robot") shouldEqual
      TriggeredAbility(
        AfterDestroyed(AllO(ObjectsInPlay(Robot)), AnyEvent),
        SpawnObject(
          GeneratedCard(Robot, List(AttributeAmount(Scalar(1), Attack), AttributeAmount(Scalar(1), Health), AttributeAmount(Scalar(1), Speed)), Some("Zombie")),
          RandomT(Scalar(1), TilesMatchingConditions(List(AdjacentTo(ThisObject), Unoccupied))), Self
        )
      )
    parse("At the end of your turn, if your opponent has more life than you, draw a card") shouldEqual
      TriggeredAbility(
        EndOfTurn(Self),
        If(
          TargetMeetsCondition(
            ObjectsMatchingConditions(Kernel, List(ControlledBy(Opponent))),
            AttributeComparison(Health, GreaterThan(AttributeValue(ObjectsMatchingConditions(Kernel, List(ControlledBy(Self))), Health)))
          ),
          Draw(Self, Scalar(1))
        )
      )

    // beta v0.20
    parse("When this structure is played, if this structure is not adjacent to your kernel, destroy it") shouldEqual
      TriggeredAbility(
        AfterPlayed(ThisObject),
        If(
          TargetMeetsCondition(ThisObject,NotC(AdjacentTo(ObjectsMatchingConditions(Kernel,List(ControlledBy(Self)))))),
          Destroy(ItO)
        )
      )

    // beta v0.20.1
    parse("When this robot is destroyed, give a random other friendly robot +2 attack") shouldEqual
      TriggeredAbility(
        AfterDestroyed(ThisObject),
        ModifyAttribute(RandomO(Scalar(1), Other(ObjectsMatchingConditions(Robot, List(ControlledBy(Self))))), Attack, Plus(Scalar(2)))
      )
    parse("When this robot is played, this robot loses attack equal to your kernel's health") shouldEqual
      TriggeredAbility(
        AfterPlayed(ThisObject),
        ModifyAttribute(ThisObject, Attack, Minus(AttributeValue(ObjectsMatchingConditions(Kernel, List(ControlledBy(Self))), Health)))
      )
    parse("At the end of your turn, if this robot didn't move, restore 2 health to this robot") shouldEqual
      TriggeredAbility(
        EndOfTurn(Self),
        If(NotGC(TargetHasProperty(ThisObject, MovedThisTurn)), RestoreAttribute(ThisObject, Health, Some(Scalar(2))))
      )
    parse("At the end of your turn, destroy this robot unless it moved this turn") shouldEqual
      TriggeredAbility(
        EndOfTurn(Self),
        If(NotGC(TargetHasProperty(ItO,MovedThisTurn)),Destroy(ThisObject))
      )
    parse("When this robot deals damage, draw a card") shouldEqual TriggeredAbility(AfterDealsDamage(ThisObject), Draw(Self, Scalar(1)))
    parse("Lose 3 energy and return an enemy robot to your hand") shouldEqual
      And(
        ModifyEnergy(Self, Minus(Scalar(3))),
        ReturnToHand(ChooseO(ObjectsMatchingConditions(Robot, List(ControlledBy(Opponent))), Scalar(1)), Some(Self))
      )

    // beta v0.20.4
    parse("At the start of each turn, if this robot is within 2 tiles of the opponent’s kernel, deal 1 damage to the opponent’s kernel") shouldEqual
      TriggeredAbility(
        BeginningOfTurn(AllPlayers),
        If(
          TargetMeetsCondition(ThisObject, WithinDistanceOf(Scalar(2), ObjectsMatchingConditions(Kernel, List(ControlledBy(Opponent))))),
          DealDamage(ObjectsMatchingConditions(Kernel, List(ControlledBy(Opponent))), Scalar(1))
        )
      )
    parse("At the start of each player's turn, that player discards all cards") shouldEqual parse("At the start of each player's turn, that player discards their hand")
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

    // Alpha v0.12 playtesting:
    parse("Your robots have 4 speed") shouldEqual
      AttributeAdjustment(ObjectsMatchingConditions(Robot, Seq(ControlledBy(Self))), Speed, Constant(Scalar(4)))
    parse("Robots your opponent controls have \"This robot can move and attack immediately after it is played\" and +1 speed") shouldEqual  // ("Haste and +1 speed")
      parse("Robots your opponent controls have +1 speed and \"This robot can move and attack immediately after it is played\"")
    parse("This robot gets +1 attack for each robot in all discard piles") shouldEqual
      ModifyAttribute(ThisObject, Attack, Plus(Times(Scalar(1), Count(CardsInDiscardPile(AllPlayers, Robot)))))
    parse("Robots with 1 health can't move") shouldEqual
      ApplyEffect(ObjectsMatchingConditions(Robot, List(AttributeComparison(Health, EqualTo(Scalar(1))))), CannotMove)
    parse("Robots with even power can't attack") shouldEqual
      ApplyEffect(ObjectsMatchingConditions(Robot, Seq(AttributeComparison(Attack, IsEven))),CannotAttack)
    parse("This robot has +4 attack if your discard pile has 5 or more cards") shouldEqual
      AttributeAdjustment(ConditionTargetOn(ThisObject, CollectionCountComparison(CardsInDiscardPile(Self), GreaterThanOrEqualTo(Scalar(5)))), Attack, Plus(Scalar(4)))

    // alpha v0.13
    parse("All robot cards cost 1 less energy to play") shouldEqual
      AttributeAdjustment(AllC(CardsInHand(Self,Robot)), Cost, Minus(Scalar(1)))
    parse("Robots you play cost 1 less for every robot in play") shouldEqual
      AttributeAdjustment(AllC(CardsInHand(Self, Robot, Seq())), Cost, Minus(Times(Scalar(1), Count(ObjectsMatchingConditions(Robot, Seq())))))

    // alpha v0.16:
    parse("Enemy robots can't move adjacent to this structure") shouldEqual
      ApplyEffect(
        ObjectsMatchingConditions(Robot, Seq(ControlledBy(Opponent))),
        CannotMoveTo(TilesMatchingConditions(Seq(AdjacentTo(ThisObject))))
      )
  }

  it should "parse passively triggered actions for robots" in {
    parse("When this object has 20 or more health, you win the game") shouldEqual
      ConditionalAction(
        TargetMeetsCondition(ThisObject, AttributeComparison(Health, GreaterThanOrEqualTo(Scalar(20)))),
        WinGame(Self)
      )
  }

  it should "parse activated abilities for robots" in {
    parse("Activate: Destroy this robot") shouldEqual
      ActivatedAbility(Destroy(ThisObject))
    parse("Activate: Draw a card, then discard a card") shouldEqual
      ActivatedAbility(And(Draw(Self, Scalar(1)), Discard(ChooseC(CardsInHand(Self, AnyCard)))))

    // New terms for alpha v0.4:
    parse("Activate: Restore an adjacent object's health.") shouldEqual
      ActivatedAbility(RestoreAttribute(ChooseO(ObjectsMatchingConditions(AllObjects, Seq(AdjacentTo(ThisObject)))), Health))

    parse("Activate: Deal 1 damage to a robot 3 tiles away") shouldEqual
      ActivatedAbility(DealDamage(ChooseO(ObjectsMatchingConditions(Robot, Seq(ExactDistanceFrom(Scalar(3), ThisObject)))), Scalar(1)))
    parse("Activate: All adjacent robots gain 1 health") shouldEqual  // #109
      ActivatedAbility(ModifyAttribute(ObjectsMatchingConditions(Robot, List(AdjacentTo(ThisObject))), Health, Plus(Scalar(1))))
  }

  it should "parse actions and abilities involving spawning new objects (v0.11)" in {
    parse("Whenever a robot is destroyed, spawn a 2/1/1 robot named \"Zombie Bot\" on a random tile") shouldEqual
      TriggeredAbility(
        AfterDestroyed(AllO(ObjectsMatchingConditions(Robot, Seq())), AnyEvent),
        SpawnObject(
          GeneratedCard(Robot, attrs(2, 1, 1), Some("Zombie Bot")),
          RandomT(Scalar(1), AllTiles)
        )
      )

    parse("Spawn a 1/2/1 robot named \"Reinforcements\" on each tile adjacent to your kernel") shouldEqual
      SpawnObject(
        GeneratedCard(Robot, attrs(1, 2, 1), Some("Reinforcements")),
        TilesMatchingConditions(Seq(AdjacentTo(ObjectsMatchingConditions(Kernel, Seq(ControlledBy(Self))))))
      )

    // beta v0.20.1
    parse("Activate: spawn a 1/1/2 robot named \"Iceado\" on an adjacent empty tile") shouldEqual
      ActivatedAbility(
        SpawnObject(
          GeneratedCard(Robot, attrs(1, 1, 2), Some("Iceado")),
          ChooseT(TilesMatchingConditions(List(AdjacentTo(ThisObject), Unoccupied)), Scalar(1)), Self
        )
      )
  }

  it should "generate JS code for actions" in {
    generateJS("Draw a card") should be ("(function () { actions['draw'](targets['self'](), 1); })")
    generateJS("Destroy a robot") should be ("(function () { actions['destroy'](targets['choose'](objectsMatchingConditions('robot', []), 1)); })")
    generateJS("Gain 2 energy") should be ("(function () { actions['modifyEnergy'](targets['self'](), function (x) { return x + 2; }); })")
    generateJS("Give a robot +1 speed") should be ("(function () { actions['modifyAttribute'](targets['choose'](objectsMatchingConditions('robot', []), 1), 'speed', function (x) { return x + 1; }); })")
  }

  it should "not allow invalid JS code to be returned" in {
    val terribleCardText = "At the beginning of your opponent's turn, spawn a 1/1/1 Robot named \"Annoying Gnat\" with \"At the beginning of your opponent's turn, spawn a 1/1/1 Robot named \"Annoying Gnat\" on a random tile adjacent to your opponent's kernel\" on a random tile adjacent to your opponent's kernel"
    an[EvaluatorException] should be thrownBy {
      generateJS(terribleCardText)
    }
  }

  it should "disallow choosing targets inside a triggered action, *except* for AfterPlayed triggers" in {
    parse("When this robot is destroyed, destroy a robot.") shouldEqual
      Failure(ValidationError("Choosing targets not allowed for triggered actions."))

    parse("When this robot is played, destroy a robot.") should not equal
      Failure(ValidationError("Choosing targets not allowed for triggered actions."))
  }

  it should "disallow choosing targets after a random target has been selected" in {
    parse("Move a random robot 1 space") shouldBe a[Failure[_]]  // Failure[ValidationError]
  }

  it should "disallow paying energy inside an ability" in {
    parse("When this object deals damage, pay 1 energy to draw a card") shouldEqual
      Failure(ValidationError("Paying energy is not supported within triggered abilities."))
  }
}
// scalastyle:on line.size.limit
