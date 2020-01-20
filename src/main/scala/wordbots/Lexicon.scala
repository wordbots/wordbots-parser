package wordbots

import com.workday.montague.ccg._
import com.workday.montague.parser.ParserDict
import com.workday.montague.semantics._
import com.workday.montague.semantics.FunctionReaderMacro.λ
import io.circe.Json

import scala.language.implicitConversions
import scala.language.postfixOps

case class LexiconDefinition(syntax: String, semantics: String)

object Fail {
  def apply(str: String): Unit = throw new ClassCastException(str)
}

/**
  * Created by alex on 2/28/17.
  *
  * CCG syntax reminder:
  *   N = noun, NP = noun phrase, V = verb, S = sentence, PP = propositional phrase
  *   '\X' = needs X before this, '/X' = needs X after this, '|X' = needs X either before or after this
  **/
object Lexicon {
  import Semantics._

  type Sem = SemanticState

  // scalastyle:off method.name
  implicit class StringImplicits(val str: String) extends AnyVal {
    // "blah".s = ["blah", "blahs"]
    def s: Seq[String] = Seq(str, str + "s")

    // "my" / ["thing", "stuff"] = ["my thing", "my stuff"]
    def /(nextWords: Seq[String]): Seq[String] = nextWords.map(s"$str " +)

    // "my" /?/ ["thing", "stuff"] = ["thing", "stuff", "my thing", "my stuff"]
    def /?/(nextWords: Seq[String]): Seq[String] = nextWords ++ nextWords.map(s"$str " +)
  }
  // scalastyle:on method.name

  implicit def astNodeToSem(node: ParseNode): Sem = Form(node)

  val lexicon: ParserDict[CcgCat] = ParserDict[CcgCat]() +
    (Seq("a", "an") -> Seq(
      (N/N, identity),
      (NP/N, λ {o: ObjectType => ChooseO(ObjectsInPlay(o))}),  // e.g. "a robot"
      (NP/NP, λ {c: GeneratedCard => c}),  // e.g. "a 1/1/1 robot"
      (NP/NP, λ {c: ObjectCollection => ChooseO(c)}),  // e.g. "a robot you control"
      (NP/NP, λ {c: CardCollection => ChooseC(c)}),  // e.g. "(discard) a card"
      (Num, Scalar(1): Sem)  // e.g. "(draw) a card"
    )) +
    ("a player" -> (NP, ChooseO(ObjectsInPlay(Kernel)): Sem)) +
    ("a random tile" -> (NP, RandomT(Scalar(1), AllTiles): Sem)) +
    ("a tile" -> (NP, ChooseT(AllTiles): Sem)) +
    ("activate:" -> (S/S, λ {a: Action => ActivatedAbility(a)})) +
    ("adjacent" -> Seq(
      (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(AdjacentTo(ThisObject)))}),
      (NP/NP, λ {c: ObjectsMatchingConditions => ObjectsMatchingConditions(c.objectType, Seq(AdjacentTo(ThisObject)) ++ c.conditions)})
    )) +
    ("adjacent tile" -> (NP, TilesMatchingConditions(Seq(AdjacentTo(They))): Sem)) +  // e.g. "Move each robot to a random adjacent tile."
    ("adjacent to" -> Seq(
      (PP/NP, λ {t: TargetObjectOrTile => AdjacentTo(t)}),
      (PP/NP, λ {t: TargetObject => ChooseT(TilesMatchingConditions(Seq(AdjacentTo(t))))}),
      ((NP/NP)\N, λ {o: ObjectType => λ {t: TargetObjectOrTile => ObjectsMatchingConditions(o, Seq(AdjacentTo(t)))}}),
      (PP/NP, λ {c: ObjectsMatchingConditions => ObjectsMatchingConditions(c.objectType, Seq(AdjacentTo(ThisObject)) ++ c.conditions)})
    )) +
    ("is" /?/ Seq("adjacent to", "adjacent to a", "adjacent to an") -> Seq(
      ((S\N)/NP, λ {o: TargetObject => λ {e: EnemyObject => CollectionExists(ObjectsMatchingConditions(e.objectType, Seq(AdjacentTo(o), ControlledBy(Opponent))))}}),
      ((S\NP)/NP, λ {c: ObjectsMatchingConditions => λ {o: TargetObject => CollectionExists(ObjectsMatchingConditions(c.objectType, c.conditions :+ AdjacentTo(o)))}})
    )) +
    ("after attacking" -> (S\S, λ {a: Action => TriggeredAbility(AfterAttack(ThisObject, AllObjects), a)})) +
    (Seq("all", "each", "every") -> Seq( // Also see Seq("each", "every") below for definitions that DON'T apply to "all".
      (NP/N, λ {o: ObjectType => ObjectsInPlay(o)}),
      (NP/N, λ {c: CardType => AllC(CardsInHand(AllPlayers, c))}),
      (NP/NP, λ {c: CardCollection => AllC(c)}),
      (NP/NP, λ {c: Collection => c}),
      (NP/PP, λ {c: Collection => c})
    )) +
    ("all" /?/ Seq("attributes", "stats") -> (N, AllAttributes: Sem)) +
    ("all energy" -> (NP, AllEnergy: Sem)) +
    (Seq("all players", "each player", "every player", "either player", "a player", "both players") -> (NP, AllPlayers: Sem)) +
    (Seq("all your other", "all of your other", "your other") -> (NP/N, λ {o: ObjectType => Other(ObjectsMatchingConditions(o, Seq(ControlledBy(Self))))})) +
    ("and" -> Seq(
      // Specific.
      ((S/S)\S, λ {a: TriggeredAbility => λ {b: TriggeredAbility => MultipleAbilities(Seq(a, b))}}),
      ((N/N)\N, λ {a1: Attribute => λ {a2: Attribute => MultipleAttributes(Seq(a1, a2))}}),
      (((N\N)\N)/N, λ {a1: Attribute => λ {a2: Attribute => λ {a3: Attribute => MultipleAttributes(Seq(a1, a2, a3))}}}),
      // General.
      (conj, λ {b: ParseNode => λ {a: Seq[ParseNode] => a :+ b}}),
      (ReverseConj, λ {a: ParseNode => λ {b: ParseNode => Seq(a, b)}}),
      (((N\N)\N)/N, λ {c: ParseNode => λ {b: ParseNode => λ {a: ParseNode =>  // e.g. "1 attack, 2 health, and 3 speed"
        if (a.isInstanceOf[Attribute] && b.isInstanceOf[Attribute] && c.isInstanceOf[Attribute]) Fail("") else Seq(a, b, c)}}}  // the conditional Fail() is to force the specific 3-Attribute rule above
      ),
      ((S/S)\NP, λ {a: ParseNode => λ {b: ParseNode => (a, b)}}),  // e.g. "+1 attack and Haste"
      ((S/NP)\S, λ {a: ParseNode => λ {b: ParseNode => (a, b)}})  // e.g. "Haste and +1 attack"
    )) +
    ("any card" -> (N, AnyCard: Sem)) +
    ("at" -> ((S|S)/NP, λ {t: Trigger => λ {a: Action => TriggeredAbility(t, a)}})) +
    (Seq("at most", "up to") -> (Adj/Num, λ {num: Number => LessThanOrEqualTo(num)})) +
    (Seq("attack", "power") -> Seq(
      (N, Attack: Sem),
      (N\Num, λ {i: Scalar => AttributeAmount(i, Attack)}),
      (NP|Adj, λ {amount: Number => AttributeAmount(amount, Attack)}),
      (NP\Adj, λ {op: Operation => AttributeOperation(op, Attack)}),
      (NP\Adj, λ {comp : Comparison => AttributeComparison(Attack, comp)}), // needed for "> x attack"
      (NP\Num, λ {n: Number => AttributeComparison(Attack, EqualTo(n))}) // "...with X attack" (implied "equal to" in there)
    )) +
    ("attacks" -> Seq(
      (S\NP, λ {c: ChooseO => AfterAttack(AllO(c.collection), AllObjects)}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterAttack(t, AllObjects)}),
      ((S\NP)/N, λ {o: ObjectType => λ {t: TargetObject => AfterAttack(t, o)}})
    )) +
    ("attacked last turn" -> (S, HasProperty(AttackedLastTurn): Sem)) +
    ("attacked this turn" -> (S, HasProperty(AttackedThisTurn): Sem)) +
    ("away" -> Seq(
      (PP\NP, λ {s: Spaces => ExactDistanceFrom(s.num, ThisObject)}),  // "X spaces away"
      ((PP/PP)\NP, λ {s: Spaces => λ {t: TargetObject => ExactDistanceFrom(s.num, t)}}),  // "X spaces away from this robot"
      ((NP\N)\NP, λ {s: Spaces => λ {t: ObjectType => ObjectsMatchingConditions(t, Seq(ExactDistanceFrom(s.num, ThisObject)))}}), // "a robot X spaces away"
      ((NP\NP)\NP, λ {s: Spaces => λ {c: ObjectsMatchingConditions => ObjectsMatchingConditions(c.objectType, c.conditions :+ ExactDistanceFrom(s.num, ThisObject))}})
    )) +
    ("become".s -> Seq(
      ((S\NP)/Num, λ {num: Number => λ {e: EnergyAmount => ModifyEnergy(e.player, Constant(num))}}),  // e.g. "Your energy becomes X"
      ((S/Adj)\NP, λ {t: TargetAttribute => λ {num: Number => SetAttribute(t.target, t.attr, num)}})  // e.g. "Each robot's attack becomes X"
    )) +
    (("become".s :+ "becomes a") -> Seq(
      ((S\NP)/NP, λ {target: TargetCard => λ {source: TargetObject => Become(source, target)}}), // used with aCopyOf
      ((S\NP)/NP, λ {target: GeneratedCard => λ {source: TargetObject => Become(source, target)}}) // only used in such things as "becomes a robot with 1 attack and...".
    )) +
    (Seq("beginning", "start") -> (NP/PP, λ {turn: Turn => BeginningOfTurn(turn.player)})) +
    ("by" -> (PP/Num, identity)) +
    (Seq("can move", "can move again", "gains a second move action") -> (S\NP, λ {t: TargetObject => CanMoveAgain(t)})) +
    (Seq("can attack", "can attack again") -> (S\NP, λ {t: TargetObject => CanAttackAgain(t)})) +
    (Seq("can move and attack", "can move and attack again") -> (S\NP, λ {t: TargetObject => CanMoveAndAttackAgain(t)})) +
    ("can move over other objects" -> (S\NP, λ {t: TargetObject => ApplyEffect(t, CanMoveOverObjects)})) +
    ("can only attack" -> ((S\NP)/NP, λ {target: TargetObject => λ {attacker: TargetObject => ApplyEffect(attacker, CanOnlyAttack(target))}})) +
    ("can't activate" -> (S\NP, λ {t: TargetObject => ApplyEffect(t, CannotActivate)})) +
    ("can't attack" -> (S\NP, λ {t: TargetObject => ApplyEffect(t, CannotAttack)})) +
    ("can't defend" -> (S\NP, λ {t: TargetObject => ApplyEffect(t, CannotFightBack)})) +
    ("can't move" -> (S\NP, λ {t: TargetObject => ApplyEffect(t, CannotMove)})) +
    ("can't be changed" -> (S\NP, λ {t: TargetAttribute => FreezeAttribute(t.target, t.attr)})) +
    (("card".s :+ "a card") -> Seq(
      (N, AnyCard: Sem),
      (NP\Num, λ {num: Number => Cards(num)}),
      (NP/Adj, λ {num: Number => Cards(num)}),
      (NP\Adj, λ {comp: Comparison => CardComparison(comp)}),
      (NP, CardsInHand(Self): Sem),
      (NP/PP, λ {hand: Hand => CardsInHand(hand.player)}),
      (NP/PP, λ {d: DiscardPile => CardsInDiscardPile(d.player)}),
      (NP\N, λ {cardType: CardType => CardsInHand(Self, cardType)}),
      ((NP/PP)\N, λ {cardType: CardType => λ {hand: Hand => CardsInHand(hand.player, cardType)}}),
      ((NP/PP)\N, λ {cardType: CardType => λ {d: DiscardPile => CardsInDiscardPile(d.player, cardType)}}),
      ((NP/PP)/Adj, λ {condition: CardCondition => λ {hand: Hand => CardsInHand(hand.player, AnyCard, Seq(condition))}}),
      ((NP/PP)/Adj, λ {condition: CardCondition => λ {d: DiscardPile => CardsInDiscardPile(d.player, AnyCard, Seq(condition))}})
    )) +
    ("control".s -> ((NP\N)\NP, λ {p: TargetPlayer => λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(ControlledBy(p)))}})) +
    (Seq("control a", "control an", "has a", "has an", "have a", "have an") ->
      ((S/NP)\NP,
        λ {p: TargetPlayer => λ {c: ObjectsMatchingConditions => CollectionExists(ObjectsMatchingConditions(c.objectType, c.conditions :+ ControlledBy(p)))}})
    ) +
    ("a copy of" -> (NP/NP, λ {t:TargetObject => CopyOfC(t)})) + // can this be decomposed further?
    (Seq("cost", "energy cost") -> Seq(
      (N, Cost: Sem),
      (NP|Adj, λ {amount: Number => AttributeAmount(amount, Cost)}),
      (NP/Adj, λ {comp : Comparison => AttributeComparison(Cost, comp)}), // needed for "cost > x "
      (NP\Adj, λ {comp : Comparison => AttributeComparison(Cost, comp)}),  // needed for "> x cost"
      (NP/Num, λ {n: Number => AttributeComparison(Cost, EqualTo(n))}), // "...with cost X"(implied "equal to" in there)
      ((S\NP)/Num, λ {i: Scalar => λ {t: TargetObjectOrCard => AttributeAdjustment(t, Cost, Constant(i))}}),
      ((S\NP)/Adv, λ {o: Operation => λ {t: TargetObjectOrCard => AttributeAdjustment(t, Cost, o)}}),
      ((S\NP)/Num, λ {i: Scalar => λ {cp: CardPlay => AttributeAdjustment(AllC(CardsInHand(cp.player, cp.cardType)), Cost, Constant(i))}}),
      ((S\NP)/Adv, λ {o: Operation => λ {cp: CardPlay => AttributeAdjustment(AllC(CardsInHand(cp.player, cp.cardType)), Cost, o)}})
    )) +
    ("damage" -> Seq(
      ((S/PP)\Num, λ {amount: Number => λ {t: TargetObjectOrPlayer => DealDamage(t, amount)}}),
      ((S\NP)\Num, λ {amount: Number => λ {t: TargetObjectOrPlayer => DealDamage(t, amount)}}),
      ((S/Adj)/PP, λ {t: TargetObjectOrPlayer => λ {amount: Number => DealDamage(t, amount)}}),
      ((S\NP)/Adj, λ {amount: Number => λ {t: TargetObjectOrPlayer => DealDamage(t, amount)}}),
      ((S/PP)/Adj, λ {amount: Number => λ {t: TargetObjectOrPlayer => DealDamage(t, amount)}}),
      (S/PP, λ {t: TargetObjectOrPlayer => DealDamage(t, AttributeValue(ThisObject, Attack))}),  // (by default, a robot deals damage equal to its power)
      (S\Num, λ {amount: Number => DealDamage(ChooseO(ObjectsInPlay(AllObjects)), amount)})  // (if no target is given, any target can be chosen)
    )) +
    ("damaged" -> (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(HasProperty(IsDamaged)))})) +
    (Seq("deal", "deals", "it deals", "this robot deals", "this object deals", "take", "takes") -> (X|X, identity)) +  // e.g. deals X damage, takes X damage
    ("deck".s -> (NP\Adj, λ {p: TargetPlayer => Deck(p)})) +
    ("defending robot" -> (NP, That: Sem)) +  // This works within AfterAttack because That refers to the undergoer (patient) of an action
    ("destroy" -> (S/NP, λ {t: TargetObject => Destroy(t)})) +
    (Seq("destroys", "kills") -> Seq(
      ((S\NP)/N, λ {o: ObjectType => λ {t: TargetObject => AfterDestroysOtherObject(t, o)}}),
      ((S\NP)/N, λ {e: EnemyObject => λ {t: TargetObject => AfterDestroysOtherObject(t, e.objectType)}})
    )) +
    (Seq("destroyed by", "killed by") -> Seq(
      ((S/NP)\N, λ {o: ObjectType => λ {t: TargetObject => AfterDestroysOtherObject(t, o)}}),
      ((S/NP)\N, λ {e: EnemyObject => λ {t: TargetObject => AfterDestroysOtherObject(t, e.objectType)}})
    )) +
    (Seq("destroyed", "dies") -> Seq(
      (S\NP, λ {c: ChooseO => AfterDestroyed(AllO(c.collection))}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterDestroyed(t)}),
      (S\NP, λ {o: TargetObject => TargetHasProperty(o, IsDestroyed)}) // Condition form (e.g. "If that robot is destroyed, [...]"
    )) +
    (Seq("doesn't deal damage when attacked", "only deals damage when attacking") -> (S\NP, λ {t: TargetObject => ApplyEffect(t, CannotFightBack)})) +
    ("draw" -> (S/NP, λ {c: Cards => Draw(Self, c.num)})) +
    ("draw".s -> Seq(
      ((S/NP)\NP, λ {p: TargetPlayer => λ {c: Cards => Draw(p, c.num)}}),
      ((S/N)\NP, λ {t: TargetPlayer => λ {c: CardType => AfterCardDraw(t, c)}})  // e.g. "[whenever] you draw a robot, [do something]"
    )) +
    ("discard" -> (S/NP, λ {t: TargetCard => Discard(t)})) +
    ("discards" -> Seq(
      ((S/NP)\NP, λ {_: TargetPlayer => λ {_: CardsInHand => Fail("Cards can't force a player to make a decision (try \"random card(s)\" instead)")}}),
      ((S/NP)\NP, λ {p: TargetPlayer => λ {c: RandomCards => Discard(RandomC(c.num, CardsInHand(p, c.cardType)))}})
    )) +
    ("discard pile".s -> Seq(
      (NP\Adj, λ {p: TargetPlayer => DiscardPile(p)}),
      (NP\Adj, λ {p: TargetPlayer => CardsInDiscardPile(p)})
    )) +
    ("double" -> Seq(
      (S/NP, λ {ta: TargetAttribute => ModifyAttribute(ta.target, ta.attr, Multiply(Scalar(2)))}),
      ((S/PP)/N, λ {a: Attribute => λ {t: TargetObject => ModifyAttribute(t, a, Multiply(Scalar(2)))}}),
      (V/N, λ {a: Attribute => AttributeOperation(Multiply(Scalar(2)), a)})
    )) +
    (Seq("each", "every", "all", "each player 's", "every player 's") -> Seq(
      (Adj, AllPlayers: Sem),  // e.g. "each turn"
      (NP/PP, identity)  // e.g. "each of (your turns)"
    )) +
    (Seq("each player", "every player", "all players") -> (S/S, λ {a: Action => ForEach(AllPlayers, a)})) +
    ("end" -> (NP/PP, λ {turn: Turn => EndOfTurn(turn.player)})) +
    (Seq("end of turn", "end of the turn") -> (NP, TurnsPassed(1): Sem)) +
    (Seq("end of next turn", "end of the next turn") -> (NP, TurnsPassed(2): Sem)) +
    ("immediately" /?/ Seq("end the turn", "end your turn") -> (S, EndTurn: Sem)) +
    ("enemy" -> Seq(
      (N, EnemyObject(AllObjects): Sem),  // e.g. "whenever X destroys an enemy"
      (NP, ObjectsMatchingConditions(AllObjects, Seq(ControlledBy(Opponent))): Sem),  // e.g. "deal X damage to each enemy"
      (N/N, λ {o: ObjectType => EnemyObject(o)}),  // e.g. "whenever X destroys an enemy object"
      (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(ControlledBy(Opponent)))}),  // e.g. "deal X damage to each enemy robot"
      (NP/NP, λ {c: ObjectsMatchingConditions => ObjectsMatchingConditions(c.objectType, Seq(ControlledBy(Opponent)) ++ c.conditions)})  // e.g. "enemy robot in Y"
    )) +
    (Seq("energy", "energy to play") -> Seq(
      (NP|Num, λ {amount: Number => Energy(amount)}),
      (NP/Adj, λ {amount: Number => Energy(amount)}),
      (NP\Adj, λ {comp: Comparison => EnergyComparison(comp)}),
      (S\S, λ {aa: AttributeAdjustment => AttributeAdjustment(aa.target, Cost, aa.operation)})  // "X costs Y more" == "X costs Y more energy"
    )) +
    ("enters" -> ((S\N)/NP, λ {d: DiscardPile => λ {c: CardType => AfterCardEntersDiscardPile(d.player, c)}})) +
    ("equal" -> Seq(
      (Adj/PP, identity),
      (Adj/PP, λ {num: Number => EqualTo(num)})
    )) +
    ("even" -> (NP/N, λ {attr: Attribute => AttributeComparison(attr, IsEven)})) +
    (("event".s ++ "event card".s) -> Seq(
      (N, Event: Sem),
      (NP/PP, λ {hand: Hand => CardsInHand(hand.player, Event)}),  // e.g. "All events in your hand"
      ((NP/PP)/Adj, λ {condition: CardCondition => λ {hand: Hand => CardsInHand(hand.player, Event, Seq(condition))}}),
      (NP/PP, λ {d: DiscardPile => CardsInDiscardPile(d.player, Event)}),
      ((NP/PP)/Adj, λ {condition: CardCondition => λ {d: DiscardPile => CardsInDiscardPile(d.player, Event, Seq(condition))}})
    )) +
    (Seq("for each", "for every") -> Seq(
      (Adj/NP, λ {c: Collection => Count(c)}),  // e.g. "Draw a card for each X"
      ((NP\NP)/NP, λ {c: Collection => λ {a: AttributeOperation => a.copy(op = a.op.times(Count(c)))}}),  // e.g. "+X attack for every Y"
      ((S\S)/NP, λ {c: Collection => λ {a: Action => ForEach(c, a) }})  // "(do something) for each X"
    )) +
    ("everything" -> (N, AllObjects: Sem)) +
    ("everything adjacent to" -> (NP/NP, λ {t: TargetObject => AllO(ObjectsMatchingConditions(AllObjects, Seq(AdjacentTo(t))))})) +
    ("friendly" -> Seq(
      (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(ControlledBy(Self)))}),
      (NP/NP, λ {c: ObjectsMatchingConditions => ObjectsMatchingConditions(c.objectType, Seq(ControlledBy(Self)) ++ c.conditions)})
    )) +
    ("gain" -> Seq(
      (S/NP, λ {e: Energy => ModifyEnergy(Self, Plus(e.amount))}),  // Gain X energy.
      (S/NP, λ {l: Life => ModifyAttribute(ObjectsMatchingConditions(Kernel, Seq(ControlledBy(Self))), Health, Plus(l.amount))})  // Gain X life.
    )) +
    ("gain".s -> Seq( // "[All robots] gain ..."
      ((S\NP)/NP, λ {aa: AttributeAmount => λ {t: TargetObject => ModifyAttribute(t, aa.attr, Plus(aa.amount))}}),  // " ... X attack"
      ((S/N)\NP, λ {t: TargetObject => λ {attrs: Seq[AttributeAmount] =>  // "... X attack and Y speed"
        MultipleActions(Seq(SaveTarget(t)) ++ attrs.map(a => ModifyAttribute(SavedTargetObject, a.attr, Plus(a.amount))))}}),
      ((S/NP)\NP, λ {p: TargetPlayer => λ {e: Energy => ModifyEnergy(p, Plus(e.amount))}}),  // Y gains X energy.
      (((S\NP)/N)/Num, λ {num: Number => λ {a: Attribute => λ {t: TargetObject => ModifyAttribute(t, a, Plus(num))}}})  // Y gains X (attribute).
    )) +
    ("get".s ->  (((S/N)/Num)\NP, λ {t: TargetObject => λ {i: Scalar => λ {a: Attribute => SetAttribute(t, a, i)}}})) +  // "All robots get X attack"))
    (("get".s ++ "gain".s) -> Seq( // "[All robots] get/gain ..."
      ((S/NP)\NP, λ {t: TargetObject => λ {op: AttributeOperation => ModifyAttribute(t, op.attr, op.op)}}),  // "... +X attack"
      ((S/NP)\NP, λ {t: TargetObject => λ {ops: Seq[AttributeOperation] =>  // "... +X attack and +Y speed"
        MultipleActions(Seq(SaveTarget(t)) ++ ops.map(op => ModifyAttribute(SavedTargetObject, op.attr, op.op)))}}),
      ((S/S)\NP, λ {t: TargetObject => λ {a: Ability => GiveAbility(t, a)}}),  // "... [ability]"
      ((S/S)\NP, λ {t: TargetObject => λ {a: (AttributeOperation, Ability) =>  // "... +X attack and [ability]"
        MultipleActions(Seq(SaveTarget(t), ModifyAttribute(SavedTargetObject, a._1.attr, a._1.op), GiveAbility(SavedTargetObject, a._2)))}})
    )) +
    ("give" -> Seq( // "Give [a robot] ..."
      (((S/N)/Num)/NP, λ {t: TargetObject => λ {i: Scalar => λ {a: Attribute => ModifyAttribute(t, a, Plus(i))}}}),  // "... X attack"
      (((S/N)/Adj)/NP, λ {t: TargetObject => λ {o: Operation => λ {a: Attribute => ModifyAttribute(t, a, o)}}}),  // "... +X attack"
      ((S/N)/NP, λ {t: TargetObject => λ {attrs: Seq[AttributeAmount] =>  // "... X attack and Y speed"
        MultipleActions(Seq(SaveTarget(t)) ++ attrs.map(a => ModifyAttribute(SavedTargetObject, a.attr, Plus(a.amount))))}}),
      ((S/NP)/NP, λ {t: TargetObject => λ {ops: Seq[AttributeOperation] =>  // "... +X attack and +Y speed"
        MultipleActions(Seq(SaveTarget(t)) ++ ops.map(op => ModifyAttribute(SavedTargetObject, op.attr, op.op)))}}),
      ((S/S)/NP, λ {t: TargetObject => λ {a: Ability => GiveAbility(t, a)}}),  // "... [ability]"
      ((S/S)/NP, λ {t: TargetObject => λ {a: (AttributeOperation, Ability) =>  // "... +X attack and [ability]"
        MultipleActions(Seq(SaveTarget(t), ModifyAttribute(SavedTargetObject, a._1.attr, a._1.op), GiveAbility(SavedTargetObject, a._2)))}})
    )) +
 // ("greater than" : see "more than")+
    ("hand" -> (NP\Adj, λ {p: TargetPlayer => Hand(p)})) +
    ("halve" -> Seq(  // (Ordinarily takes Rounding, but defaults to RoundedDown.)
      ((S/NP)/Adv, λ {r: Rounding => λ {ta: TargetAttribute => ModifyAttribute(ta.target, ta.attr, Divide(Scalar(2), r))}}),
      (S/NP, λ {ta: TargetAttribute => ModifyAttribute(ta.target, ta.attr, Divide(Scalar(2), RoundedDown))}),
      (((S/PP)/Adv)/N, λ {a: Attribute => λ {r: Rounding => λ {t: TargetObject => ModifyAttribute(t, a, Divide(Scalar(2), r))}}}),
      ((S/PP)/N, λ {a: Attribute => λ {t: TargetObject => ModifyAttribute(t, a, Divide(Scalar(2), RoundedDown))}}),
      ((V/Adv)/N, λ {a: Attribute => λ {r: Rounding => AttributeOperation(Divide(Scalar(2), r), a)}}),
      (V/N, λ {a: Attribute => AttributeOperation(Divide(Scalar(2), RoundedDown), a)})
    )) +
    (Seq("has", "have") -> Seq(
      (S/NP, λ {ac: AttributeComparison => ac}),
      (S/NP, λ {cs: Seq[AttributeComparison] => cs}), // multiple conditions
      ((S\NP)/S, λ {a: Ability => λ {t: TargetObject => HasAbility(t, a)}}),
      ((S\NP)/N, λ {a: AttributeAmount => λ {t: TargetObject => AttributeAdjustment(t, a.attr, Constant(a.amount))}}),  // "... X attack"
      ((S/NP)\NP, λ {t: TargetObject => λ {op: AttributeOperation => AttributeAdjustment(t, op.attr, op.op)}}),  // "... +X attack"
      ((S\NP)/NP, λ {comp: CardComparison => λ {coll: CardCollection => CollectionCountComparison(coll, comp.comp)}}),  // "... +X attack"
      ((S/S)\NP, λ {t: TargetObject => λ {a: (AttributeOperation, Ability) =>  // "... +X attack and [ability]"
        MultipleAbilities(Seq(AttributeAdjustment(t, a._1.attr, a._1.op), HasAbility(t, a._2)))}}),
      ((S/S)\NP, λ {t: TargetObject => λ {a: (Ability, AttributeOperation) =>  // "... [ability] and +X attack"
        MultipleAbilities(Seq(AttributeAdjustment(t, a._2.attr, a._2.op), HasAbility(t, a._1)))}})
    )) +
    (Seq("health", "life") -> Seq(
      (N, Health: Sem),
      (N\Num, λ {i: Scalar => AttributeAmount(i, Health)}),
      (NP|Adj, λ {amount: Number => AttributeAmount(amount, Health)}),
      (NP\Adj, λ {op: Operation => AttributeOperation(op, Health)}),
      (NP\Num, λ {n: Number => AttributeComparison(Health, EqualTo(n))}), // "...with X health"(implied "equal to" in there)
      (NP\Adj, λ {comp : Comparison => AttributeComparison(Health, comp)}), // "...greater than X health"
      (NP|Num, λ {amount: Number => Life(amount)}),
      (NP/Adj, λ {amount: Number => Life(amount)})
    )) +
    ("if" -> Seq(
      ((S|S)|S, λ {c: GlobalCondition => λ {a: Action => If(c, a)}}),  // "if" for actions
      ((S|S)|S, λ {c: GlobalCondition => λ {a: PassiveAbility => a.conditionOn(c)}})   // "if" for abilities
    )) +
    (Seq("in", "on", "of", "from", "into") -> (PP/NP, identity)) +
    ("instead" -> (S|S, λ {a: Action => Instead(a)})) +
    ("in combat" -> (S\S, λ {t: AfterDestroyed => AfterDestroyed(t.target, Combat)})) +
    (Seq("in play", "on the board") -> (NP\N, λ {o: ObjectType => ObjectsInPlay(o)})) +
    ("is" -> (X|X, identity)) +
    ("it" -> (NP, ItO: Sem)) +
    ("its" -> (Num/N, λ {a: SingleAttribute => AttributeValue(ItO, a)})) +
    ("its controller" -> (NP, ControllerOf(ItO): Sem)) +
    (Seq("its owner 's hand", "its controller 's hand", "their owner 's hands", "their controller 's hands") -> (NP, ItsOwnersHand: Sem)) +
    (("kernel".s ++ "core".s) -> (N, Kernel: Sem)) +
    ("less" -> (Adv\Num, λ {num: Number => Minus(num)})) +
    (Seq("less than", "<") -> (Adj/Num, λ {num: Number => LessThan(num)})) +
    ("lose" -> Seq(
      (S/NP, λ {e: Energy => ModifyEnergy(Self, Minus(e.amount))}),  // Lose X energy.
      (S/NP, λ {_: AllEnergy.type => ModifyEnergy(Self, Constant(Scalar(0)))})  // Lose all energy.
    )) +
    (Seq("lose", "pay") -> (S/NP, λ {l: Life => DealDamage(Self, l.amount)})) +
    ("lose".s -> Seq(
      ((S/NP)\NP, λ {p: TargetPlayer => λ {_: AllEnergy.type => ModifyEnergy(p, Constant(Scalar(0)))}}),  // Y loses all energy.
      ((S/NP)\NP, λ {p: TargetPlayer => λ {e: Energy => ModifyEnergy(p, Minus(e.amount))}}),  // Y loses X energy.
      (((S\NP)/N)/Num, λ {num: Number => λ {a: Attribute => λ {t: TargetObject => ModifyAttribute(t, a, Minus(num))}}})  // Y loses X (attribute).
    )) +
    ("more" -> (Adv\Num, λ {num: Number => Plus(num)})) +
    ("move" -> Seq(
      ((S/PP)/NP, λ {t: TargetObject => λ {dest: TargetTile => MoveObject(t, dest)}}),  // e.g. "Move a robot to X"
      ((S/NP)/NP, λ {t: TargetObject => λ {d: WithinDistance => MultipleActions(Seq(  // e.g. "Move a robot up to X spaces"
        SaveTarget(t),
        MoveObject(SavedTargetObject, ChooseT(TilesMatchingConditions(Seq(WithinDistanceOf(d.spaces, SavedTargetObject), Unoccupied)))))
      )}})
    )) +
    (Seq("more than", "greater than", ">") -> (Adj/Num, λ {num: Number => GreaterThan(num)})) +
    ("moved last turn" -> (S, HasProperty(MovedLastTurn): Sem)) +
    ("moved this turn" -> (S, HasProperty(MovedThisTurn): Sem)) +
    ("moves" -> Seq(
      (S\NP, λ {c: ChooseO => AfterMove(AllO(c.collection))}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterMove(t)})
    )) +
    ("must" -> (X/X, identity)) +
    ("named" -> ((NP/NP)\NP, λ {c: GeneratedCard => λ {n: Name => GeneratedCard(c.cardType, c.attributes, Some(n.name))}})) +
    ("number" -> (Num/PP, λ {c: Collection => Count(c)})) +
    (("object".s :+ "objects '") -> (N, AllObjects: Sem)) +
    ("odd" -> (NP/N, λ {attr: Attribute => AttributeComparison(attr, IsOdd)})) +
    ("of" -> ((S/NP)\V, λ {ops: Seq[AttributeOperation] => λ {t: TargetObject => MultipleActions(Seq(SaveTarget(t)) ++ ops.map(op => ModifyAttribute(SavedTargetObject, op.attr, op.op)))}})) +
    (Seq("other", "another") -> (NP/N, λ {o: ObjectType => Other(ObjectsInPlay(o))})) +
    (Seq("or", "and") -> ((N/N)\N, λ {o1: ObjectType => λ {o2: ObjectType => MultipleObjectTypes(Seq(o1, o2))}})) +
    ("or less" -> Seq(
      (Adj\Num, λ {num: Number => LessThanOrEqualTo(num)}),
      (NP\N, λ {aa: AttributeAmount => AttributeComparison(aa.attr, LessThanOrEqualTo(aa.amount))})
    )) +
    (Seq("or more", "or greater") -> Seq(
      (Adj\Num, λ {num: Number => GreaterThanOrEqualTo(num)}),
      (NP\N, λ {aa: AttributeAmount => AttributeComparison(aa.attr, GreaterThanOrEqualTo(aa.amount))})
    )) +
    ("pay" -> (S/NP, λ {e: Energy => PayEnergy(Self, e.amount)})) +
    ("play".s -> Seq(
      ((S/N)\NP, λ {t: TargetPlayer => λ {c: CardType => AfterCardPlay(t, c)}}),  // e.g. "[whenever] you play a robot, [do something]"
      ((NP\N)\NP, λ {t: TargetPlayer => λ {c: CardType => CardPlay(t, c)}})  // e.g. "robots you play [cost X less, etc]"
    )) +
    (Seq("played", "comes into play", "enters the board") -> Seq(
      (S\N, λ {t: CardType => AfterCardPlay(AllPlayers, t)}),
      (S\NP, λ {c: ChooseO => AfterPlayed(AllO(c.collection))}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterPlayed(t)})
    )) +
    ("random" -> Seq(
      ((NP/N)\Num, λ {num: Number => λ {c: CardType => RandomCards(num, c)}}),
      ((NP/N)\Num, λ {num: Number => λ {o: ObjectType => RandomO(num, ObjectsInPlay(o))}}),  // e.g. "Destroy a random robot"
      ((NP/NP)\Num, λ {num: Number => λ {c: CardCollection => RandomC(num, c)}}),  // e.g. "Discard 2 random cards"
      ((NP/NP)\Num, λ {num: Number => λ {o: ObjectCollection => RandomO(num, o)}}),
      ((NP/NP)\Num, λ {num: Number => λ {t: TileCollection => RandomT(num, t)}})
    )) +
    ("reduce" -> Seq(
      (((S/PP)/PP)/N, λ {a: Attribute => λ {t: TargetObject => λ {num: Number => ModifyAttribute(t, a, Minus(num))}}}),
      (((S/PP)/PP)/N, λ {a: Attribute => λ {t: TargetCard => λ {num: Number => ModifyAttribute(t, a, Minus(num))}}}),
      (((S/PP)/PP)/N, λ {a: Attribute => λ {c: CardsInHand => λ {num: Number => ModifyAttribute(AllC(c), a, Minus(num))}}})  // e.g. "Reduce the cost of robot cards in your hand by 1"
    )) +
    ("remove all abilities" -> (S/PP, λ {t: TargetObject => RemoveAllAbilities(t)})) +
    ("restore" -> Seq(
      ((S/PP)/N, λ {a: Attribute => λ {t: TargetObjectOrPlayer => RestoreAttribute(t, a, None)}}),  // e.g. "Restore health to X"
      (((S/PP)/N)/Num, λ {n: Number => λ {a: Attribute => λ {t: TargetObjectOrPlayer => RestoreAttribute(t, a, Some(n))}}}),  // e.g. "Restore N health to X"
      (S/NP, λ {ta: TargetAttribute => RestoreAttribute(ta.target, ta.attr, None)})  // e.g. "Restore X's health"
    )) +
    (Seq("return", "move") -> Seq(
      ((S/PP)/NP, λ {t: TargetObject => λ {_: ItsOwnersHand.type => ReturnToHand(t)}}),  // e.g. "Return a robot to its owner's hand"
      ((S/PP)/NP, λ {t: TargetObject => λ {h: Hand => ReturnToHand(t, Some(h.player))}}),  // e.g. "Return all robots to your hand"
      ((S/PP)/NP, λ {c: TargetCard => λ {h: Hand => MoveCardsToHand(c, h.player)}})  // e.g. "Return a random robot from your discard pile to your hand"
    )) +
    (Seq("return", "move", "play") -> Seq(
      ((S/PP)/NP, λ {c: TargetCard => λ {dest: TargetTile => SpawnObject(c, dest)}})  // e.g. "Return a random robot from your discard pile to a random tile"
    )) +
    (Seq("returns", "moves", "plays") -> Seq(
      (((S\NP)/PP)/NP, λ {c: TargetCard => λ {dest: TargetTile => λ {p: TargetPlayer => SpawnObject(c, dest, p)}}})  // e.g. "Your opponent returns a random robot from their discard pile to a random tile"
    )) +
    (("robot".s :+ "robots '") -> Seq(
      (N, Robot: Sem),
      (NP/PP, λ {hand: Hand => CardsInHand(hand.player, Robot)}),  // e.g. "all robots in your hand"
      (NP/PP, λ {d: DiscardPile => CardsInDiscardPile(d.player, Robot)}),
      ((NP/PP)/Adj, λ {condition: CardCondition => λ {hand: Hand => CardsInHand(hand.player, Robot, Seq(condition))}}),
      ((NP/PP)/Adj, λ {condition: CardCondition => λ {d: DiscardPile => CardsInDiscardPile(d.player, Robot, Seq(condition))}}),
      (NP\Adj, λ {attrs: Seq[AttributeAmount] => GeneratedCard(Robot, attrs)})  // e.g. "a 3/1/2 robot"
    )) +
    ("robot on the board" -> (N, Robot: Sem)) +  // e.g. "If you control a robot on the board with 3 or more health, ..."
    ("rounded down" -> (Adv, RoundedDown: Sem)) +
    ("rounded up" -> (Adv, RoundedUp: Sem)) +
    ("set" -> Seq(
      ((S/PP)/NP, λ {t: TargetAttribute => λ {num: Number => SetAttribute(t.target, t.attr, num)}}),
      (((S/PP)/PP)/N, λ {a: Attribute => λ {t: TargetObject => λ {num: Number => SetAttribute(t, a, num)}}})
    )) +
    ("shuffle".s -> ((S/PP)/NP, λ {c: TargetCard => λ {d: Deck => ShuffleCardsIntoDeck(c, d.player)}})) +
    (Seq("space", "tile", "hex") -> Seq(
      (NP\Num, λ {num: Number => if (num == Scalar(1)) Spaces(num) else Fail("Use 'tiles' instead of 'tile'") }),
      (NP\Num, λ {num: Number => if (num == Scalar(1)) WithinDistance(num) else Fail("Use 'tiles' instead of 'tile'") }),
      (NP/PP, λ {c: ObjectCondition => TilesMatchingConditions(Seq(c))})
    )) +
    (Seq("spaces", "tiles", "hexes") -> Seq(
      (NP, AllTiles: Sem),  // e.g. "all tiles"
      (NP\Num, λ {num: Number => Spaces(num)}),  // e.g. "3 tiles"
      (NP\Adj, λ {c: LessThanOrEqualTo => WithinDistance(c.num)}),  // e.g. "up to 3 tiles away"
      (NP/PP, λ {c: ObjectCondition => TilesMatchingConditions(Seq(c))})  // e.g. "all tiles adjacent to your kernel"
    )) +
    (Seq("spawn", "create") -> ((S/PP)/NP, λ {c: TargetCard => λ {t: TargetTile => SpawnObject(c, t, Self)}})) +
    (Seq("spawns", "creates") -> (((S\NP)/PP)/NP, λ {c: TargetCard => λ {t: TargetTile =>  λ {p: TargetPlayer => SpawnObject(c, t, p)}}})) +
    ("speed" -> Seq(
      (N, Speed: Sem),
      (N\Num, λ {i: Scalar => AttributeAmount(i, Speed)}),
      (NP|Adj, λ {amount: Number => AttributeAmount(amount, Speed)}),
      (NP\Adj, λ {op: Operation => AttributeOperation(op, Speed)}),
      (NP\Adj, λ {comp : Comparison => AttributeComparison(Speed, comp)}), // need for "> x speed"
      (NP\Num, λ {n: Number => AttributeComparison(Speed, EqualTo(n))}) // need for "...with X speed" (implied "equal to" in there)
    )) +
    (("structure".s :+ "structures '") -> Seq(
      (N, Structure: Sem),
      (NP/PP, λ {hand: Hand => CardsInHand(hand.player, Structure)}),  // e.g. "All structures in your hand"
      (NP/PP, λ {d: DiscardPile => CardsInDiscardPile(d.player, Structure)}),
      ((NP/PP)/Adj, λ {condition: CardCondition => λ {hand: Hand => CardsInHand(hand.player, Structure, Seq(condition))}}),
      ((NP/PP)/Adj, λ {condition: CardCondition => λ {d: DiscardPile => CardsInDiscardPile(d.player, Structure, Seq(condition))}}),
    )) +
    ("swap" -> Seq(
      ((S/N)/NP, λ {t: TargetObject => λ {attrs: Seq[Attribute] => SwapAttributes(t, attrs(0), attrs(1))}}),
      ((S/PP)/N, λ {attrs: Seq[Attribute] => λ {t: TargetObject => SwapAttributes(t, attrs(0), attrs(1))}})
    )) +
    ("take control" -> (S/PP, λ {t: TargetObject => TakeControl(Self, t)})) +
    ("takes control" -> ((S\NP)/PP, λ {t: TargetObject => λ {p: TargetPlayer => TakeControl(p, t)}})) +
    ("takes damage" -> Seq(
      (S\NP, λ {c: ChooseO => AfterDamageReceived(AllO(c.collection))}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterDamageReceived(t)})
    )) +
    (Seq("target", "a target") -> (NP/NP, λ {c: ObjectCollection => ChooseO(c)})) +
    (Seq("to", "equal to") -> Seq(
      (PP/NP, identity),
      (PP/Num, identity)
    )) +
    ("that" -> Seq(
      ((NP\N)/S, λ {c: ObjectCondition => λ { o: ObjectType => ObjectsMatchingConditions(o, Seq(c))}}),
      ((NP\N)/S, λ {cs: Seq[ObjectCondition] => λ { o: ObjectType => ObjectsMatchingConditions(o, cs)}})
    )) +
    ("that" / Seq("robot", "creature", "structure", "object") -> (NP, That: Sem)) +
    (("that cost".s ++ "which costs".s) -> Seq(
      ((NP\NP)/NP, λ {e: Energy => λ {c: CardsInHand => CardsInHand(c.player, c.cardType, c.conditions :+ AttributeComparison(Cost, EqualTo(e.amount)))}}),
      ((NP\NP)/NP, λ {ec: EnergyComparison => λ {c: CardsInHand => CardsInHand(c.player, c.cardType, c.conditions :+ AttributeComparison(Cost, ec.comp))}}),
      ((NP\NP)/NP, λ {e: Energy => λ {c: CardsInDiscardPile => CardsInDiscardPile(c.player, c.cardType, c.conditions :+ AttributeComparison(Cost, EqualTo(e.amount)))}}),
      ((NP\NP)/NP, λ {ec: EnergyComparison => λ {c: CardsInDiscardPile => CardsInDiscardPile(c.player, c.cardType, c.conditions :+ AttributeComparison(Cost, ec.comp))}})
    )) +
    (Seq("that player", "they") -> (NP, ItP: Sem)) +
    ("the" -> (X/X, identity)) +
    ("their" -> Seq(
      (Adj, TheyP: Sem),
      (Num/N, λ {a: SingleAttribute => AttributeValue(They, a)})
    )) +
    (Seq("there is", "there is a", "there is an") -> (S/NP, λ {c: Collection => CollectionExists(c)})) +
    (Seq("then", "and", "to") -> ((S/S)\S, λ {a1: Action => λ {a2: Action => And(a1, a2)}})) +
    ("this" / Seq("robot", "creature", "structure", "object") -> (NP, ThisObject: Sem)) +
    ("total" -> ((Num/PP)/N, λ {a: SingleAttribute => λ {c: ObjectOrCardCollection => AttributeSum(c, a)}})) +
    ("transform" -> Seq(
      ((S/PP)/NP, λ {source: TargetObject => λ {target: TargetCard => Become(source, target)}}), // used with aCopyOf
      ((S/PP)/NP, λ {source: TargetObject => λ {target: GeneratedCard => Become(source, target)}}) // only used in such things as "becomes a robot with 1 attack and...".
    )) +
    ("turn".s -> (NP\Adj, λ {p: TargetPlayer => Turn(p)})) +
    ("until" -> ((S|S)|NP, λ {d: Duration => λ {a: Action => Until(d, a)}})) +
    (Seq("when", "whenever", "after", "immediately after", "each time", "every time") ->
      ((S|S)|S, λ {t: Trigger => λ {a: Action => TriggeredAbility(t, a)}})
    ) +
    ("with" -> Seq(  // "with" = "that" + "has"
      (Adj/NP, identity),
      ((NP\N)/NP, λ {s: AttributeComparison => λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(s))}}),
      ((NP\N)/NP, λ {s: Seq[AttributeComparison] => λ {o: ObjectType => ObjectsMatchingConditions(o, s)}}),
      ((NP\N)/N, λ {attrs: Seq[AttributeAmount] => λ {o: ObjectType => GeneratedCard(o, attrs)}})
    )) +
    ("within" -> Seq(
      (PP/NP, λ {s: Spaces => WithinDistanceOf(s.num, ThisObject)}),
      ((PP/PP)/NP, λ {s: Spaces => λ {t: TargetObject => WithinDistanceOf(s.num, t)}}),
      ((NP\NP)/NP, λ {s: Spaces => λ {c: ObjectsMatchingConditions => ObjectsMatchingConditions(c.objectType, c.conditions :+ WithinDistanceOf(s.num, ThisObject))}})
    )) +
    (Seq("you", "yourself") -> (NP, Self: Sem)) +
    ("your" -> Seq(
      (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(ControlledBy(Self)))}),
      (NP/NP, λ {c: ObjectsMatchingConditions => ObjectsMatchingConditions(c.objectType, c.conditions :+ ControlledBy(Self))}),
      (Adj, Self: Sem)
    )) +
    ("your energy" -> (NP, EnergyAmount(Self): Sem)) +
    ("your maximum energy" -> (NP, MaximumEnergyAmount(Self): Sem)) +
    (Seq("your opponent", "the opponent") -> (NP, Opponent: Sem)) +
    ("your opponent 's energy" -> (NP, EnergyAmount(Opponent): Sem)) +
    ("your opponent 's maximum energy" -> (NP, MaximumEnergyAmount(Opponent): Sem)) +
    (Seq("your opponent 's", "the opponent 's", "all of your opponent 's") -> Seq(
      (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(ControlledBy(Opponent)))}),
      (NP/NP, λ {c: ObjectsMatchingConditions => ObjectsMatchingConditions(c.objectType, c.conditions :+ ControlledBy(Opponent))}),
      (Adj, Opponent: Sem)
    )) +
    (Seq("'", "'s") -> Seq(
      ((NP\NP)/N, λ {a: Attribute => λ {t: TargetObjectOrPlayer => TargetAttribute(t, a)}}),
      ((NP\NP)/N, λ {a: SingleAttribute => λ {t: TargetObject => AttributeValue(t, a)}})
    )) +
    ("\"" -> Seq(
      (Quoted/S, identity),
      (S\Quoted, identity)
    )) +
    (StrictIntegerMatcher -> (Num, {i: Int => Scalar(i): Sem})) +
    (NumberWordMatcher -> (Num, {i: Int => Scalar(i): Sem})) +
    (PrefixedIntegerMatcher("+") -> (Adj, {i: Int => Plus(Scalar(i)): Sem})) +
    (PrefixedIntegerMatcher("-") -> (Adj, {i: Int => Minus(Scalar(i)): Sem})) +
    (StatsTripleMatcher -> (NP/N, {s: StatsTriple =>
      λ {o: ObjectType =>
        GeneratedCard(o, Seq(AttributeAmount(Scalar(s.attack), Attack), AttributeAmount(Scalar(s.health), Health), AttributeAmount(Scalar(s.speed), Speed)))}
    })) +
    (NameMatcher -> (NP, {str: String => Name(str): Sem}))

  /** Like [[lexicon]], but with null semantics (i.e. all semantic values set to [[Ignored]]),
    * and with added dummy entries for each syntactic category (in [[categoriesMap]].
    * Used by [[ErrorAnalyzer.syntacticParse]] to diagnose whether a given error is syntactic or semantic. */
  lazy val syntaxOnlyLexicon: ParserDict[CcgCat] = {
    ParserDict[CcgCat](
      lexicon.map.mapValues(_.map {case (syn, _) => (syn, Ignored(""))}),
      lexicon.funcs.map(func => {str: String => func(str).map {case (syn, _) => (syn, Ignored(""))}}),
      lexicon.fallbacks.map(func => {str: String => func(str).map {case (syn, _) => (syn, Ignored(""))}})
    ).withTerms(categoriesMap)
  }

  /** Dummy entries for each syntactic category in the [[lexicon]], with null semantics. */
  lazy val categoriesMap: Map[String, Seq[(CcgCat, Sem)]] = {
    categories.map(cat => s"#${cat.category.toLowerCase.replaceAll("[()]", "")}#" -> Seq(cat -> Ignored(""))).toMap
  }
  /** Dummy entries for each *terminal* (non-compositional) syntactic category in the [[lexicon]], with null semantics. */
  lazy val terminalCategoriesMap: Map[String, Seq[(CcgCat, Sem)]] = {
    categoriesMap.filterKeys(cat => cat.matches(raw"[^/|\\]*"))
  }

  /** Returns all terms with a corresponding entry in the given syntactic category. */
  def termsInCategory(category: CcgCat): Seq[String] = {
    lexicon.map
      .filter { case (_, definition) => definition.exists(_._1 == category) }
      .keys
      .toSeq
  }

  /** List of all terms in [[lexicon]]. */
  lazy val listOfTerms: List[String] = lexicon.map.keys.toList.sorted

  /** A human-readable JSON representation of the lexicon, used by [[Server]]'s /lexicon endpoint. */
  lazy val asJson: Json = {
    import io.circe.generic.auto._
    import io.circe.syntax._

    lexicon.map
      .mapValues((defs: Seq[(CcgCat, SemanticState)]) => defs.map { case (syn, sem) =>
        LexiconDefinition(
          syn.toString
            .replaceAllLiterally("Noun", "N")
            .replaceAllLiterally("\\", "\\\\"),
          sem.toString
            .replaceAllLiterally("Lambda", "λ ")
            .replaceAllLiterally("\\", "\\\\")
            .replaceAllLiterally("\"", "\\\"")
            .replaceAllLiterally("\n", " ")
        )
      })
      .asJson
  }

  /** List of all CCG categories in [[lexicon]], sorted in increasing complexity. */
  private lazy val categories: Seq[CcgCat] = {
    lexicon.map
      .flatMap(d => d._2.map(_._1))
      .toSeq
      .distinct
      .sortBy(_.category.count(raw"/|\\" contains _))  // Sort categories by increasing complexity.
  }
}
