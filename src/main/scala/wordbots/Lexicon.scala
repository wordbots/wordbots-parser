package wordbots

import com.workday.montague.ccg._
import com.workday.montague.parser.ParserDict
import com.workday.montague.semantics._
import com.workday.montague.semantics.FunctionReaderMacro.λ
import io.circe.Json
import wordbots.Semantics.TileCollection

import org.apache.commons.lang.StringUtils

import scala.language.implicitConversions
import scala.language.postfixOps

case class LexiconDefinition(syntax: String, semantics: String)

object Fail {
  def apply(str: String = ""): Unit = throw new ClassCastException(str)
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
      (NP/NP, λ {c: TileCollection => ChooseT(c)}),
      (Num, Scalar(1): Sem)  // e.g. "(draw) a card"
    )) +
    ("a player" -> (NP, ChooseO(ObjectsInPlay(Kernel)): Sem)) +
    ("a random tile" -> (NP, RandomT(Scalar(1), AllTiles): Sem)) +
    ("a tile" -> (NP, ChooseT(AllTiles): Sem)) +
    ("activate:" -> (S/S, λ {a: Action => ActivatedAbility(a)})) +
    ("adjacent" -> Seq(
      (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(AdjacentTo(ThisObject)))}),
      (NP/NP, λ {c: ObjectsMatchingConditions => ObjectsMatchingConditions(c.objectType, Seq(AdjacentTo(ThisObject)) ++ c.conditions)}),
      (NP/NP, λ {c: TilesMatchingConditions => TilesMatchingConditions(Seq(AdjacentTo(ThisObject)) ++ c.conditions)})
    )) +
    ("adjacent tile" -> (NP, TilesMatchingConditions(Seq(AdjacentTo(They))): Sem)) +  // e.g. "Move each robot to a random adjacent tile."
    ("is" / Seq("adjacent to", "adjacent to a", "adjacent to an") -> Seq(
      ((S\NP)/N, λ {t: ObjectType => λ {o: TargetObject => CollectionExists(ObjectsMatchingConditions(t, Seq(AdjacentTo(o))))}}),
      ((S\N)/NP, λ {o: TargetObject => λ {e: EnemyObject => CollectionExists(ObjectsMatchingConditions(e.objectType, Seq(AdjacentTo(o), ControlledBy(Opponent))))}}),
      ((S\NP)/NP, λ {c: ObjectsMatchingConditions => λ {o: TargetObject => CollectionExists(ObjectsMatchingConditions(c.objectType, c.conditions :+ AdjacentTo(o)))}})
    )) +
    ("is not" / Seq("adjacent to", "adjacent to a", "adjacent to an") -> Seq(
      ((S\NP)/N, λ {t: ObjectType => λ {o: TargetObject => NotGC(CollectionExists(ObjectsMatchingConditions(t, Seq(AdjacentTo(o)))))}}),
      ((S\N)/NP, λ {o: TargetObject => λ {e: EnemyObject => NotGC(CollectionExists(ObjectsMatchingConditions(e.objectType, Seq(AdjacentTo(o), ControlledBy(Opponent)))))}}),
      ((S\NP)/NP, λ {c: ObjectsMatchingConditions => λ {o: TargetObject => NotGC(CollectionExists(ObjectsMatchingConditions(c.objectType, c.conditions :+ AdjacentTo(o))))}})
    )) +
    ("adjacent to" -> Seq(
      (PP/NP, λ {t: TargetObjectOrTile => AdjacentTo(t)}),
      (PP/NP, λ {t: TargetObject => ChooseT(TilesMatchingConditions(Seq(AdjacentTo(t))))}),
      ((NP/NP)\N, λ {o: ObjectType => λ {t: TargetObjectOrTile => ObjectsMatchingConditions(o, Seq(AdjacentTo(t)))}}),
      (PP/NP, λ {c: ObjectsMatchingConditions => ObjectsMatchingConditions(c.objectType, Seq(AdjacentTo(ThisObject)) ++ c.conditions)})
    )) +
    ("after attacking" -> (S\S, λ {a: Action => TriggeredAbility(AfterAttack(ThisObject, AllObjects), a)})) +
    (Seq("all", "each", "every") -> Seq( // Also see Seq("each", "every") below for definitions that DON'T apply to "all".
      (NP/N, λ {o: ObjectType => ObjectsInPlay(o)}),
      (NP/N, λ {c: CardType => AllC(CardsInHand(AllPlayers, c))}),
      (NP/NP, λ {c: CardCollection => AllC(c)}),
      (NP/NP, λ {c: Collection => c}),
      (NP/PP, λ {c: Collection => c}),
      ((NP/PP)/N, λ {o: ObjectType => λ {c: ObjectCondition => ObjectsMatchingConditions(o, Seq(c))}})
    )) +
    ("all" /?/ Seq("attributes", "stats") -> (N, AllAttributes: Sem)) +
    ("all energy" -> (NP, AllEnergy: Sem)) +
    (Seq("all players", "each player", "every player", "either player", "a player", "both players") -> (NP, AllPlayers: Sem)) +
    ("all your energy" -> (NP, Energy(EnergyAmount(Self)): Sem)) +
    (Seq("all your other", "all of your other", "your other") -> (NP/N, λ {o: ObjectType => Other(ObjectsMatchingConditions(o, Seq(ControlledBy(Self))))})) +
    ("and" -> Seq(
      // Specific.
      ((S/S)\S, λ {a: TriggeredAbility => λ {b: TriggeredAbility => MultipleAbilities(Seq(a, b))}}),
      ((S/S)\S, λ {a: Action => λ {b: Action => MultipleActions(Seq(a, b))}}),
      ((N/N)\N, λ {a1: Attribute => λ {a2: Attribute => MultipleAttributes(Seq(a1, a2))}}),
      (((N\N)\N)/N, λ {a1: Attribute => λ {a2: Attribute => λ {a3: Attribute => MultipleAttributes(Seq(a1, a2, a3))}}}),
      ((NP/NP)\NP, λ {t1: TargetObject => λ {t2: TargetObject => UnionO(Seq(t1, t2))}}),
      // General.
      (conj, λ {b: ParseNode => λ {a: Seq[ParseNode] => a :+ b}}),
      (ReverseConj, λ {a: ParseNode => λ {b: ParseNode => Seq(a, b)}}),
      ((S/S)\NP, λ {a: ParseNode => λ {b: ParseNode => (a, b)}}),  // e.g. "+1 attack and Haste"
      ((S/NP)\S, λ {a: ParseNode => λ {b: ParseNode => (a, b)}}),  // e.g. "Haste and +1 attack"
      // Support arity-3 conjugations of AttributeAmounts (e.g. "1 attack, 2 health, and 3 speed") (using conditional Fail() to enforce types):
      (((N\N)\N)/N, λ {c: ParseNode => λ {b: ParseNode => λ {a: ParseNode => if (Seq(a, b, c).forall(_.isInstanceOf[AttributeAmount])) Seq(a, b, c) else Fail() }}}),
      // Support arity-3 and arity-4 conjugations of TextReplacements:
      (((NP\NP)\NP)/NP, λ {c: ParseNode => λ {b: ParseNode => λ {a: ParseNode => if (Seq(a, b, c).forall(_.isInstanceOf[TextReplacement])) Seq(a, b, c) else Fail() }}}),
      ((((NP\NP)\NP)\NP)/NP, λ {d: ParseNode => λ {c: ParseNode => λ {b: ParseNode => λ {a: ParseNode => if ((Seq(a, b, c, d).forall(_.isInstanceOf[TextReplacement]))) Seq(a, b, c, d) else Fail() }}}})
    )) +
    ("another" -> (NP/N, λ {o: ObjectType => ChooseO(Other(ObjectsInPlay(o)))})) +
    ("any card" -> (N, AnyCard: Sem)) +
    ("at" -> ((S|S)/NP, λ {t: Trigger => λ {a: Action => TriggeredAbility(t, a)}})) +
    (Seq("at most", "up to") -> Seq(
      (Adj/Num, λ {num: Number => LessThanOrEqualTo(num)}),
      (PP/PP, λ {dist: ExactDistanceFrom => WithinDistanceOf(dist.distance, dist.obj)}),
      (((NP\NP)/PP), λ {dist: ExactDistanceFrom => λ {c: ObjectsMatchingConditions => ObjectsMatchingConditions(c.objectType, c.conditions :+ WithinDistanceOf(dist.distance, dist.obj))}})  // "an enemy robot up to X tiles away"
    )) +
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
      ((S\NP)/N, λ {o: ObjectType => λ {t: TargetObject => AfterAttack(t, o)}}),
      ((S\N)/NP, λ {t: TargetObject => λ {o: ObjectType => AfterAttackedBy(t, o)}})
    )) +
    ("attacked last turn" -> (S, HasProperty(AttackedLastTurn): Sem)) +
    ("attacked this turn" -> (S, HasProperty(AttackedThisTurn): Sem)) +
    ("away" -> Seq(
      (PP\NP, λ {s: Spaces => ExactDistanceFrom(s.num, ItO)}),  // "X spaces away" (ItO rather than CurrentObject to allow this to be used in Action cards - semantically this will still work the same)
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
    (Seq("can activate", "can activate again") -> (S\NP, λ {t: TargetObject => CanActivateAgain(t)})) +
    (Seq("can move", "can move again", "gains a second move action") -> (S\NP, λ {t: TargetObject => CanMoveAgain(t)})) +
    (Seq("can attack", "can attack again") -> (S\NP, λ {t: TargetObject => CanAttackAgain(t)})) +
    (Seq("can move and attack", "can move and attack again") -> (S\NP, λ {t: TargetObject => CanMoveAndAttackAgain(t)})) +
    ("can move over other objects" -> (S\NP, λ {t: TargetObject => ApplyEffect(t, CanMoveOverObjects)})) +
    ("can only attack" -> ((S\NP)/NP, λ {target: TargetObject => λ {attacker: TargetObject => ApplyEffect(attacker, CanOnlyAttack(target))}})) +
    ("can't activate" -> (S\NP, λ {t: TargetObject => ApplyEffect(t, CannotActivate)})) +
    ("can't attack" -> (S\NP, λ {t: TargetObject => ApplyEffect(t, CannotAttack)})) +
    ("can't defend" -> (S\NP, λ {t: TargetObject => ApplyEffect(t, CannotFightBack)})) +
    ("can't move" -> Seq(
      (S\NP, λ {t: TargetObject => ApplyEffect(t, CannotMove)}),  // "X can't move"
      ((S\NP)/PP, λ {c: ObjectCondition => λ {t: TargetObject => ApplyEffect(t, CannotMoveTo(TilesMatchingConditions(Seq(c))))}})  // e.g. "X can't move adjacent to this object"
    )) +
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
    ("choose" -> (S/NP, λ {t: TargetObject => SaveTarget(t)})) +
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
      (NP/PP, λ {target: TargetObjectOrCard => AttributeValue(target, Cost)}),  // e.g. "[X equal to the] energy cost of a card in your hand"
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
    ("deals damage" -> Seq(
      ((S\N)/PP, λ {target: TargetObject => λ {objType: ObjectType => AfterDamageReceived(target, objType)}}),
      (S\NP, λ {c: ChooseO => AfterDealsDamage(AllO(c.collection), AllObjects)}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterDealsDamage(t, AllObjects)})
    )) +
    ("deals damage to a" -> ((S\NP)/N, λ {o: ObjectType => λ {t: TargetObject => AfterDealsDamage(t, o)}})) +
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
    ("didn't move last turn" -> (S\NP, λ {t: TargetObject => NotGC(TargetHasProperty(t, MovedLastTurn))})) +
    (Seq("didn't move this turn", "didn't move") -> (S\NP, λ {t: TargetObject => NotGC(TargetHasProperty(t, MovedThisTurn))})) +
    (Seq("doesn't deal damage when attacked", "only deals damage when attacking") -> (S\NP, λ {t: TargetObject => ApplyEffect(t, CannotFightBack)})) +
    ("draw" -> (S/NP, λ {c: Cards => Draw(Self, c.num)})) +
    ("draw".s -> Seq(
      ((S/NP)\NP, λ {p: TargetPlayer => λ {c: Cards => Draw(p, c.num)}}),
      ((S/N)\NP, λ {t: TargetPlayer => λ {c: CardType => AfterCardDraw(t, c)}})  // e.g. "[whenever] you draw a robot, [do something]"
    )) +
    ("discard" -> Seq(
      (S/NP, λ {t: TargetCard => Discard(t)}),  // e.g. "Discard a random card in your hand"
      (S/NP, λ {h: Hand => if (h.player == Self) Discard(AllC(CardsInHand(Self))) else Fail("You can only discard your own cards.") })  // "Discard your hand"
    )) +
    ("discard all cards" -> (S, Discard(AllC(CardsInHand(Self))): Sem)) +
    ("discards" -> Seq(
      ((S/NP)\NP, λ {_: TargetPlayer => λ {_: CardsInHand => Fail("Cards can't force a player to make a decision (try \"random card(s)\" instead)")}}),
      ((S/NP)\NP, λ {p: TargetPlayer => λ {c: RandomCards => Discard(RandomC(c.num, CardsInHand(p, c.cardType)))}}),
      ((S/NP)\NP, λ {p: TargetPlayer => λ {h: Hand => if (h.player == TheyP) Discard(AllC(CardsInHand(p))) else Fail("Players can only discard 'their hand', not any other hand.") }})
    )) +
    ("discards all cards" -> (S \ NP, λ { p: TargetPlayer => Discard(AllC(CardsInHand(p))) })) +
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
    ("empty" -> (NP/NP, λ {t: TilesMatchingConditions => t.copy(conditions = t.conditions :+ Unoccupied)})) +
    ("empty tile".s -> (NP, TilesMatchingConditions(Seq(Unoccupied)): Sem)) +
    ("end" -> (NP/PP, λ {turn: Turn => EndOfTurn(turn.player)})) +
    (Seq("end of turn", "end of the turn") -> (NP, TurnsPassed(1): Sem)) +
    (Seq("end of next turn", "end of the next turn") -> (NP, TurnsPassed(2): Sem)) +
    ("immediately" /?/ Seq("end the turn", "end your turn") -> (S, EndTurn: Sem)) +
    ("enemy" -> Seq(
      (N, EnemyObject(AllObjects): Sem),  // e.g. "whenever X destroys an enemy"
      (NP, ObjectsMatchingConditions(AllObjects, Seq(ControlledBy(Opponent))): Sem),  // e.g. "deal X damage to each enemy"
      (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(ControlledBy(Opponent)))}),  // e.g. "deal X damage to each enemy robot"
      (N/N, λ {o: ObjectType => EnemyObject(o)}),  // e.g. "whenever X destroys an enemy object"
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
    (("event".s ++ "event card".s ++ "action".s ++ "action card".s) -> Seq(
      (N, Event: Sem),
      (NP/PP, λ {hand: Hand => CardsInHand(hand.player, Event)}),  // e.g. "All events in your hand"
      ((NP/PP)/Adj, λ {condition: CardCondition => λ {hand: Hand => CardsInHand(hand.player, Event, Seq(condition))}}),
      (NP/PP, λ {d: DiscardPile => CardsInDiscardPile(d.player, Event)}),
      ((NP/PP)/Adj, λ {condition: CardCondition => λ {d: DiscardPile => CardsInDiscardPile(d.player, Event, Seq(condition))}})
    )) +
    (Seq("for each", "for every") -> Seq(
      (Adj/NP, λ {c: Collection => Count(c)}),  // e.g. "Draw a card for each X"
      ((NP\NP)/NP, λ {c: Collection => λ {a: AttributeOperation => a.copy(op = a.op.times(Count(c)))}}),  // e.g. "+X attack for every Y"
      ((Adv\Adv)/NP, λ {c: Collection => λ {op: Operation => op.times(Count(c))}}),  // e.g. "1 less for every X"
      ((S\S)/NP, λ {c: Collection => λ {a: Action => ForEach(c, a) }})  // "(do something) for each X"
    )) +
    ("everything" -> (N, AllObjects: Sem)) +
    ("everything adjacent to" -> (NP/NP, λ {t: TargetObject => AllO(ObjectsMatchingConditions(AllObjects, Seq(AdjacentTo(t))))})) +
    //scalastyle:off magic.number
    (Seq("four", "4") -> Seq(
      (NP/N, λ {o: ObjectType => ChooseO(ObjectsInPlay(o), Scalar(4))}),
      (NP/N, λ {c: CardType => ChooseC(CardsInHand(Self, c), Scalar(4))}),
      (NP/NP, λ {t: TileCollection => ChooseT(t, Scalar(4))})
    )) +
    //scalastyle:on magic.number
    ("friendly" -> Seq(
      (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(ControlledBy(Self)))}),
      (NP/NP, λ {c: ObjectsMatchingConditions => ObjectsMatchingConditions(c.objectType, Seq(ControlledBy(Self)) ++ c.conditions)})
    )) +
    ("gain" -> Seq(
      (S/NP, λ {e: Energy => ModifyEnergy(Self, Plus(e.amount))}),  // Gain X energy.
      (S/NP, λ {l: Life => ModifyAttribute(ObjectsMatchingConditions(Kernel, Seq(ControlledBy(Self))), Health, Plus(l.amount))}),  // Gain X life.
      (S/NP, λ {ao: AttributeOperation => ao match {  // For backwards compatibility: "Gain +X life" == "Gain X life"
        case AttributeOperation(Plus(amount), Health) => ModifyAttribute(ObjectsMatchingConditions(Kernel, Seq(ControlledBy(Self))), Health, Plus(amount))
        case _ => Fail("You can only gain life")
      }})
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
        MultipleActions(Seq(SaveTarget(t), ModifyAttribute(SavedTargetObject, a._1.attr, a._1.op), GiveAbility(SavedTargetObject, a._2)))}}),
      ((S/S)\NP, λ {t: TargetObject => λ {a: (Ability, AttributeOperation) =>  // "... [ability] and +X attack"
        MultipleActions(Seq(SaveTarget(t), ModifyAttribute(SavedTargetObject, a._2.attr, a._2.op), GiveAbility(SavedTargetObject, a._1)))}})
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
    ("hand" -> Seq(
      (NP\Adj, λ {p: TargetPlayer => Hand(p)}),
      (NP\Adj, λ {p: TargetPlayer => CardsInHand(p)})
    )) +
    ("half" -> Seq(  // (Ordinarily takes Rounding, but defaults to RoundedDown.)
      (NP/NP, λ {num: Number => Half(num, RoundedDown) }),
      ((NP/Adv)/NP, λ {num: Number => λ {r: Rounding => Half(num, r) }})
    )) +
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
        MultipleAbilities(Seq(AttributeAdjustment(t, a._2.attr, a._2.op), HasAbility(t, a._1)))}}),
      ((S/NP)\NP, λ {t: TargetObject => λ {ac: AttributeComparison => TargetMeetsCondition(t, ac)}}) // "... >X attack" as a condition
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
      ((S|S)|S, λ {c: GlobalCondition => λ {a: Action => If(c, a)}}),  // "if" for actions: if (condition) (action)
      (((S/NP)/S)|S, λ {o: TargetObject => λ {c: ObjectCondition => λ {a: Action => If(TargetMeetsCondition(o, c), a)}}})   // if ((object) (condition)) (action)
    )) +
    (Seq("if", "when", "whenever") -> ((S|S)|S, λ {c: GlobalCondition => λ {a: PassiveAbility => a.conditionOn(c)}})) + // "if" for abilities
    (Seq("in", "on", "of", "from", "into") -> (PP/NP, identity)) +
    ("increase" -> ((S/PP)/N, λ {a: TargetAttribute => λ {i: Scalar => ModifyAttribute(a.target, a.attr, Plus(i))}})) +  // e.g. "increase its attack by X"
    ("instead" -> (S|S, λ {a: Action => Instead(a)})) +
    ("in combat" -> (S\S, λ {t: AfterDestroyed => AfterDestroyed(t.target, Combat)})) +
    (Seq("in play", "on the board") -> (NP\N, λ {o: ObjectType => ObjectsInPlay(o)})) +
    (Seq("is", "are") -> (X|X, identity)) +
    ("is" -> ((S\NP)/PP, λ {c: ObjectCondition => λ {o: TargetObject => TargetMeetsCondition(o, c)}})) +
    ("it" -> (NP, ItO: Sem)) +
    ("its" -> Seq(
      (Num/N, λ {a: SingleAttribute => AttributeValue(ItO, a)}),
      (N/N, λ {a: SingleAttribute => TargetAttribute(ItO, a)})
    )) +
    ("its controller" -> (NP, ControllerOf(ItO): Sem)) +
    (Seq("its owner 's hand", "its controller 's hand", "their owner 's hands", "their controller 's hands") -> (NP, ItsOwnersHand: Sem)) +
    (("kernel".s ++ "core".s) -> (N, Kernel: Sem)) +
    (Seq("less", "fewer") -> Seq(
      (Adv\Num, λ {num: Number => Minus(num)}),
      ((NP/PP)/N, λ {attr: SingleAttribute => λ { rel: RelativeTo => AttributeComparison(attr, LessThan(AttributeValue(rel.obj, attr)))}})  // i.e. "less health than this robot"
    )) +
    (Seq("less than", "fewer than", "<") -> (Adj/Num, λ {num: Number => LessThan(num)})) +
    ("lose" -> Seq(
      (S/NP, λ {e: Energy => ModifyEnergy(Self, Minus(e.amount))}),  // Lose X energy.
      (S/NP, λ {_: AllEnergy.type => ModifyEnergy(Self, Constant(Scalar(0)))})  // Lose all energy.
    )) +
    (Seq("lose", "pay") -> (S/NP, λ {l: Life => DealDamage(Self, l.amount)})) +
    ("lose".s -> Seq(
      ((S/NP)\NP, λ {p: TargetPlayer => λ {_: AllEnergy.type => ModifyEnergy(p, Constant(Scalar(0)))}}),  // Y loses all energy.
      ((S/NP)\NP, λ {p: TargetPlayer => λ {e: Energy => ModifyEnergy(p, Minus(e.amount))}}),  // Y loses X energy.
      ((S\NP)/NP, λ {aa: AttributeAmount => λ {t: TargetObject => ModifyAttribute(t, aa.attr, Minus(aa.amount))}}),  // " ... X attack"
      (((S\NP)/N)/Num, λ {num: Number => λ {a: Attribute => λ {t: TargetObject => ModifyAttribute(t, a, Minus(num))}}})  // Y loses X (attribute).
    )) +
    ("more" -> Seq(
      (Adv\Num, λ {num: Number => Plus(num)}),
      ((NP/PP)/N, λ {attr: SingleAttribute => λ { rel: RelativeTo => AttributeComparison(attr, GreaterThan(AttributeValue(rel.obj, attr)))}})  // i.e. "more health than this robot"
    )) +
    ("move" -> Seq(
      ((S/PP)/NP, λ {t: TargetObject => λ {dest: TargetTile => MoveObject(t, dest)}}),  // e.g. "Move a robot to X"
      ((S/NP)/NP, λ {t: TargetObject => λ {s: Spaces => MultipleActions(Seq(  // e.g. "Move a robot X spaces"
        SaveTarget(t),
        MoveObject(SavedTargetObject, ChooseT(TilesMatchingConditions(Seq(ExactDistanceFrom(s.num, SavedTargetObject), Unoccupied)))))
      )}}),
      ((S/NP)/NP, λ {t: TargetObject => λ {d: WithinDistance => MultipleActions(Seq(  // e.g. "Move a robot up to X spaces"
        SaveTarget(t),
        MoveObject(SavedTargetObject, ChooseT(TilesMatchingConditions(Seq(WithinDistanceOf(d.spaces, SavedTargetObject), Unoccupied)))))
      )}})
    )) +
    (Seq("more than", "greater than", ">") -> (Adj/Num, λ {num: Number => GreaterThan(num)})) +
    ("moved last turn" -> Seq(
      (S, HasProperty(MovedLastTurn): Sem),
      (S\NP, λ {t: TargetObject => TargetHasProperty(t, MovedLastTurn)})
    )) +
    (Seq("moved this turn", "moved") -> Seq(
      (S, HasProperty(MovedThisTurn): Sem),
      (S\NP, λ {t: TargetObject => TargetHasProperty(t, MovedThisTurn)})
    )) +
    ("moves" -> Seq(
      (S\NP, λ {c: ChooseO => AfterMove(AllO(c.collection))}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterMove(t)})
    )) +
    ("must" -> (X/X, identity)) +
    ("named" -> ((NP/NP)\NP, λ {c: GeneratedCard => λ {n: Name => GeneratedCard(c.cardType, c.attributes, Some(n.name))}})) +
    ("not" -> Seq(
      (PP/PP, λ {c: ObjectCondition => NotC(c)}),
      (PP/PP, λ {c: GlobalCondition => NotGC(c)})
    )) +
    ("number" -> (Num/PP, λ {c: Collection => Count(c)})) +
    (("object".s :+ "objects '") -> (N, AllObjects: Sem)) +
    ("odd" -> (NP/N, λ {attr: Attribute => AttributeComparison(attr, IsOdd)})) +
    ("of" -> Seq(
      ((S/NP)\V, λ {ops: Seq[AttributeOperation] => λ {t: TargetObject => MultipleActions(Seq(SaveTarget(t)) ++ ops.map(op => ModifyAttribute(SavedTargetObject, op.attr, op.op)))}}),
      ((NP/NP)\Num, λ {num: Number => λ {o: ObjectCollection => ChooseO(o, num)}})  // e.g. "X of your opponent's robots"
    )) +
    ("other" -> Seq(
      (NP/N, λ {o: ObjectType => Other(ObjectsInPlay(o))}),
      (NP/NP, λ {oc: ObjectCollection => Other(oc)})
    )) +
    (Seq("or", "and") -> ((N/N)\N, λ {o1: ObjectType => λ {o2: ObjectType => MultipleObjectTypes(Seq(o1, o2))}})) +
    (Seq("or less", "or fewer") -> Seq(
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
    ("replace" -> Seq(
      ((S/PP)/NP, λ {r: TextReplacement => λ {t: TargetCard => RewriteText(t, Map(r.from.text -> r.to.text))}}),
      ((S/PP)/NP, λ {rs: Seq[TextReplacement] => λ {t: TargetCard => RewriteText(t, rs.map(r => (r.from.text -> r.to.text)).toMap)}})
    )) +
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
    (Seq("spawn", "create", "place") -> ((S/PP)/NP, λ {c: TargetCard => λ {t: TargetTile => SpawnObject(c, t, Self)}})) +
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
      ((NP/PP)/Adj, λ {condition: CardCondition => λ {d: DiscardPile => CardsInDiscardPile(d.player, Structure, Seq(condition))}})
    )) +
    ("swap" -> Seq(
      ((S/N)/NP, λ {t: TargetObject => λ {attrs: Seq[Attribute] => SwapAttributes(t, attrs(0), attrs(1))}}),
      ((S/PP)/N, λ {attrs: Seq[Attribute] => λ {t: TargetObject => SwapAttributes(t, attrs(0), attrs(1))}})
    )) +
    ("swap the position".s ->
      (S/PP, λ {t: Seq[TargetObject] => if (t.length == 2) SwapPositions(t(0), t(1)) else Fail("Must swap the positions of two objects")})
    ) +
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
    ("than" -> (PP/NP, λ {t: TargetObject => RelativeTo(t)})) +
    ("that" -> Seq(
      ((NP\N)/S, λ {c: ObjectCondition => λ { o: ObjectType => ObjectsMatchingConditions(o, Seq(c))}}),
      ((NP\N)/S, λ {cs: Seq[ObjectCondition] => λ { o: ObjectType => ObjectsMatchingConditions(o, cs)}})
    )) +
    ("that" / Seq("robot", "structure", "object") -> (NP, That: Sem)) +
    (("that cost".s ++ "which cost".s) -> Seq(
      ((NP\NP)/NP, λ {e: Energy => λ {c: CardsInHand => CardsInHand(c.player, c.cardType, c.conditions :+ AttributeComparison(Cost, EqualTo(e.amount)))}}),
      ((NP\NP)/NP, λ {ec: EnergyComparison => λ {c: CardsInHand => CardsInHand(c.player, c.cardType, c.conditions :+ AttributeComparison(Cost, ec.comp))}}),
      ((NP\NP)/NP, λ {e: Energy => λ {c: CardsInDiscardPile => CardsInDiscardPile(c.player, c.cardType, c.conditions :+ AttributeComparison(Cost, EqualTo(e.amount)))}}),
      ((NP\NP)/NP, λ {ec: EnergyComparison => λ {c: CardsInDiscardPile => CardsInDiscardPile(c.player, c.cardType, c.conditions :+ AttributeComparison(Cost, ec.comp))}})
    )) +
    ("that much" -> (Num, ThatMuch: Sem)) +
    (Seq("that player", "they") -> (NP, ItP: Sem)) +
    ("the" -> (X/X, identity)) +
    ("their" -> Seq(
      (Adj, TheyP: Sem),
      (Num/N, λ {a: SingleAttribute => AttributeValue(They, a)}),
      (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(ControlledBy(TheyP)))}),
      (NP/NP, λ {c: ObjectsMatchingConditions => ObjectsMatchingConditions(c.objectType, c.conditions :+ ControlledBy(TheyP))})
    )) +
    (Seq("there is", "there is a", "there is an") -> (S/NP, λ {c: Collection => CollectionExists(c)})) +
    (Seq("then", "and", "to") -> ((S/S)\S, λ {a1: Action => λ {a2: Action => And(a1, a2)}})) +
    ("this" / Seq("robot", "structure", "object", "kernel") -> (NP, ThisObject: Sem)) +
    (Seq("three", "3") -> Seq(
      (NP/N, λ {o: ObjectType => ChooseO(ObjectsInPlay(o), Scalar(3))}),
      (NP/N, λ {c: CardType => ChooseC(CardsInHand(Self, c), Scalar(3))}),
      (NP/NP, λ {t: TileCollection => ChooseT(t, Scalar(3))})
    )) +
    ("total" -> ((Num/PP)/N, λ {a: SingleAttribute => λ {c: ObjectOrCardCollection => AttributeSum(c, a)}})) +
    (Seq("transform", "turn") -> Seq(
      ((S/PP)/NP, λ {source: TargetObject => λ {target: TargetCard => Become(source, target)}}), // used with aCopyOf
      ((S/PP)/NP, λ {source: TargetObject => λ {target: GeneratedCard => Become(source, target)}}) // only used in such things as "becomes a robot with 1 attack and...".
    )) +
    ("turn".s -> (NP\Adj, λ {p: TargetPlayer => Turn(p)})) +
    (Seq("two", "2") -> Seq(
      (NP/N, λ {o: ObjectType => Seq(ChooseO(ObjectsInPlay(o)), ChooseO(ObjectsInPlay(o)))}),
      (NP/N, λ {o: ObjectType => ChooseO(ObjectsInPlay(o), Scalar(2))}),
      (NP/N, λ {c: CardType => ChooseC(CardsInHand(Self, c), Scalar(2))}),
      (NP/NP, λ {t: TileCollection => ChooseT(t, Scalar(2))})
    )) +
    ("unless" -> Seq(
      ((S|S)|S, λ {c: GlobalCondition => λ {a: Action => If(NotGC(c), a)}}),  // "if" for actions
      ((S|S)|S, λ {c: GlobalCondition => λ {a: PassiveAbility => a.conditionOn(NotGC(c))}})   // "if" for abilities
    )) +
    ("until" -> ((S|S)|NP, λ {d: Duration => λ {a: Action => Until(d, a)}})) +
    (Seq("when", "whenever", "after", "immediately after", "each time", "every time") -> Seq(
      ((S|S)|S, λ {t: Trigger => λ {a: Action => TriggeredAbility(t, a)}}),  // triggered ability: When [trigger], [action]
      ((S|S)|S, λ {c: GlobalCondition => λ {a: Action => ConditionalAction(c, a)}})  // passive ability: When [condition], [action]
    )) +
    (("win".s ++ Seq("win the game", "wins the game")) -> ((S\NP, λ {p: TargetPlayer => WinGame(p)}))) +
    ("with" -> Seq(  // "with" = "that" + "has"
      (Adj/NP, identity),
      (ReverseConj, λ {a: ParseNode => λ {b: ParseNode => Seq(a, b)}}),
      ((NP\N)/NP, λ {s: AttributeComparison => λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(s))}}),
      ((NP\N)/NP, λ {s: Seq[AttributeComparison] => λ {o: ObjectType => ObjectsMatchingConditions(o, s)}}),
      ((NP\N)/N, λ {attr: AttributeAmount => λ { o: ObjectType => GeneratedCard(o, Seq(attr))}}),  // (generated card with 1 attribute, useful only for structures)
      ((NP\N)/N, λ {attrs: Seq[AttributeAmount] => λ {o: ObjectType => GeneratedCard(o, attrs)}}),
      ((NP\S)/S, λ {toText: Text => λ {fromText: Text => TextReplacement(fromText, toText)}})  // i.e. "Replace \"<from>\" with \"<to>\" on ..."
    )) +
    ("within" -> Seq(
      (PP/NP, λ {s: Spaces => WithinDistanceOf(s.num, ThisObject)}),
      ((PP/PP)/NP, λ {s: Spaces => λ {t: TargetObjectOrTile => WithinDistanceOf(s.num, t)}}),
      ((NP\NP)/NP, λ {s: Spaces => λ {c: ObjectsMatchingConditions => ObjectsMatchingConditions(c.objectType, c.conditions :+ WithinDistanceOf(s.num, ThisObject))}})
    )) +
    (Seq("you", "yourself") -> Seq(
      (NP, Self: Sem),
      (NP, ObjectsMatchingConditions(Kernel, List(ControlledBy(Self))): Sem)  // in a pinch, "you" == "your kernel"
    )) +
    ("your" -> Seq(
      (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(ControlledBy(Self)))}),
      (NP/NP, λ {c: ObjectsMatchingConditions => ObjectsMatchingConditions(c.objectType, c.conditions :+ ControlledBy(Self))}),
      (Adj, Self: Sem)
    )) +
    ("your energy" -> (NP, EnergyAmount(Self): Sem)) +
    ("your maximum energy" -> (NP, MaximumEnergyAmount(Self): Sem)) +
    (Seq("your opponent", "the opponent") -> Seq(
      (NP, Opponent: Sem),
      (NP, ObjectsMatchingConditions(Kernel, List(ControlledBy(Opponent))): Sem)  // in a pinch, "your opponent" == "your opponent's kernel"
    )) +
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
    (StrictIntegerMatcher -> (NP/NP, {i: Int => (λ {o: ObjectCollection => ChooseO(o, Scalar(i)) })})) +  // e.g. "2 enemy robots"
    (NumberWordMatcher -> (Num, {i: Int => Scalar(i): Sem})) +
    (NumberWordMatcher -> (NP/NP, {i: Int => (λ {o: ObjectCollection => ChooseO(o, Scalar(i)) })})) +  // e.g. "2 enemy robots"
    (PrefixedIntegerMatcher("+") -> (Adj, {i: Int => Plus(Scalar(i)): Sem})) +
    (PrefixedIntegerMatcher("-") -> (Adj, {i: Int => Minus(Scalar(i)): Sem})) +
    (StatsTripleMatcher -> (NP/N, {s: StatsTriple =>
      λ {o: ObjectType =>
        GeneratedCard(o, Seq(AttributeAmount(Scalar(s.attack), Attack), AttributeAmount(Scalar(s.health), Health), AttributeAmount(Scalar(s.speed), Speed)))}
    })) +
    (NameMatcher -> (NP, {str: String => Name(str): Sem})) +  // i.e. for object spawn effects
    (TextMatcher -> (S, {str: String => Text(str): Sem}))  // i.e. for card rewrite effects

  /** Like [[lexicon]], but with null semantics (i.e. all semantic values set to [[Ignored]]),
    * and with added dummy entries for each syntactic category (in [[categoriesMap]].
    * Used by [[ErrorAnalyzer.syntacticParse]] to diagnose whether a given error is syntactic or semantic. */
  val syntaxOnlyLexicon: ParserDict[CcgCat] = {
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
      .sortBy(t => mapOfTermsToUsages(t))
      .reverse
  }

  /** List of all terms in [[lexicon]]. */
  val listOfTerms: List[String] = lexicon.map.keys.toList.sorted

  /**
   *  A collection of all text on all cards on the production Wordbots instance as of July 16, 2023.
   *  Produced by running the following command on the game state after loading Firebase data:
   *    state.collection.allCards.map(c => c.text).flatMap(t => t?.split('\n')).map(t => t?.trim()).filter(t => t).map(expandKeywords).join(' ').toLowerCase()
   */
  val cardTextCorpus_07_16_23: String = "your opponent's adjacent robots can only attack this object. at the beginning of each of your turns, this robot gains 1 attack. \"when this object is played,\" gain 2 energy and draw a card. \"when this object is destroyed,\" your opponent gains 2 energy, then your opponent draws a card \"this robot can move and attack immediately after it is played.\". \"when this object is played,\" lose 2 life. activate: give a friendly robot +2 attack, then deal 3 damage to your kernel. activate: restore 1 health to all adjacent friendly robots at the start of your turn, a random friendly robot gains 1 attack and a random enemy robot gains 1 attack. \"your opponent's adjacent robots can only attack this object.\" at the start of your turn, gain 1 energy and lose 1 life. activate: pay 1 energy and each player draws a card all robots have +1 speed. at the end of your turn, deal 1 damage to each player. when this robot is destroyed, take control of all adjacent robots. at the end of each turn, each kernel gains 1 health \"this robot can move and attack immediately after it is played.\". robots you play cost 1 less energy. activate: discard a card, then draw a card. \"when this object is played,\" deal 2 damage to your opponent. \"when this object is played,\" give adjacent robots 3 health \"this robot can't attack.\",. \"your opponent's adjacent robots can only attack this object.\" activate: pay 3 energy and discard a card, then this robot gains 1 attack and 1 health. \"this robot can't attack.\",. \"this robot can move and attack immediately after it is played.\" at the start of your turn, draw a card and lose 2 life. \"this robot can move over other objects.\" when this robot attacks, it deals damage to all adjacent robots instead. at the start of your turn, pay 1 energy and this robot gains 1 health. \"this robot can't attack.\",. \"your opponent's adjacent robots can only attack this object.\" at the end of each turn, each robot takes 1 damage. when this robot is played, deal 4 damage. \"when this object is played,\" all of your other robots can move again. adjacent robots have +1 attack gain energy equal to the total power of robots you control. destroy all robots you control. gain 2 energy. draw 2 cards. discard a robot card. gain life equal to its health. move a robot up to 2 spaces. destroy a structure. give a robot \"when this robot attacks, restore 3 health to your kernel\" double a robot's health reduce the cost of all cards in your hand by 1. set the attack of all robots equal to their health deal 1 damage to everything adjacent to a tile. give all robots you control +2 attack. deal 3 damage to a robot. deal 4 damage to your opponent. set all stats of all robots in play to 3. draw cards equal to the number of robots you control. destroy all robots that have less than 2 speed. remove all abilities from all robots set the attack and speed of all robots to 0. give all robots \"activate: destroy this robot\". give your robots +2 attack and +2 health. deal 1 damage to each enemy object your adjacent robots have +1 health. at the start of each player's turn, that player gains 1 energy if they control an adjacent robot. all robots have \"this robot can move over other objects.\". \"when this object is played,\" give all friendly robots +1 speed and -1 health. all robots have \"whenever this robot attacks a kernel, draw a card\". whenever a robot is destroyed in combat, deal 1 damage to its controller. at the start of your turn, this structure gains 1 health. activate: destroy all robots with cost equal to this structure's health. activate: destroy this structure. \"when this object is destroyed,\" restore all adjacent robots' health when you play a robot, this structure becomes a copy of that robot. activate: destroy this structure. \"when this object is destroyed,\" deal 2 damage to all objects within 2 spaces. activate: move an enemy robot adjacent to this structure up to 3 tiles. activate: move a friendly robot 1 tile. activate: pay 2 energy to give a robot \"your opponent's adjacent robots can only attack this object.\". when this robot is played, pay 1 energy to give this robot this robot can move and attack immediately after it is played. reduce health of adjacent enemy robots by 2 at the end of each turn. return all enemy robots to your hand. defend . at the start of your turn, all adjacent friendly robots get +1 health and \"\"your opponent's adjacent robots can only attack this object.\"\" and this robot gets +1 health, then spawn a 1/2/2 robot named \"knight\" in a random adjacent tile. \"your opponent's adjacent robots can only attack this object.\". activate: give this robot +1 health. whenever you play an action, create a copy of this robot on a random adjacent tile and lose 1 life.  at the end of your turn, destroy this robot and the opponent loses 2 life. activate: deal 2 damage to a enemy robot, and give this robot 2 health then the enemy robot lose 2 health. all cards in your opponent's hand cost 5 more energy. deal 7 damage to enemy robots. activate: deal 2 damage to a robot 4 spaces away. \"when this object is played,\" all friendly robots gain \"when this robot destroys an enemy robot, this robot can move and attack again\". at the start of your turn, deal 1 damage to this structure. activate: spawn a copy of this structure on each adjacent tile, then destroy this structure. \"when this object is destroyed,\" give a random adjacent structure you control +1 health. \"when this object is played,\" destroy your opponent's kernel. your kernel gains \"activate: deal 2 damage to this object and you draw a card\". whenever this robot moves it can move again. all your robots have \"this robot can move and attack immediately after it is played.\". destroy all robots with energy cost 3 or less. activate: return an adjacent friendly robot to your hand then gain energy equal to its cost and lose 1 energy. activate: destroy this robot then shuffle 3 random cards from your discard pile into your deck then draw a card. at the start of each of your turns, this robot gains 1 attack. set this robot's attack to 0 after attacking. this robot only deals damage when attacking. give a robot +1 attack.  deal 1 damage to that robot. whenever a player plays a robot, return a random structure to their hand.  whenever a player play a structure, return a random robot to their hand. at the end of your turn, all objects take 2 damage and deal 3 damage to this structure. \"when this object is played,\" this structure gains 5 health. activate: deal 2 damage to an enemy robot. activate: pay 2 energy and this robot gains attack equal to a random robot's attack. set a enemy robot's attack to 0 . \"when this object is played,\" discard your hand and shuffle a random action card from your discard pile into your deck.   \"when this object is played,\" draw 3 cards. activate: adjacent friendly robots gain 1 health. \"when this object is played,\" destroy all robots . at the start of your turn, spawn 1/1/1 robots named \"b\" on each adjacent tile. discard 2 cards then gain 5 energy. \"when this object is played,\" draw a card. activate: discard a card then gain 2 energy. give all your robots 99 attack and 99 health. deal 5 damage to a enemy robot then return a random enemy structure to your hand. activate: pay 1 energy to return a random card from your discard pile into your hand. \"when this object is played,\" deal 2 damage to each adjacent robot. activate: deal 2 damage to each adjacent robot. at the end of each turn, deal 1 damage to all adjacent objects. activate: destroy all enemy robots. activate: give your robots +1 attack . whenever this robot moves, it loses 1 speed and it can move and attack again. whenever this robot attacks, it loses 1 attack and it can move and attack again. at the start of your turn, this robot gains 2 attack and 2 speed. \"when this object is destroyed,\" return a random robot from your discard pile to your hand and lose 1 life. when a friendly robot is destroyed, your kernel gains 2 health. whenever an enemy robot attacks, destroy that robot and this robot. deal 9 damage to all enemy robots and enemy kernel. discard all cards in your hand, then draw cards equal to the number of robots you control. destroy each robot adjacent to a kernel.  restore 1 health to each kernel. activate: shuffle an action from your discard pile into your deck and draw a card. at the start of your turn, all friendly structures gain 2 health then lose 1 energy for each structure you control. activate: destroy this structure. at the start of your turn, adjacent friendly robots and structures gain 1 health. \"your opponent's adjacent robots can only attack this object.\". give a robot 2 health. this robot has +1 attack for each structure you control. \"when this object is played,\" deal 8 damage to all robots and kernels. all robots have \"this robot can move over other objects.\". your robots have \"this robot can move over other objects.\".  at the end of your turn, destroy adjacent robots you control. pay 5 energy to draw 2 cards. draw cards equal to the number of structures you control. activate: spawn a 3/5/1 robot named \"pc\" on a tile adjacent to this robot. when this robot dies gain 2 life. \"this robot can move over other objects.\".   \"when this object is played,\" spawn a 0/2/0 robot named \"impact debris\" on each adjacent tile. adjacent enemy robots have -1 speed. adjacent robots have -1 attack. activate: you gain 1 energy for each robot adjacent to this object. draw 4 cards.   discard 2 cards.   lose 2 health. at the start of your turn, gain 1 energy for each adjacent friendly robot you control. your objects can't activate. \"when this object is played,\" destroy all robots. when this robot attacks, destroy all robots. when this robot attacks the kernel, deal 3 damage to the enemy kernel. whenever your opponent plays a robot, remove all abilities from that robot and this robot. \"when this object is played,\" draw 30 cards and discard your hand.     activate: deal 6 damage to an adjacent object and shuffle a random card from your discard pile into your deck. activate: this robot gains 2 attack and 2 health for each card in your hand then discard your hand. this robot only deals damage when attacking. \"when this object is played,\" deal 1 damage. activate: deal 1 damage. activate: reduce the cost of a card in your hand by 1. set this robot's attack to 1 after attacking. activate: if you have 1 health, you win. adjacent robots you control have -2 attack.  \"when this object is destroyed,\" draw a card and gain 1 life. at the start of your turn, gain 2 energy if your kernel has even health. at the start of your turn, destroy this robot if your kernel has odd health. enemy robots adjacent to your kernel can't attack. activate: this robot gains health equal to your energy then pay all your energy. activate: each adjacent structure becomes a copy of this robot. activate: deal 3 damage to a robot or structure 3 tiles away. \"this robot can move and attack immediately after it is played.\". \"when this object is played,\" lose 2 life. \"when this object is played,\" destroy a friendly robot and gain 1 energy. activate: draw a card and adjacent robots gain +2 health. activate: draw a card and adjacent robots gain +2  speed. \"when this object is played,\", draw a card for each  your damaged robot or structure. return each robot to its owner's hand. at the beginning of your turn, set this robot's health to 4. whenever an adjacent robot takes damage, your kernel gains 1 health.  whenever this robot takes damage, your kernel gains 1 health. activate: spawn a 3/4/2 robot named \"three bot\" on a random adjacent tile. at the end of each of your turns, all of your adjacent robots gain 1 health. at the start of each turn, spawn a 0/1/1 robot named \"as: scrap\" on a random tile and, you, draw 1 card.     at the beginning of your turn, this structure gains 1 health. destroy a robot with 5 or more attack. \"when this object is played,\" this robot gains 1 attack and 1 health for each card in your discard pile. whenever a card enters your discard pile, this robot gains 1 attack and 1 health. every time this robot takes damage it gains +2 attack. \"this robot can move over other objects.\" . \"this robot can move and attack immediately after it is played.\". after this robot attacks, this robot loses 1 speed. whenever this robot takes damage, restore 1 health to this robot. activate: give an adjacent robot \"whenever this robot takes damage, restore 1 health to this robot\". when you play an action, return a random card from your discard pile to your hand. when this robot dies, your kernel gain 5 health. when this robot attacks a kernel, swap the position of this robot and the enemy kernel. activate: deal 5 damage to your opponent's kernel. at the end of your turn, if your kernel has less than 7 health, draw a card. activate: draw a card then destroy a friendly robot. activate: pay 4 energy to destroy an enemy robot or structure then deal 5 damage to the enemy kernel. when your kernel takes damage, your kernel gains 1 health. give a friendly robot \"your opponent's adjacent robots can only attack this object.\". give a robot 1 speed and 1 health.  draw a card. all friendly robots gain 1 speed. activate: another robot can move and attack again. spawn 3/1/3 robots named \"discotron\" with \"\"this robot can move and attack immediately after it is played.\"\" in 5 random empty tile. give all enemy robots and structures and kernels \"at the start of your turn, this robot takes 2 damage\". activate: an enemy robot loses 1 speed. activate: an enemy robot loses 1 attack. \"when this object is played,\" discard a card. at the start of your turn, draw 3 cards and gain 2 energy then destroy this structure. \"when this object is played,\" destroy all robots. whenever this robot attacks return a random robot from your discard pile on a random tile. \"when this object is played,\" your kernel gains 1 health for each friendly robot. all cards cost 3 energy. deal 3 damage to a robot. at the beginning of each turn draw 2 cards. whenever this robot takes damage, restore 1 health to your kernel. at the start of your turn, your kernel gains 1 health. activate: your kernel gains 1 health. \"your opponent's adjacent robots can only attack this object.\".   whenever a robot moves gain 1 life. whenever this robot moves, move a random adjacent enemy robot to a random tile. deal 2 damage to an enemy robot.  if it is destroyed, gain 2 energy. \"when this object is played,\" spawn a 0/1/0 robot named \"faygo\" on every tile adjacent to a random enemy robot. \"when this object is played,\" destroy all robots and each player discard all cards and each player draws 3 cards. move your kernel up to 3 tiles. \"when this object is played,\" remove all abilities from adjacent enemy robots. \"when this object is played,\" destroy a enemy robot. \"when this object is destroyed,\" reduce the attack of all adjacent enemy robots by 1. \"this robot can't attack.\".  \"when this object is played,\" create a 2/4/1 robot named \"wall outgrowth\" in a random empty adjacent tile. adjacent robots you control have \"this robot can't attack.\". at the end of your turn, take control of adjacent enemy robots. whenever a robot dies, draw a card. whenever this robot attacks, remove all abilities from their robot. activate: deal 5 damage to an enemy robot. when this robot is played, gain +2 life. return a robot from your discard pile to your hand. \"when this object is played,\" draw 1 card.  activate: restore 1 health to a robot. \"when this object is destroyed,\" draw a card. all friendly robots gain 2 attack until end of turn. activate: deal damage equal to this structure's attack to an object 3 tiles away. activate: this structure gains 1 speed and set this structure's attack to 0. activate: set this structure's attack to 7 and this structure loses 1 speed. when this robot attacks, this robot gains 1 speed then draw a card. robots your opponent plays cost 10 energy. at the beginning of your turn, destroy this structure and draw 4 cards. gain 2 energy for every robot you control. \"when this object is played,\" deals 1 damage. all adjacent friendly robots have \"activate: deal damage equal to its health to an enemy 2 tiles away and destroy this robot\". when this robot attacks, your kernel gets +1 health. gain 20 energy. remove all abilities from all robots.  all robots gain \"\"when this object is destroyed,\" draw a card\". draw a card.  lose 1 life. at the start of your turn, destroy a random friendly adjacent robot. when a friendly robot dies, this robot gains health equal to half that robot's cost then this robot gains attack equal to half that robot's cost. whenever this robot attacks a kernel, draw a card. at the start of your turn, all friendly robots gain +1 speed. activate: pay 1 energy and return an action card from your discard pile to your hand and this structure takes 1 damage. activate: pay 5 energy to deal 5 damage to all objects within 1 tile of a tile within 2 tiles. activate: set an enemy robot's speed to 0. \"your opponent's adjacent robots can only attack this object.\". \"when this object is destroyed,\" destroy a random friendly robot. \"when this object is played,\" destroy is a robot. \"your opponent's adjacent robots can only attack this object.\".  \"this robot can move and attack immediately after it is played.\". deal damage to a robot equal to your energy. destroy all enemy robots and structures. activate: adjacent enemy robots get 0 speed until the end of the next turn. at the beginning of your turn, give a random robot +2 attack.  at the beginning of your turn, give a random robot +1 attack.  at the end of your turn destroy all robots with 9 or more attack. give all of your robots +2 attack. at the start of your turn, this robot gains 1 health then draw 1 card. when a robot attacks this robot, this robot gains 2 health. when this robot is destroyed, return this robot to your hand. \"this robot can't attack.\". \"your opponent's adjacent robots can only attack this object.\". activate: pay 10 energy then this robot gains 5 health, then, you win if this robot has more than 14 health. draw 8 cards then destroy all enemy robots. whenever this structure takes damage, deal 1 damage to that robot. \"when this object is destroyed,\" a random other friendly robot gets +3 attack  and +3 health and +1 speed. \"when this object is destroyed,\" lose 3 energy. \"when this object is played,\"  spawn a copy of this robot adjacent to this robot, then destroy this robot. activate: move all adjacent robots to a random tile. \"when this object is played,\" give all adjacent friendly robots +2 attack. at the start of your turn, lose 1 life.  activate: lose 3 life and your opponent takes control of this robot. spawn a 2/4/1 robot named \"turtlecard forger\" on each tile adjacent to your kernel. whenever a player draws a card, that player draws a card. activate:  create a 1/1/1 robot named \"swarmer\" in 2 empty adjacent tile. \"when this object is played,\" all enemy robots lose 1 attack then draw a card. \"when this object is destroyed,\" all enemy robots gain 1 attack then discard a random card. at the start of your turn, draw a card for each enemy. activate: spawn a 2/1/3 robot named \"tennis ball\" on a random tile. all action cards cost 99 energy. activate: set the speed of a friendly robot to 3 until the end of turn. your kernel gains 5 attack. give a structure 1 speed. \"this robot can move over other objects.\".  \"when this object is played,\" return a friendly adjacent robot to your hand. destroy target robot with 2 or less health. \"when this object is played,\" adjacent objects get +2 health. all cards in the opponent's hand cost 1 more. activate: destroy all enemy robots. remove all abilities from all objects. \"this robot can move and attack immediately after it is played.\".  at the start of your turn, your kernel takes 2 damage. destroy all robots. \"when this object is destroyed,\" return this object to its owner's hand. when you draw a card, this robot gains 1 attack and 1 health. whenever your opponent draws a card, your opponent's kernel takes 1 damage. \"when this object is played,\" discard a card and return a random card from your discard pile to your hand. choose a robot.    it can move and attack again.   discard a robot from your hand. \"when this object is played,\" destroy all robots. activate: this robot gains 1 speed and it loses 3 attack and it loses 3 health. activate: this robot loses 1 speed and it gains 2 attack and it gains 2 health. \"when this object is played,\" this robot gains +1 attack for each robot in play then this robot gains +1 speed for each robot in play. \"when this object is played,\" give 1 random friendly robot +1 attack and +1 health. activate: return this robot to your hand then gain 1 energy. \"when this object is played,\" give your robots +1 health . set a enemy robot's speed to 0. adjacent enemy robots can't attack. adjacent robots have +2 attack. activate: discard a card and create a copy of this structure on a random empty adjacent tile. reduce speed of all robots by 1. \"when this object is played,\" destroy all robots. when this robot is played destroy all of your opponent's structures. \"when this object is destroyed,\" each adjacent robot gains \"this robot can't attack.\" and -1 speed. \"your opponent's adjacent robots can only attack this object.\".  at the end of your turn, draw a card and lose 1 life for each adjacent enemy robot. activate: deal 3 damage then deal 3 damage then deal 3 damage then deal 1 damage to this structure. at the start of your turn, gain 10 energy. whenever a robot dies, deal 1 damage to every robot. adjacent enemy robots can't move. when this robot takes damage, spawn 2/1/1 robots named \"spectre\" on 3 random tiles. \"when this object is played,\" draw 2 cards and restore 5 health to your kernel. at the start of your turn, this robot loses 1 attack and this robot loses 1 health. \"when this object is played,\" deals 20 damage to enemy kernel. at the start of your turn, spawn a random card in a random adjacent tile then lose 3 energy. \"when this object is played,\" spawn a 0/5/1 robot named \"walking wall\" on a random tile adjacent to this robot. \"your opponent's adjacent robots can only attack this object.\".  whenever this robot takes damage, draw a card. activate: draw 3 cards. \"when this object is played,\" you draws 2 cards and your opponent draws 2 cards. at the end of your turn, deal 2 damage to each adjacent enemy then deal 1 damage to each enemy within 2 tiles. when this robot dies, your kernel gain 5 health. when this robot attacks it can move again. when this robot moves, it loses 1 speed. move all robots to a random empty tile. give all your robots +7 attack and +7 health. whenever this robot takes damage deal 4 damage to enemy kernel.  \"your opponent's adjacent robots can only attack this object.\". whenever this robot moves, destroy all adjacent enemy structures. at the start of your turn, deal 1 damage to a random enemy robot or structure or kernel then deal 1 damage to a random enemy robot or structure or kernel then deal 1 damage to a random enemy robot or structure or kernel. whenever a friendly robot dies, draw a card. at the end of your turn, destroy adjacent robot. move all objects to a random tile. \"this robot can move and attack immediately after it is played.\".    \"this robot can't attack.\".    \"when this object is played,\" this robot can move.    activate: give adjacent robot +1 speed and adjacent robot can move and attack again. when this robot attacks a robot, move it to adjacent tile. whenever you draw a card, lose 2 energy. replace \"1\" with \"2\", \"2\" with \"4\", \"3\" with \"6\", and \"4\" with \"8\" on all cards in your hand. give a robot 1 speed, 1 health, and 1 attack. at the end of each turn, restore 2 health to all adjacent robots. activate: swap the position of this robot and another robot or kernel. when this robot attacks a kernel, it gains +1 health. enemy robots adjacent to this structure can't move. \"when this object is played,\" destroy all robots. enemy robots adjacent to this robot have \"your opponent's adjacent robots can only attack this object.\".  if this robot is adjacent to an enemy robot, this robot has 0 speed. \"your opponent's adjacent robots can only attack this object.\". swap the position of a structure you control and a structure you control. activate: destroy this structure, then destroy all adjacent robots and structures. activate: destroy this structure. at the end of each turn, if this structure is not adjacent to your robots, destroy it. your robots have +1 speed. \"when this object is played,\" draw 3 cards and discard 2 random cards from your hand. activate: give an adjacent robot 2 health. \"when this object is played,\" deal 1 damage to all objects. double the attack of all robots.    halve the life (rounded up) of all robots. when you play an action, draw a card and gain 1 life.   at the end of your turn, destroy this structure. when you draw a card, this robot gains 1 attack. when you play an action, this robot gains 1 health and 1 speed. \"this robot can move over other objects.\". \"when this object is destroyed,\" reduce the attack of all adjacent robots to 1.  \"when this object is destroyed,\" reduce the speed of all adjacent robots to 1.  \"when this object is destroyed,\" reduce the health of all adjacent robots to 1. when this robot is played, destroy a robot. \"when this object is played,\" destroy your opponent's kernel. whenever you play an action, give this robot 1 attack. activate: deal 1 damage to an object 2 tiles away. activate: deal 2 damage to an object 3 tiles away. activate: pay 2 energy then draw a card. activate: discard a card then gain 1 energy. \"your opponent's adjacent robots can only attack this object.\". \"when this object is played,\" move this robot 6 tiles. when this robot is played, pay 1 energy to give it \"this robot can move and attack immediately after it is played.\" and give it \"this robot can move over other objects.\". when this robot dies, deal 100 damage to all adjacent objects. when there is an adjacent robot, destroy this robot. \"when this object is played,\" deal 2 damage to your opponent. deal 1 damage to all enemy robots. destroy all structures and robots.  restore 3 health to your kernel. deal 5 damage to the enemy kernel. set each robot's attack to its health.  set each robot's speed to its health. when this robot is destroyed, deal 2 damage to all enemy robots. all cards in your hand cost 2 less energy. discard your hand then draw 4 cards.  gain 5 energy. destroy a friendly robot . activate: draw a card and restore 3 health to your kernel. \"when this object is played,\" create a copy of this robot on a random adjacent tile, then create a copy of this robot on a random adjacent tile. activate: this robot gains +3 speed. set the attack of all robots equal to their health. \"when this object is played,\" destroy all robots. replace \"shutdown\" with \"activate\" on all cards in your hand. your adjacent robots have +2 health. \"this robot can move and attack immediately after it is played.\". whenever you play an action, give this robot 1 attack.   at the end of each turn set this robot's attack to 1. when this robot attacks, discard a random card. activate: swap the position of this object with a friendly object. move a friendly robot up to 3 tiles. 3 damage to that robot. give a robot 3 health. \"when this object is played,\" destroy all structures . when this robot attacks, set each adjacent robot's speed to 0. all robots have -1 speed.   at the end of your turn, deal 1 damage to all robots. a robot loses 2 attack.   destroy all robots with 1 or less attack. activate: destroy this robot and gain 2 energy. draw cards equal to your energy.  you lose all energy. \"when this object is played,\" it can activate. activate: destroy a friendly robot, then destroy it. \"when this object is destroyed,\" return it to your hand and draw 2 cards. destroy all of your opponent's robots. whenever you play an action, this robot gets +1 attack and -1 speed.  whenever you play a robot, this robot gets -1 attack and +1 speed. activate: destroy this robot and gain 2 energy and draw 2 cards. at the end of your turn, deal damage to your opponent equal to your energy, then you lose all energy. give a robot 3 health. all robots takes 3 damage. activate: spawn 2/2/2 robot named \"the unboxed\" in a random empty adjacent tile, then deal 2 damage to this structure. give all of your robots +2 attack until end of turn. at the end of your turn, spawn a 1/2/0 robot named \"sapling\" in a random empty adjacent tile and gain 1 life. deal damage to your opponent's kernel equal to half the enemy kernel's health. after it moves, it and all adjacent robots take 1 damage. when you play an action card, draw a card. deal 7 damage to a enemy robot. \"this robot can move and attack immediately after it is played.\".  \"when this object is played,\" all robots gain -2 speed.   \"when this object is destroyed,\" all robots gain 2 speed. activate: take control of a enemy robot . at the start of each turn, robots you control get 4 speed. at the end of your turn, adjacent friendly robots gain 1 attack. \"when this object is destroyed,\" deal 5 damage to adjacent enemy robots. \"when this object is played,\" create a copy of this structure on each tile adjacent to your kernel, then destroy this structure if it is not adjacent to your kernel. \"your opponent's adjacent robots can only attack this object.\".  \"this robot can't attack.\".  activate: return this robot to your hand. adjacent robots have 0 attack. whenever your opponent play a structure, deal 5 damage to enemy kernel then destroy that structure. \"when this object is destroyed,\" deal 1 damage to all adjacent objects. give your robots +3 attack. activate: pay 2 energy then this robot gains 2 attack. activate: spawn a 2/1/2 robot named \"crab bot\" on a random empty tile adjacent to your opponent's kernel. activate: move this robot to an empty tile. after it moves, adjacent enemy robots lose 2 attack. double the attack of all of your robots. activate: restore 2 health to this robot and set this robot's speed to 0 until end of turn. set all kernel's health to 1 . activate: your kernel gains activate: draw 1 card.   activate: your kernel gains activate: destroy all structures . at the end of each turn destroy your opponent's adjacent robots. at the beginning of your turn, return a random card from your discard pile to your hand. take control of a structure. deal damage to each kernel equal to your maximum energy. activate: pay 3 energy to destroy an adjacent robot. give all robots +2 health. activate: spawn a 5/4/3 robot named \"paper\" on a random tile. \"when this object is played,\" destroy all robots. \"when this object is played,\" return an action from your discard pile to your hand. whenever this robot takes damage, you gain 1 energy. deal 3 damage to an object. whenever this robot deals damage, you gain that much health and it gains 1 health.   this robot doesn't deal damage when attacked. activate: remove all abilities from enemy robots. activate: deal 2 damage to all adjacent objects and destroy this robot. activate:  spawn a copy of this robot on a random tile. whenever this robot takes damage, spawn a 2/2/2 robot named \"liquid slime\" on a random empty tile adjacent to this robot. when this robot takes damage, spawn a 2/1/3 robots named \"red bot\" on tiles adjacent to this robot. \"when this object is played,\" destroy all robots. whenever each robot within 2 tiles attacks, that robot takes 2 damage. give a robot +3 attack. \"when this object is destroyed,\" all adjacent robots and structures and kernels take 3 damage. whenever an adjacent enemy robot or structure takes damage, destroy it. when an enemy robot or structure is destroyed, this robot gains 1 attack. \"this robot can move over other objects.\". at the start of your turn, gain 1 energy for each adjacent enemy robot or structure or kernel. \"when this object is played,\" destroy all robots. when this robot attacks a robot, all friendly adjacent robots gain 1 health. at the beginning of your turn, draw a card and lose 3 life. at the start of your turn, gain 1 energy.  \"when this object is destroyed,\" deal 1 damage to your kernel. \"this robot can move over other objects.\".  after this robot moves, it can move again if there is an adjacent robot.  this robot can't attack if it moved. when this robot destroys an enemy robot, this robot can move again and this robot gains 1 attack and 1 health. replace \"win\" with \"lose\" on all cards. when a friendly robot is destroyed, this robot gains 1 health, then you win if this robot has 7 or more health. all friendly structures gain 2 health. your kernel gains 5 health. whenever you play a card, draw a card.  whenever your opponent plays a card, your opponent draws a card. when this robot dies return it to your hand. \"when this object is played,\" set your kernel's health to 7. at the end of your turn, set your kernel's health to 7. at the end of your opponent's turn, set your kernel's health to 7. deal 1 damage. enemy robots can't move adjacent to this robot. your adjacent robots have +2 health. at the beginning of each turn, this robot gains 1 attack. \"when this object is played,\" spawn a 2/1/2 robot named \"puppet\" on each adjacent tile. move all robots from your discard pile to your hand. when this robot takes damage, spawn a 2/1/2 robot named \"fork knight\" on a random tile adjacent to this robot. at the end of your turn, all adjacent friendly robots gain 1 attack. \"this robot can move over other objects.\".  \"your opponent's adjacent robots can only attack this object.\".  \"this robot can move and attack immediately after it is played.\".  whenever this robot moves draw a card. destroy a robot with 3 or less attack. all friendly robots within 2 tiles have +2 attack. at the end of your turn, adjacent friendly robots gain +1 attack. \"when this object is played,\" move this robot up to 3 spaces. \"when this object is played,\" this robot gains 2 speed. \"this robot can move and attack immediately after it is played.\". at the end of your turn, destroy this robot. at the end of each turn, this robot gains +2 health. move an object to a random tile up to 2 tiles away. spawn a 1/2/1 robot named \"reinforcements\" on each tile adjacent to your kernel. give enemy robots -1 attack. \"when this object is played,\" destroy all robots. \"when this object is played,\" destroy all robots. gain 4 energy. activate: pay 1 energy, then, destroy a robot with 2 or less attack. activate: a robot loses 1 attack. \"when this object is played,\" shuffle 3 random robots from your discard pile into your deck. when this robot moves it gains +1 health and +1 attack. give a friendly robot \"when this robot attacks, your kernel gains 3 health\". deal damage to your kernel equal to the number of robots in play.  deal damage to your kernel equal to the number of robots in play.  destroy all robots. activate: give a friendly robot +6 attack. activate: spawn a 3/2/3 robot named \"metroid\" on a random tile.  activate: deal 4 damage to a enemy robot. whenever a robot moves, it takes 1 damage. at the end of your turn, deal 1 damage to all adjacent objects. \"when this object is played,\" spawn a copy of this structure on adjacent tile and destroy this structure. activate: move this robot 1 tile then deal 1 damage to each adjacent enemy robot and structure then move this robot 1 tile then deal 2 damage to each adjacent enemy robot and structure. your adjacent robots have +100 attack. move an enemy robot up to 2 tiles. activate: draw a card then discard a card. \"your opponent's adjacent robots can only attack this object.\".  robots your opponent controls have +1 speed and \"this robot can move and attack immediately after it is played.\". give a robot 4 health. destroy a friendly robot.  objects adjacent to it take damage equal to its health. \"when this object is played,\" spawn a copy of this structure to a random adjacent tile and lose 1 life. when this robot attacks a robot, take control of the defending robot. \"when this object is destroyed,\" return this robot to its owner's hand. replace \"win\" with \"lose\" on all the cards in your opponent's hand. choose a friendly robot.  deal damage equal to half that robot's attack, rounded up, to all objects adjacent to that robot. at the beginning of your turn, deal 1 damage to all enemy objects within 3 tiles. \"this robot can't attack.\".    \"this robot can move and attack immediately after it is played.\". \"when this object is destroyed,\" gain 1 life. deal 3 damage to your opponent 's kernel. activate: discard a card and lose 2 life and take control of an enemy robot. \"this robot can move and attack immediately after it is played.\".  \"when this object is played,\" spawn a 2/5/2 robot named \"squarewave\" in a random empty adjacent tile. \"when this object is played,\" draw 2 card.   activate: deal damage to an enemy robots equal to this robot's attack. gain life equal to total health of your robots. destroy all of your robots. \"when this object is played,\" a random enemy robots loses 5 speed. draw a card for each robot your opponent controls.  your opponent draws a card for each robot you control. activate: this structure becomes a robot with 3 attack and 3 health and 1 speed. destroy all robots adjacent to your kernel. \"when this object is destroyed,\" deal 4 damage to all enemy robots.   \"when this object is destroyed,\" spawn 1/2/1 robots named \"woodleef baby\" on tiles adjacent to this robot. gain +1 life whenever this robot attacks. move your kernel to an empty tile. \"this robot can't attack.\". \"your opponent's adjacent robots can only attack this object.\". your kernel gains \"when this kernel takes damage, deal 1 damage to adjacent enemy robots\". whenever an enemy robot attacks, deal 1 damage to the enemy kernel. at the start of your turn, this robot gains 1 attack and each adjacent enemy robot or structure or kernel takes 1 damage. destroy a random robot. at the start of your turn, take control of all adjacent robots and structures. spawn a 3/5/0 robot named \"war camp\" on a random tile then deal 3 damage to all robots adjacent to it. whenever this robot takes damage, gain 2 energy. you win the game. discard a card.  draw 2 cards. each robot becomes a copy of a random other robot. robots with 2 power can't attack.  robots with 4 power can't attack.  robots with 6 power can't attack.  robots with 8 power can't attack. at the end of your turn, this robot takes 3 damage. when this robot destroys an enemy robot, this robot can move and attack again. whenever a robot attacks this robot gains 1 speed. all cards in your opponent's hand cost 3 more. whenever a enemy robot moves deal 2 damage to that robot. \"when this object is played,\" return a robot to its owner's hand. \"when this object is destroyed,\" draw 2 cards. \"your opponent's adjacent robots can only attack this object.\". all robots can move and attack again. gain 0 life. return a robot to its owner's hand. activate: spawn a copy of this robot on a random tile. activate: destroy this structure then gain 6 life. at the beginning of your turn, spawn a 1/2/1 robot named \"tiny bot\" on a random tile. draw a card for each your damaged robot or structure. deal 1 damage to an object. \"when this object is destroyed,\" remove all abilities from this robot and create a copy of this robot in a random empty adjacent tile. \"when this object is played,\"give your robots +1 speed. \"when this object is played,\" all enemy robots lose 1 speed. draw cards equal to your energy.  lose life equal to your energy.  lose energy equal to your energy. take control of a target enemy robot. destroy a robot you control to gain 5 life. take control of a robot. at the start of your turn, gain 1 energy if your kernel has 15 or less health. when an enemy robot is destroyed, this robot gains 1 attack and 1 health. activate: a robot gets +1 speed and -2 health. activate: a robot gets +1 speed and -2 attack. activate: a robot gets +2 attack and -2 health. activate: a robot gets +2 attack and -1 speed. activate: a robot gets +2 health and -2 attack. activate: a robot gets +2 health and -1 speed. deal 5 damage to all robots and enemy kernel. \"when this object is played,\" destroy all robots. when this robot is played, destroy all robots and gain 2 life. robots with more than 2 speed have 2 speed. when you play a robot, this robot gains 1 attack and 1 health. at the start of your turn, spawn a 0/3/0 robot named \"vine\" in a random adjacent tile, then, this robot gains 1 attack and 1 health. \"when this object is played,\"gain 1 energy. target enemy robot loses 2 attack.  if it has 0 attack, destroy it. at the start of your turn, set all friendly adjacent robots' speed to 3. activate:  destroy this robot and deal 3 damage to all adjacent robots and structures. \"when this object is destroyed,\" return it to your hand. activate: pay 2 energy to draw a card. activate: move your kernel 4 tiles. \"when this object is played,\" destroy a friendly robot and gain 2 energy. whenever your opponent plays a card, this robot gains 1 attack. \"this robot can move and attack immediately after it is played.\". whenever you play an action, give this robot 1 power and 1 health. give a robot +1 health. when this robot dies, spawn a 2/2/2 robot named \"spider bot\" on a random tile adjacent to this robot . all enemy robots and structures lose 1 health. all enemy robots lose 1 attack. \"when this object is played,\" give this object +4 speed until end of turn. \"this robot can move and attack immediately after it is played.\". whenever this robot takes damage, spawn a 1/1/0 robot named \"bee clown\" on each empty adjacent tile. when this robot takes damage, deal 2 damage to the enemy kernel. whenever your opponent draws a card, their kernel takes 1 damage. at the start of your turn, return a random card from your discard pile to your hand.        \"when this object is destroyed,\"  return it to your hand. all cards cost 1 more energy to play. robots you control get +1 speed. destroy enemy kernel. \"when this object is played,\" draw a card. when this robot has 5 or less health, it gains 10 health. when an enemy robot or structure is destroyed, draw a card. when a friendly robot or structure dies, your kernel gains 2 health. at the start of your turn, all adjacent friendly robots gain 1 attack. \"when this object is destroyed,\" destroy your kernel. give a friendly robot \"at the start of your turn, this robot gains 2 health\". \"when this object is played,\" this robot loses attack equal to half your kernel's health and this robot loses health equal to half your kernel's health. \"when this object is played,\" if your kernel has 10 or less health, this robot gains 1 speed, and if your kernel has 5 or less health, this robot gains 1 speed. when this robot attacks a kernel, it gains +1 attack. replace \"draw\" with \"discard\" on all cards in your opponent's hand. activate: all your robots can move and attack again. activate: deal 1 damage to a enemy robot. \"when this object is destroyed,\" spawn a 3/3/2 robot named \"fool\" on a random empty tile. activate: gain +5 life. end the turn. at the end of your turn deal 20 damage to enemy core. draw 2 cards.  lose 2 life. return a random robot from your discard pile to a random tile. return a random robot from your discard pile to a random tile. return a random robot from your discard pile to a random tile. return a random robot from your discard pile to a random tile. return a random robot from your discard pile to a random tile. robots you control have +1 speed. \"when this object is played,\" spawn a 1/1/1 robot named \"quotey\" with \"\"when this object is destroyed,\" gain 1 life\" on a random empty adjacent tile. at the end of your turn, spawn a 1/1/1 robot named \"gnatlet\" on a random empty tile adjacent to your opponent's kernel. destroy all robots with energy cost 3 or greater. \"when this object is played,\"deal 2 damage. \"when this object is played,\" this robot gains attack for every robot with 1 attack and 1 speed. move each robot to a random tile up to 2 tiles away from it. \"when this object is played,\"deal 1 damage to all enemy robots. destroy a friendly structure.   draw 2 cards.   shuffle a card from your discard pile to your deck. \"your opponent's adjacent robots can only attack this object.\". activate: give this robot 1 health. at the end of your turn, restore 3 health to your kernel. \"when this object is destroyed,\"gain 5 energy. \"when this object is destroyed,\" end the turn. destroy all robots and structures . at the end of your turn, this structure gains health equal to your energy, then you lose all energy. activate: you gain energy equal to this structure's health, then set this structure's health to 1 and you lose 1 energy. activate: deal 1 damage to target enemy robot and lose 1 energy. this robot can't attack.  when this robot takes damage, it gains that much health. draw 2 cards.   gain 3 energy.   your kernel takes 6 damage. activate: all robots gain +1 attack and -1 health until end of turn. deal 3 damage to your kernel.  draw 3 cards. activate: pay 6 energy to destroy this structure. \"when this object is destroyed,\" destroy all robots and structures. when your kernel is destroyed, you win. at the beginning of your turn, create a 5/5/3 robot named \"snorlax\" with the ability \" on a random tile. deal damage to a robot equal to the number of cards in your hand. draw 5 cards. deal 1 damage to an object adjacent to your robot. \"when this object is played,\" this structure can activate. activate: pay 1 energy to spawn a copy of this structure on an adjacent tile. at the end of your turn, remove all abilities from this object. when you draw a card, your kernel gains 1 health. whenever this robot moves, deal 1 damage to a random robot. activate: destroy a structure. activate: give a enemy robot -1 attack. give a friendly robot \"at the start of your turn, this robot gains 3 attack and 2 speed and lose 1 health\". activate: lose 1 life to draw a card. remove all abilities from all robots. deal 2 damage to all enemy objects. \"your opponent's adjacent robots can only attack this object.\".   \"this robot can't attack.\".   \"when this object is played,\" draw a card. activate: restore 1 health to friendly adjacent structures and restore 1 health to friendly adjacent robots. move a robot up to 9 tiles. when a friendly robot is destroyed, this robot gains 1 health and 1 attack. at the start of your turn, if this robot is adjacent to an enemy robot, destroy this robot. when you draw a card, this robot gains 1 attack. when this robot attacks, draw a card. at the start of your turn, this robot gains 1 health. \"your opponent's adjacent robots can only attack this object.\". when this robot dies, your opponent spawns a copy of this robot on a random empty tile. when the enemy kernel is destroyed, your opponent wins. at the start of your turn, all enemy robots and structures take 2 damage and the enemy kernel takes 3 damage. draw 2 cards. whenever this robot attacks, it gains health equal to its attack. \"when this object is played,\" this robot gains +5 speed. \"this robot can move over other objects.\". \"this robot can move over other objects.\". whenever you draw a card, gain 2 life. \"your opponent's adjacent robots can only attack this object.\". adjacent enemy robots can't attack. move all enemy robots to a random empty tile. gain 6 energy for each robot in play. \"this robot can move over other objects.\". deal 3 damage to each of your opponent's robots. at the start of your turn, all adjacent enemy robots lose 1 attack and remove all abilities of adjacent enemy objects. deal 1 damage to yourself.   deal 1 damage to your opponent.  deal 1 damage to all structures.  restore 1 health to all robots. move a kernel 1 space. \"when this object is played,\" this robot gains 1 attack for each card in your discard pile. all robots gain \"\"when this object is destroyed,\" deal 1 damage to each robot or structure adjacent to this robot\". \"when this object is destroyed,\" return this object to its owner's hand. \"when this object is played,\" create a 0/1/2 robot named \"spider\" on each empty adjacent tile. after this robot attacks, the defending robot gains 1 health and \"at the start of your turn, this robot takes 2 damage\". whenever a player draws a card, discard a random card from their hand, and shuffle a random card from their discard pile to their deck, and they draw a card. return all your discard pile to your hand. give an robot 2 attack and 1 speed. \"this robot can move and attack immediately after it is played.\". when this robot is played set the attack of this robot of your energy. destroy this robot at the end of your turn. \"when this object is played,\" gain 1 life. when this robot attacks, take control of a random enemy structure. activate: give your robots +1 health. activate: destroy this robot and restore health of all adjacent, friendly, robots and create a copy of this robot on an empty, adjacent tile. \"when this object is played,\" spawn a 2/1/2 robot named \"right bot\" on a random tile adjacent to this robot. adjacent robots have -1 speed. set the health of all kernels to 1.  end your turn. \"when this object is played,\" destroy all robots. each player draws 3 cards. activate: lose 4 life and draw a card. activate: discard a card and gain 3 life. activate: you gain 1 energy. both players draw 2 cards. destroy an enemy structure. \"when this object is played,\" deal 7 damage to an object. give enemy robot \"this robot can't attack.\". \"when this object is played,\" destroy all robots. \"when this object is destroyed,\" shuffle 5 random cards from your discard pile into your deck and remove all abilities from this robot. activate: deal 5 damage to all objects adjacent to a tile. give all enemy robots and structures \"at the start of your turn, this robot takes 1 damage\". replace \"gain\" with \"lose\" and \"lose\" with \"gain\" on all cards in your opponent's hand. replace \"+\" with \"-\" and \"-\" with \"+\" on all cards in your opponent's hand. move each kernel and structure to a random tile. at the beginning of each turn this robot gains 99 health. all enemy robots within 2 tiles can't move. \"when this object is played,\" destroy all robots. \"when this object is played,\" draw 1 card.                                                           \"when this object is destroyed,\" give your robots +3 health. at the beginning of each of your turns, this robot gains -2 attack. return a enemy robot to your hand. destroy an enemy robot.   draw a card. at the start of your turn, give this object 1 health. when this object has 15 or more health, you win the game. when you play a robot, that robot gains 1 speed and that robot gains \"\"this robot can move over other objects.\"\". \"this robot can move and attack immediately after it is played.\". destroy a random enemy robot and a random enemy structure. shuffle all actions from your discard pile into your deck. deal 5 damage to a robot.  restore 4 health to yourself. at the beginning of your turn, spawn a 2/1/0 robot named \"potato\" on a random empty tile adjacent to a random enemy robot. when your kernel takes damage, destroy this robot and discard all cards in your hand. all robots can move and attack again. whenever this robot attacks a robot, destroy that robot instead. activate: pay 8 energy, and deal 8 damage to a enemy robot . \"when this object is played,\" destroy all robots. \"when this object is played,\" destroy all adjacent robots. \"when this object is played,\" destroy all robots. \"this robot can move over other objects.\".   whenever this robot moves it takes 2 damage and all adjacent robots take 1 damage and it can move again. when a friendly robot dies, spawn a 1/1/1 robot named \"spooky skeleton\" on a random tile adjacent to your kernel. \"this robot can move over other objects.\".   \"when this object is destroyed,\" take control of all adjacent enemy robots. activate: deal 4 damage to a robot 4 tiles away. activate: deal 4 damage to a robot 3 tiles away. activate: deal 4 damage to a robot 2 tiles away and deal 1 damage to this robot. \"when this object is destroyed,\" deal 2 damage to adjacent robots. spawn a copy of a friendly robot on an empty tile.  give your robot +1 attack and +1 health. defend. at the start of each turn, this robot and adjacent friendly robots gain 1 attack and 1 speed and lose 1 health. remove all abilities from an enemy robot and all objects adjacent to it. \"when this object is played,\" deal 2 damage to all enemy robots with 3 health or more. all structures have \"when an adjacent robot moves, deal 1 damage to all structure within 2 tiles, then deal its cost damage to it\". whenever the enemy kernel takes damage, this structure gains 1 attack.  activate: deal damage equal to this structure's attack to the enemy kernel and set this structure's attack to 0. when a robot is played, destroy that robot. at the end of each of your turns, deal 1 damage to this structure. \"when this object is destroyed,\" return a random robot from your discard pile to a random tile, then your opponent returns a random robot from their discard pile to a random tile. choose a robot.  that robot gains 4 health.  deal 4 damage to all robots. \"this robot can't attack.\".  whenever this robot takes damage, you gain that much health.  at the start of each turn, this robot gains 1 health. all robots have +1 speed. activate: gain 4 energy. gain 1 energy for every friendly robot in play.  destroy all friendly robots in play. activate: remove all abilities from enemy robots. spawn 1/1/1 robots named \"bug bot\" in 3 random empty tile. activate: pay 1 energy to remove all abilities from a robot. activate: adjacent enemy robots lose 1 speed. activate: pay 1 energy to swap the position of this robot and another robot. when this robot moves, you gain 1 energy. \"when this object is destroyed,\" return this robot to its owner's hand. at the beginning of your turn, reduce energy cost of all friendly robots equal to the number of all robots. \"when this object is destroyed,\" gain 3 energy.  return it to your hand. deal 1 damage to all robots. deal 1 damage to all structures. deal 1 damage to enemy. deal 1 damage to yourself. \"this robot can move and attack immediately after it is played.\" . activate: choose an adjacent robot and deal 3 damage to that robot. when a robot moves, it gains 1 attack until the end of the turn. \"when this object is played,\" return an enemy structure to your hand. give your robots +1 attack. activate: discard 2 random cards then draw 2 cards. activate: pay 6 energy to draw 5 cards . enemy robots within 2 tiles can't attack unless there is an adjacent enemy robot. at the start of your turn, all adjacent robots gain 2 health unless there is an adjacent enemy robot. all of your robots have +2 health. at the start of your turn, gain 1 energy. your other robots have +1 attack.  your other robots have +1 health.  your other robots have +1 speed. all robots have +1 speed.     \"when this object is played,\"  if your discard pile has 4 or more cards, all robots gain +2 attack. remove all abilities from all enemy robots. replace \"startup\" with \"shutdown\" on all cards in your opponent's hand. \"this robot can't attack.\".  \"this robot can move and attack immediately after it is played.\". \"this robot can move over other objects.\". \"when this object is played,\" discard a card to give this robot +1 speed. activate: discard a random card to draw a card. \"when this object is destroyed,\" discard a random card and give adjacent robots \"this robot can move over other objects.\". \"when this object is played,\" destroy all robots. at the beginning of each turn, draw 2 cards. \"when this object is played,\" all robots and structures become a copy of this robot. give a robot +3 speed. \"when this object is played,\" destroy target all your other robot. activate: pay 1 energy and this robot gains 1 attack. when this robot moves, it gains 5 health.  at the end of your turn, this robot takes 5 damage. \"when this object is played,\" spawn a copy of this structure on every adjacent tile. when this structure is destroyed, spawn a copy of this structure on every adjacent tile. \"this robot can move and attack immediately after it is played.\".  \"when this object is played,\" this robot gains 2 speed until the end of the turn. \"when this object is destroyed,\" restore 5 health to your opponent's kernel. you win the game. move a random robot to a random tile up to 2 tiles away. move a random robot to a random tile up to 1 tile away. activate: deal 4 damage. activate: deal 1 damage then deal 1 damage then deal 1 damage. activate: deal 7 damage then destroy this robot. move a friendly robot up to 3 tiles. \"this robot can move and attack immediately after it is played.\". whenever you play an action, this robot gains +1 attack and +1 health until the end of the next turn. take control of 2 random robots your opponent controls. \"when this object is destroyed,\" give all your robots 2 speed. \"when this object is played,\"  gain 10 health  . when this robot takes damage, gain 1 energy. spawn a 0/1/0 robot named \"defense grid\" on every tile adjacent to each robot and kernel you control. at the start of every turn, this robot gains 1000 health.  at the start of every turn, this robot loses 1000 attack. when this robot dies, draw 4 cards. \"when this object is played,\" move all robots to a random empty tiles. when this robot attacks, spawn a copy of this robot on each adjacent tile. draw 1 card for every all objects. restore 4 health to yourself. deal 1 damage equal to the total power of all robots random objects. give all robots -2 attack. set attack of a robot to 0 until end of turn. take control of enemy robot. \"when this object is played,\" all friendly robots gain 1 speed.    \"when this object is destroyed,\" all friendly robots lose 2 speed. \"when this object is destroyed,\"gain 3 energy. when this robot deals damage to a object, that object gains that much life.  when this robot takes damage, it gains that much life. each player shuffles all cards from their hand into their deck.  each player draws 2 cards. \"when this object is played,\" the enemy kernel gains \"when this kernel takes damage, it gains that much life\". \"when this object is played,\" draw a card for each friendly robot with 2 attack or less. \"when this object is played,\" destroy all robots. \"when this object is played,\" shuffle all cards from your opponent's discard pile into your deck. destroy all of your opponent's robots. remove all abilities from a robot or structure then it loses 99 speed and it loses 99 attack. at the start of your turn, gain 1 energy. at the start of the opponent's turn, they lose 1 energy. at the start of each turn, give this robot +1 attack and +1 health. \"when this object is played,\"deal 2 damage to enemy robots. whenever a robot deals damage to the enemy kernel, draw a card. at the end of your turn, deal 1 damage to each player. when you play a robot, this structure becomes a copy of that robot. activate: move an adjacent robot 1 tile. when this robot attacks, deal 2 damage to enemy robots or structures or kernels adjacent to the enemy. your kernel gains activate: deal 1 damage to a enemy robot. activate: draw 10 cards for every card in your hand, then draw 10 cards for every card in your hand, then draw 10 cards for every card in your hand, then draw 10 cards for every card in your hand. when a robot attacks this robot, your kernel gains 3 health. \"this robot can't attack.\". \"your opponent's adjacent robots can only attack this object.\". \"when this object is played,\" destroy all robots. your opponent discards 2 random cards. whenever a player draws a card, they lose 1 energy. deal 10 damage to a enemy robot and structures . when this robot dies destroy 2 random enemy robots. when this robot dies gain 2 energy. all cards cost 1 more energy. at the start of your turn, set the attack of this robot to this robot's health then set the speed of this robot to half this robot's health. this robot only deals damage when attacking. at the beginning of each turn each player draws 2 cards. discard all cards in your hand, then draw cards equal to the number of cards in your opponent's hand. when this robot dies, gain 2 energy. deal 2 damage to 2 random enemy robots. whenever a friendly robot dies, return it to your hand. give a robot +1 speed. at the end of each turn, draw a card. activate: deal 3 damage to all enemy objects and deal 3 damage to your kernel. give your opponent's kernel \"activate: destroy each kernel\". destroy all robots and structures. replace \"\"this robot can move and attack immediately after it is played.\"\" with \"\"this robot can move over other objects.\"\" on all robot cards in your hand. when your kernel takes damage, your kernel gains health equal to this structure's health. at the start of your turn, this structure takes 1 damage. destroy all robots. activate: deal 4 damage to a enemy robot. \"your opponent's adjacent robots can only attack this object.\" at the beginning of each of your turns, this robot gains 1 attack. \"when this object is played,\" gain 2 energy and draw a card. \"when this object is destroyed,\" your opponent gains 2 energy, then your opponent draws a card \"this robot can move and attack immediately after it is played.\". \"when this object is played,\" lose 2 life. activate: give a friendly robot +2 attack, then deal 3 damage to your kernel. activate: restore 1 health to all adjacent friendly robots at the start of your turn, a random friendly robot gains 1 attack and a random enemy robot gains 1 attack. \"your opponent's adjacent robots can only attack this object.\" at the start of your turn, gain 1 energy and lose 1 life. activate: pay 1 energy and each player draws a card all robots have +1 speed. at the end of your turn, deal 1 damage to each player. when this robot is destroyed, take control of all adjacent robots. at the end of each turn, each kernel gains 1 health \"this robot can move and attack immediately after it is played.\". robots you play cost 1 less energy. activate: discard a card, then draw a card. \"when this object is played,\" deal 2 damage to your opponent. \"when this object is played,\" give adjacent robots 3 health \"this robot can't attack.\",. \"your opponent's adjacent robots can only attack this object.\" activate: pay 3 energy and discard a card, then this robot gains 1 attack and 1 health. \"this robot can't attack.\",. \"this robot can move and attack immediately after it is played.\" at the start of your turn, draw a card and lose 2 life. \"this robot can move over other objects.\" when this robot attacks, it deals damage to all adjacent robots instead. at the start of your turn, pay 1 energy and this robot gains 1 health. \"this robot can't attack.\",. \"your opponent's adjacent robots can only attack this object.\" at the end of each turn, each robot takes 1 damage. when this robot is played, deal 4 damage. \"when this object is played,\" all of your other robots can move again. adjacent robots have +1 attack gain energy equal to the total power of robots you control. destroy all robots you control. gain 2 energy. draw 2 cards. discard a robot card. gain life equal to its health. move a robot up to 2 spaces. destroy a structure. give a robot \"when this robot attacks, restore 3 health to your kernel\" double a robot's health reduce the cost of all cards in your hand by 1. set the attack of all robots equal to their health deal 1 damage to everything adjacent to a tile. give all robots you control +2 attack. deal 3 damage to a robot. deal 4 damage to your opponent. set all stats of all robots in play to 3. draw cards equal to the number of robots you control. destroy all robots that have less than 2 speed. remove all abilities from all robots set the attack and speed of all robots to 0. give all robots \"activate: destroy this robot\". give your robots +2 attack and +2 health. deal 1 damage to each enemy object your adjacent robots have +1 health. at the start of each player's turn, that player gains 1 energy if they control an adjacent robot. all robots have \"this robot can move over other objects.\". \"when this object is played,\" give all friendly robots +1 speed and -1 health. all robots have \"whenever this robot attacks a kernel, draw a card\". whenever a robot is destroyed in combat, deal 1 damage to its controller. at the start of your turn, this structure gains 1 health. activate: destroy all robots with cost equal to this structure's health. activate: destroy this structure. \"when this object is destroyed,\" restore all adjacent robots' health when you play a robot, this structure becomes a copy of that robot. activate: destroy this structure. \"when this object is destroyed,\" deal 2 damage to all objects within 2 spaces."
  val cardTextCorpus_07_16_23_tokenized: String = simpleTokenizer(cardTextCorpus_07_16_23).mkString(" ")

  /** Map of all terms in [[lexicon]] and their corresponding counts in [[cardTextCorpus_07_16_23]] . */
  val mapOfTermsToUsages: Map[String, Int] = scala.collection.immutable.ListMap(
    listOfTerms
      .map((term) => term -> StringUtils.countMatches(cardTextCorpus_07_16_23_tokenized, term))
      .sortBy(_._2)
      .reverse
      : _*
  )

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
