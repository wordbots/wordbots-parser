package wordbots

import com.workday.montague.ccg._
import com.workday.montague.parser.{IntegerMatcher, ParserDict}
import com.workday.montague.semantics.{Form, Ignored, SemanticState, _}

/**
  * Created by alex on 2/28/17.
  */
object Lexicon {
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

  val lexicon =  ParserDict[CcgCat]() +
    (Seq("a", "an") -> Seq(
      (NP/N, λ {o: ObjectType => Choose(ObjectsInPlay(o))}),  // e.g. "a robot"
      (NP/NP, λ {c: Collection => Choose(c)}),  // e.g. "a robot you control"
      (Num, Form(Scalar(1)): SemanticState)  // e.g. "(draw) a card"
    )) +
    ("a tile" -> (NP, Form(Choose(AllTiles)): SemanticState)) +
    ("adjacent" -> (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(AdjacentTo(ThisRobot)))})) +
    ("adjacent to" -> ((NP/NP)\N, λ {o: ObjectType => λ {t: TargetObject => ObjectsMatchingConditions(o, Seq(AdjacentTo(t)))}})) +
    ("after attacking" -> (S\S, λ {a: Action => At(AfterAttack(ThisRobot), a)})) +
    (Seq("all", "each") -> Seq(
      (NP/N, λ {o: ObjectType => All(ObjectsInPlay(o))}),
      (NP/NP, λ {c: Collection => All(c)}),
      (NP/PP, λ {c: Collection => All(c)})
    )) +
    ("all" /?/ Seq("attributes", "stats") -> (N, Form(AllAttributes): SemanticState)) +
    ("and" -> (((S/PP)/V)\V, λ {a1: CurriedAction => λ {a2: CurriedAction => λ {t: TargetObject => And(a1.action(t), a2.action(t))}}})) +
    ("at" -> ((S/S)/NP, λ {t: Trigger => λ {a: Action => At(t, a)}})) +
    ("attacks" -> Seq(
      (S\NP, λ {c: Choose => AfterAttack(All(c.collection))}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterAttack(t)})
    )) +
    ("beginning" -> (NP/PP, λ {turn: Turn => BeginningOfTurn(turn.player)})) +
    ("by" -> (PP/Num, identity)) +
    (Seq("can move again", "gains a second move action") -> (S\NP, λ {t: TargetObject => CanMoveAgain(t)})) +
    ("can't attack" -> (S\NP, λ {t: TargetObject => ApplyEffect(t, CannotAttack)})) +
    ("can't be changed" -> (S\NP, λ {t: TargetAttribute => FreezeAttribute(t.target, t.attr)})) +
    (("card".s :+ "a card") -> Seq(
      (N, Form(AnyCard): SemanticState),
      (NP\Num, λ {num: Number => Cards(num)}),
      (NP/Adj, λ {num: Number => Cards(num)}),
      (NP, Form(CardsInHand(Self)): SemanticState),
      (NP/PP, λ {hand: Hand => CardsInHand(hand.player)}),
      (NP\N, λ {cardType: CardType => CardsInHand(Self, cardType)}),
      ((NP/PP)\N, λ {cardType: CardType => λ {hand: Hand => CardsInHand(hand.player, cardType)}})
    )) +
    ("control".s -> ((NP\N)\NP, λ {p: TargetPlayer => λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(ControlledBy(p)))}})) +
    ("cost" -> Seq(
      (N, Form(Cost): SemanticState),
      ((S\NP)/Adv, λ {o: Operation => λ {cp: CardPlay => AttributeAdjustment(All(CardsInHand(cp.player, cp.cardType)), Cost, o)}})
    )) +
    ("damage" -> Seq(
      ((S/PP)\Num, λ {amount: Number => λ {t: Target => DealDamage(t, amount)}}),
      ((S\NP)\Num, λ {amount: Number => λ {t: Target => DealDamage(t, amount)}}),
      ((S/Adj)/PP, λ {t: Target => λ {amount: Number => DealDamage(t, amount)}}),
      ((S\NP)/Adj, λ {amount: Number => λ {t: Target => DealDamage(t, amount)}}),
      (S/PP, λ {t: Target => DealDamage(t, AttributeValue(ThisRobot, Attack))}),  // (by default, a robot deals damage equal to its power)
      (S\Num, λ {amount: Number => DealDamage(Choose(ObjectsInPlay(AllObjects)), amount)})  // (if no target is given, any target can be chosen)
    )) +
    (Seq("deal", "it deals", "takes") -> (X|X, identity)) +  // e.g. deals X damage, takes X damage
    ("destroy" -> (S/NP, λ {t: TargetObject => Destroy(t)})) +
    ("destroyed" -> Seq(
      (S\NP, λ {c: Choose => AfterDestroyed(All(c.collection))}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterDestroyed(t)})
    )) +
    ("draw" -> (S/NP, λ {c: Cards => Draw(Self, c.num)})) +
    ("discard" -> (S/NP, λ {t: TargetObject => Discard(t)})) +
    ("double" -> Seq(
      ((S/PP)/N, λ {a: Attribute => λ {t: TargetObject => ModifyAttribute(t, a, Multiply(Scalar(2)))}}),
      (V/N, λ {a: Attribute => CurriedAction({t: TargetObject => ModifyAttribute(t, a, Multiply(Scalar(2)))})})
    )) +
    (Seq("each", "every") -> Seq(
      (Adj, Form(AllPlayers): SemanticState),  // e.g. "each turn"
      (NP/PP, identity)  // e.g. "each of (your turns)"
    )) +
    ("end" -> (NP/PP, λ {turn: Turn => EndOfTurn(turn.player)})) +
    ("energy" -> Seq(
      (NP|Num, λ {amount: Number => Energy(amount)}),
      (NP/Adj, λ {amount: Number => Energy(amount)}),
      (S\S, λ {aa: AttributeAdjustment => AttributeAdjustment(aa.target, Cost, aa.operation)})  // "X costs Y more" == "X costs Y more energy"
    )) +
    ("equal" -> (Adj/PP, identity)) +
    ("for each" -> (Adj/NP, λ {c: Collection => Count(c)})) +
    ("everything" -> (N, Form(AllObjects): SemanticState)) +
    ("everything adjacent to" -> (NP/NP, λ {t: TargetObject => All(ObjectsMatchingConditions(AllObjects, Seq(AdjacentTo(t))))})) +
    ("gain" -> Seq(
      (S/NP, λ {e: Energy => ModifyEnergy(Self, Plus(e.amount))}),
      (S/NP, λ {l: Life => ModifyAttribute(All(ObjectsMatchingConditions(Kernel, Seq(ControlledBy(Self)))), Health, Plus(l.amount))})
    )) +
    (Seq("gain", "gains") -> (((S\NP)/N)/Num, λ {num: Number => λ {a: Attribute => λ {t: TargetObject => ModifyAttribute(t, a, Plus(num))}}})) +
    ("give" -> (((S/N)/Adj)/NP, λ {t: TargetObject => λ {o: Operation => λ {a: Attribute => ModifyAttribute(t, a, o)}}})) +
    ("hand" -> (NP\Adj, λ {p: TargetPlayer => Hand(p)})) +
    ("halve" -> Seq(
      (((S/PP)/Adv)/N, λ {a: Attribute => λ {r: Rounding => λ {t: TargetObject => ModifyAttribute(t, a, Divide(Scalar(2), r))}}}),
      ((V/Adv)/N, λ {a: Attribute => λ {r: Rounding => CurriedAction({t: TargetObject => ModifyAttribute(t, a, Divide(Scalar(2), r))})}})
    )) +
    (Seq("has", "have") -> Seq(
      ((S/N)/Adj, λ {c: Comparison => λ {a: Attribute => AttributeComparison(a, c)}}),
      (((S\NP)/N)/Adj, λ {o: Operation => λ {a: Attribute => λ {t: TargetObject => AttributeAdjustment(t, a, o)}}})
    )) +
    (Seq("health", "life") -> Seq(
      (N, Form(Health): SemanticState),
      (NP|Num, λ {amount: Number => Life(amount)}),
      (NP/Adj, λ {amount: Number => Life(amount)})
    )) +
    (Seq("in", "of") -> (PP/NP, identity)) +
    ("in combat" -> (S\S, λ {t: AfterDestroyed => AfterDestroyed(t.target, Combat)})) +
    (Seq("in play", "on the board") -> (NP\N, λ {o: ObjectType => ObjectsInPlay(o)})) +
    ("is" -> (X|X, identity)) +
    ("it" -> (NP, Form(It): SemanticState)) +
    ("its" -> (Num/N, λ {a: Attribute => AttributeValue(It, a)})) +
    ("its controller" -> (NP, Form(ControllerOf(It)): SemanticState)) +
    ("kernel".s -> (N, Form(Kernel): SemanticState)) +
    ("less" -> (Adv\Num, λ {num: Number => Minus(num)})) +
    ("less than" -> (Adj/Num, λ {num: Number => LessThan(num)})) +
    ("more" -> (Adv\Num, λ {num: Number => Plus(num)})) +
    ("more than" -> (Adj/Num, λ {num: Number => GreaterThan(num)})) +
    ("must" -> (X/X, identity)) +
    ("number" -> Seq(
      (Num/PP, λ {c: Collection => Count(c)}),
      (Num/PP, λ {a: All => Count(a.collection)})
    )) +
    ("object".s -> (N, Form(AllObjects): SemanticState)) +
    ("or less" -> (Adj\Num, λ {num: Number => LessThanOrEqualTo(num)})) +
    ("or more" -> (Adj\Num, λ {num: Number => GreaterThanOrEqualTo(num)})) +
    ("play".s -> ((NP\N)\NP, λ {t: TargetPlayer => λ {c: CardType => CardPlay(t, c)}})) +
    (Seq("played", "comes into play", "enters the board") -> Seq(
      (S\NP, λ {c: Choose => AfterPlayed(All(c.collection))}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterPlayed(t)})
    )) +
    (Seq("power", "attack") -> (N, Form(Attack): SemanticState)) +
    ("reduce" -> (((S/PP)/PP)/N, λ {a: Attribute => λ {t: TargetObject => λ {num: Number => ModifyAttribute(t, a, Minus(num))}}})) +
    (("robot".s ++ "creature".s) -> (N, Form(Robot): SemanticState)) +
    ("(rounded down)" -> (Adv, Form(RoundedDown): SemanticState)) +
    ("(rounded up)" -> (Adv, Form(RoundedUp): SemanticState)) +
    ("set" -> (((S/PP)/PP)/N, λ {a: Attribute => λ {t: TargetObject => λ {num: Number => SetAttribute(t, a, num)}}})) +
    ("speed" -> (N, Form(Speed): SemanticState)) +
    ("take control" -> (S/PP, λ {t: TargetObject => TakeControl(Self, t)})) +
    ("takes damage" -> Seq(
      (S\NP, λ {c: Choose => AfterDamageReceived(All(c.collection))}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterDamageReceived(t)})
    )) +
    ("to" -> Seq(
      (PP/NP, identity),
      (PP/Num, identity)
    )) +
    ("that" -> ((NP\N)/S, λ {c: Condition => λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(c))}})) +
    ("the" -> (X/X, identity)) +
    ("this" / Seq("robot", "creature") -> (NP, Form(ThisRobot): SemanticState)) +
    ("this" / Seq("robot's", "creature's") -> (NP/N, λ {a: Attribute => TargetAttribute(ThisRobot, a)})) +
    ("total" -> Seq(
      ((Num/PP)/N, λ {a: Attribute => λ {c: Collection => AttributeSum(c, a)}}),
      ((Num/PP)/N, λ {a: Attribute => λ {all: All => AttributeSum(all.collection, a)}})
    )) +
    ("turn".s -> (NP\Adj, λ {p: TargetPlayer => Turn(p)})) +
    (Seq("when", "whenever") -> ((S/S)/S, λ {t: Trigger => λ {a: Action => At(t, a)}})) +
    (Seq("you", "yourself") -> (NP, Form(Self): SemanticState)) +
    ("your" -> Seq(
      (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(ControlledBy(Self)))}),
      (NP/NP, λ {c: ObjectsMatchingConditions => All(ObjectsMatchingConditions(c.objectType, c.conditions :+ ControlledBy(Self)))}),
      (Adj, Form(Self): SemanticState)
    )) +
    ("your opponent" -> (NP, Form(Opponent): SemanticState)) +
    (IntegerMatcher -> (Num, {i: Int => Form(Scalar(i))})) +
    (PrefixedIntegerMatcher("+") -> (Adj, {i: Int => Form(Plus(Scalar(i)))})) +
    (PrefixedIntegerMatcher("-") -> (Adj, {i: Int => Form(Minus(Scalar(i)))}))

  val syntaxLexicon = ParserDict[CcgCat](
    lexicon.map.mapValues(definitions => definitions.map {case (syn, sem) => (syn, Ignored(""))}),
    lexicon.funcs.map(func => {str: String => func(str).map {case (syn, sem) => (syn, Ignored(""))}}),
    lexicon.fallbacks.map(func => {str: String => func(str).map {case (syn, sem) => (syn, Ignored(""))}})
  )
}
