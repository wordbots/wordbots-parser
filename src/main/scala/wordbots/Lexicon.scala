package wordbots

import com.workday.montague.ccg._
import com.workday.montague.parser.ParserDict
import com.workday.montague.semantics._
import com.workday.montague.semantics.FunctionReaderMacro.λ

object Fail {
  def apply(str: String): Unit = throw new ClassCastException(str)
}

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
      (N/N, identity),
      (NP/N, λ {o: ObjectType => Choose(ObjectsInPlay(o))}),  // e.g. "a robot"
      (NP/NP, λ {c: Collection => Choose(c)}),  // e.g. "a robot you control"
      (Num, Form(Scalar(1)): SemanticState)  // e.g. "(draw) a card"
    )) +
    ("a player" -> (NP, Form(Choose(ObjectsInPlay(Kernel))): SemanticState)) +
    ("a tile" -> (NP, Form(Choose(AllTiles)): SemanticState)) +
    ("activate:" -> (S/S, λ {a: Action => ActivatedAbility(a)})) +
    ("adjacent" -> (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(AdjacentTo(ThisObject)))})) +
    ("adjacent to" -> ((NP/NP)\N, λ {o: ObjectType => λ {t: TargetObject => ObjectsMatchingConditions(o, Seq(AdjacentTo(t)))}})) +
    ("after attacking" -> (S\S, λ {a: Action => TriggeredAbility(AfterAttack(ThisObject, AllObjects), a)})) +
    (Seq("all", "each", "every") -> Seq( // Also see Seq("each", "every") below for definitions that DON'T apply to "all".
      (NP/N, λ {o: ObjectType => ObjectsInPlay(o)}),
      (NP/NP, λ {c: CardCollection => All(c)}),
      (NP/NP, λ {c: Collection => c}),
      (NP/PP, λ {c: Collection => c})
    )) +
    ("all" /?/ Seq("attributes", "stats") -> (N, Form(AllAttributes): SemanticState)) +
    ("and" -> Seq(
      (conj, λ {b: Any => λ {a: Any => Seq(a, b)}}),
      (conj, λ {b: Any => λ {a: Seq[Any] => a :+ b}}),
      ((S/S)\NP, λ {a: Any => λ {b: Any => (a, b)}})
    )) +
    (Seq("and", "then") -> ((S/S)\S, λ {a1: Action => λ {a2: Action => And(a1, a2)}})) +
    ("at" -> ((S/S)/NP, λ {t: Trigger => λ {a: Action => TriggeredAbility(t, a)}})) +
    ("attacks" -> Seq(
      (S\NP, λ {c: Choose => AfterAttack(All(c.collection), AllObjects)}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterAttack(t, AllObjects)}),
      ((S\NP)/N, λ {o: ObjectType => λ {t: TargetObject => AfterAttack(t, o)}})
    )) +
    ("attacked last turn" -> (S, Form(HasProperty(AttackedLastTurn)): SemanticState)) +
    ("attacked this turn" -> (S, Form(HasProperty(AttackedThisTurn)): SemanticState)) +
    (Seq("beginning", "start") -> (NP/PP, λ {turn: Turn => BeginningOfTurn(turn.player)})) +
    ("by" -> (PP/Num, identity)) +
    (Seq("can move", "can move and attack", "can move again", "gains a second move action") -> (S\NP, λ {t: TargetObject => CanMoveAgain(t)})) +
    ("can move over other objects" -> (S\NP, λ {t: TargetObject => ApplyEffect(t, CanMoveOverObjects)})) +
    ("can only attack" -> ((S\NP)/NP, λ {target: TargetObject => λ {attacker: TargetObject => ApplyEffect(attacker, CanOnlyAttack(target))}})) +
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
    (Seq("control a", "control an") ->
      ((S/NP)\NP,
        λ {p: TargetPlayer => λ {c: ObjectsMatchingConditions => CollectionExists(ObjectsMatchingConditions(c.objectType, c.conditions :+ ControlledBy(p)))}})
    ) +
    (Seq("cost", "energy cost") -> Seq(
      (N, Form(Cost): SemanticState),
      ((S\NP)/Num, λ {i: Scalar => λ {t: Target => AttributeAdjustment(t, Cost, Constant(i))}}),
      ((S\NP)/Adv, λ {o: Operation => λ {t: Target => AttributeAdjustment(t, Cost, o)}}),
      ((S\NP)/Num, λ {i: Scalar => λ {cp: CardPlay => AttributeAdjustment(All(CardsInHand(cp.player, cp.cardType)), Cost, Constant(i))}}),
      ((S\NP)/Adv, λ {o: Operation => λ {cp: CardPlay => AttributeAdjustment(All(CardsInHand(cp.player, cp.cardType)), Cost, o)}})
    )) +
    ("damage" -> Seq(
      ((S/PP)\Num, λ {amount: Number => λ {t: Target => DealDamage(t, amount)}}),
      ((S\NP)\Num, λ {amount: Number => λ {t: Target => DealDamage(t, amount)}}),
      ((S/Adj)/PP, λ {t: Target => λ {amount: Number => DealDamage(t, amount)}}),
      ((S\NP)/Adj, λ {amount: Number => λ {t: Target => DealDamage(t, amount)}}),
      (S/PP, λ {t: Target => DealDamage(t, AttributeValue(ThisObject, Attack))}),  // (by default, a robot deals damage equal to its power)
      (S\Num, λ {amount: Number => DealDamage(Choose(ObjectsInPlay(AllObjects)), amount)})  // (if no target is given, any target can be chosen)
    )) +
    ("damaged" -> (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(HasProperty(IsDamaged)))})) +
    (Seq("deal", "it deals", "takes") -> (X|X, identity)) +  // e.g. deals X damage, takes X damage
    ("destroy" -> (S/NP, λ {t: TargetObject => Destroy(t)})) +
    ("destroyed" -> Seq(
      (S\NP, λ {c: Choose => AfterDestroyed(All(c.collection))}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterDestroyed(t)})
    )) +
    (Seq("doesn't deal damage when attacked", "only deals damage when attacking") -> (S\NP, λ {t: TargetObject => ApplyEffect(t, CannotFightBack)})) +
    ("draw" -> (S/NP, λ {c: Cards => Draw(Self, c.num)})) +
    ("discard" -> (S/NP, λ {t: TargetObject => Discard(t)})) +
    ("double" -> Seq(
      ((S/PP)/N, λ {a: Attribute => λ {t: TargetObject => ModifyAttribute(t, a, Multiply(Scalar(2)))}}),
      (V/N, λ {a: Attribute => AttributeOperation(Multiply(Scalar(2)), a)})
    )) +
    (Seq("each", "every", "each player 's", "every player 's") -> Seq(
      (Adj, Form(AllPlayers): SemanticState),  // e.g. "each turn"
      (NP/PP, identity)  // e.g. "each of (your turns)"
    )) +
    ("end" -> (NP/PP, λ {turn: Turn => EndOfTurn(turn.player)})) +
    ("immediately" /?/ Seq("end the turn", "end your turn") -> (S, Form(EndTurn): SemanticState)) +
    ("enemy" -> (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(ControlledBy(Opponent)))})) +
    ("energy" -> Seq(
      (NP|Num, λ {amount: Number => Energy(amount)}),
      (NP/Adj, λ {amount: Number => Energy(amount)}),
      (S\S, λ {aa: AttributeAdjustment => AttributeAdjustment(aa.target, Cost, aa.operation)})  // "X costs Y more" == "X costs Y more energy"
    )) +
    ("equal" -> (Adj/PP, identity)) +
    ("event".s -> Seq(
      (N, Form(Event): SemanticState),
      (NP/PP, λ {hand: Hand => CardsInHand(hand.player, Event)})  // e.g. "All events in your hand"
    )) +
    (Seq("for each", "for every") -> (Adj/NP, λ {c: Collection => Count(c)})) +
    ("everything" -> (N, Form(AllObjects): SemanticState)) +
    ("everything adjacent to" -> (NP/NP, λ {t: TargetObject => All(ObjectsMatchingConditions(AllObjects, Seq(AdjacentTo(t))))})) +
    ("friendly" -> (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(ControlledBy(Self)))})) +
    (Seq("gain", "gains") -> Seq(
      (S/NP, λ {e: Energy => ModifyEnergy(Self, Plus(e.amount))}),  // Gain X energy.
      (S/NP, λ {l: Life => ModifyAttribute(ObjectsMatchingConditions(Kernel, Seq(ControlledBy(Self))), Health, Plus(l.amount))}),  // Gain X life.
      ((S/NP)\NP, λ {p: TargetPlayer => λ {e: Energy => ModifyEnergy(p, Plus(e.amount))}}),  // Y gains X energy.
      (((S\NP)/N)/Num, λ {num: Number => λ {a: Attribute => λ {t: TargetObject => ModifyAttribute(t, a, Plus(num))}}})  // Y gains X (attribute).
    )) +
    (("get".s ++ "gain".s ++ Seq("has", "have")) -> Seq( // "[All robots] get/gain/have ..."
      (((S/N)/Num)\NP, λ {t: TargetObject => λ {i: Scalar => λ {a: Attribute => SetAttribute(t, a, i)}}}),  // "... X attack"
      (((S/N)/Adj)\NP, λ {t: TargetObject => λ {o: Operation => λ {a: Attribute => ModifyAttribute(t, a, o)}}}),  // "... +X attack"
      ((S/NP)\NP, λ {t: TargetObject => λ {ops: Seq[AttributeOperation] =>  // "... +X attack and +Y speed"
        MultipleActions(Seq(SaveTarget(t)) ++ ops.map(op => ModifyAttribute(SavedTargetObject, op.attr, op.op)))}})
    )) +
    ("give" -> Seq( // "Give [a robot] ..."
      (((S/N)/Num)/NP, λ {t: TargetObject => λ {i: Scalar => λ {a: Attribute => SetAttribute(t, a, i)}}}),  // "... X attack"
      (((S/N)/Adj)/NP, λ {t: TargetObject => λ {o: Operation => λ {a: Attribute => ModifyAttribute(t, a, o)}}}),  // "... +X attack"
      ((S/NP)/NP, λ {t: TargetObject => λ {ops: Seq[AttributeOperation] =>  // "... +X attack and +Y speed"
        MultipleActions(Seq(SaveTarget(t)) ++ ops.map(op => ModifyAttribute(SavedTargetObject, op.attr, op.op)))}}),
      ((S/S)/NP, λ {t: TargetObject => λ {a: Ability => GiveAbility(t, a)}}),  // "... [ability]"
      ((S/S)/NP, λ {t: TargetObject => λ {a: (AttributeOperation, Ability) =>  //"... +X attack and [ability]"
        MultipleActions(Seq(SaveTarget(t), ModifyAttribute(SavedTargetObject, a._1.attr, a._1.op), GiveAbility(SavedTargetObject, a._2)))}})
    )) +
    ("hand" -> (NP\Adj, λ {p: TargetPlayer => Hand(p)})) +
    ("halve" -> Seq(
      (((S/PP)/Adv)/N, λ {a: Attribute => λ {r: Rounding => λ {t: TargetObject => ModifyAttribute(t, a, Divide(Scalar(2), r))}}}),
      ((V/Adv)/N, λ {a: Attribute => λ {r: Rounding => AttributeOperation(Divide(Scalar(2), r), a)}})
    )) +
    (Seq("has", "have") -> Seq(
      (S/NP, λ {ac: AttributeComparison => ac}),
      ((S/N)/Num, λ {i: Scalar => λ {a: Attribute => AttributeComparison(a, EqualTo(i))}}),
      ((S/N)/Adj, λ {c: Comparison => λ {a: Attribute => AttributeComparison(a, c)}}),
      ((S\NP)/S, λ {a: Ability => λ {t: TargetObject => HasAbility(t, a)}}),
      (((S\NP)/N)/Adj, λ {o: Operation => λ {a: Attribute => λ {t: TargetObject => AttributeAdjustment(t, a, o)}}})
    )) +
    (Seq("health", "life") -> Seq(
      (N, Form(Health): SemanticState),
      (N\Num, λ {i: Scalar => AttributeAmount(i, Health)}),
      (NP\Adj, λ {op: Operation => AttributeOperation(op, Health)}),
      (NP|Num, λ {amount: Number => Life(amount)}),
      (NP/Adj, λ {amount: Number => Life(amount)})
    )) +
    ("if" -> ((S|S)|S, λ {c: GlobalCondition => λ {a: Action => If(c, a)}})) +
    (Seq("in", "of") -> (PP/NP, identity)) +
    ("instead" -> (S|S, λ {a: Action => Instead(a)})) +
    ("in combat" -> (S\S, λ {t: AfterDestroyed => AfterDestroyed(t.target, Combat)})) +
    (Seq("in play", "on the board") -> (NP\N, λ {o: ObjectType => ObjectsInPlay(o)})) +
    ("is" -> (X|X, identity)) +
    ("it" -> (NP, Form(ItO): SemanticState)) +
    ("its" -> (Num/N, λ {a: Attribute => AttributeValue(ItO, a)})) +
    ("its controller" -> (NP, Form(ControllerOf(ItO)): SemanticState)) +
    (("kernel".s ++ "core".s) -> (N, Form(Kernel): SemanticState)) +
    ("less" -> (Adv\Num, λ {num: Number => Minus(num)})) +
    ("less than" -> (Adj/Num, λ {num: Number => LessThan(num)})) +
    ("more" -> (Adv\Num, λ {num: Number => Plus(num)})) +
    (Seq("more than", "greater than") -> (Adj/Num, λ {num: Number => GreaterThan(num)})) +
    ("moved last turn" -> (S, Form(HasProperty(MovedLastTurn)): SemanticState)) +
    ("moved this turn" -> (S, Form(HasProperty(MovedThisTurn)): SemanticState)) +
    ("moves" -> Seq(
      (S\NP, λ {c: Choose => AfterMove(All(c.collection))}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterMove(t)})
    )) +
    ("must" -> (X/X, identity)) +
    ("number" -> (Num/PP, λ {c: Collection => Count(c)})) +
    (("object".s :+ "objects '") -> (N, Form(AllObjects): SemanticState)) +
    ("of" -> ((S/NP)\V, λ {ops: Seq[AttributeOperation] => λ {t: TargetObject => MultipleActions(Seq(SaveTarget(t)) ++ ops.map(op => ModifyAttribute(SavedTargetObject, op.attr, op.op)))}})) +
    ("other" -> (NP/N, λ {o: ObjectType => Other(ObjectsInPlay(o))})) +
    (Seq("or", "and") -> ((N/N)\N, λ {o1: ObjectType => λ {o2: ObjectType => MultipleObjectTypes(Seq(o1, o2))}})) +
    ("or less" -> Seq(
      (Adj\Num, λ {num: Number => LessThanOrEqualTo(num)}),
      (NP\N, λ {aa: AttributeAmount => AttributeComparison(aa.attr, LessThanOrEqualTo(aa.amount))})
    )) +
    (Seq("or more", "or greater") -> Seq(
      (Adj\Num, λ {num: Number => GreaterThanOrEqualTo(num)}),
      (NP\N, λ {aa: AttributeAmount => AttributeComparison(aa.attr, GreaterThanOrEqualTo(aa.amount))})
    )) +
    ("play".s -> Seq(
      ((S/N)\NP, λ {t: TargetPlayer => λ {c: CardType => AfterCardPlay(t, c)}}),  // e.g. "[whenever] you play a robot, [do something]"
      ((NP\N)\NP, λ {t: TargetPlayer => λ {c: CardType => CardPlay(t, c)}})  // e.g. "robots you play [cost X less, etc]"
    )) +
    (Seq("played", "comes into play", "enters the board") -> Seq(
      (S\NP, λ {c: Choose => AfterPlayed(All(c.collection))}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterPlayed(t)})
    )) +
    (Seq("power", "attack") -> Seq(
      (N, Form(Attack): SemanticState),
      (N\Num, λ {i: Scalar => AttributeAmount(i, Attack)}),
      (NP\Adj, λ {op: Operation => AttributeOperation(op, Attack)})
    )) +
    ("random" -> Seq(
      ((NP/N)\Num, λ {num: Number => λ {o: ObjectType => Random(num, ObjectsInPlay(o))}}),  // e.g. "Destroy a random robot"
      ((NP/NP)\Num, λ {num: Number => λ {c: Collection => Random(num, c)}})  // e.g. "Discard 2 random cards"
    )) +
    ("reduce" -> Seq(
      (((S/PP)/PP)/N, λ {a: Attribute => λ {t: TargetObject => λ {num: Number => ModifyAttribute(t, a, Minus(num))}}}),
      (((S/PP)/PP)/N, λ {a: Attribute => λ {c: CardsInHand => λ {num: Number => ModifyAttribute(All(c), a, Minus(num))}}})
    )) +
    ("restore" -> (S/NP, λ {ta: TargetAttribute => ta.attr match { case Health => RestoreHealth(ta.target); case a => Fail(s"Expected Health, got $a")}})) +
    (("robot".s :+ "robots '") -> Seq(
      (N, Form(Robot): SemanticState),
      (NP/PP, λ {hand: Hand => CardsInHand(hand.player, Robot)})  // e.g. "all robots in your hand"
    )) +
    ("(rounded down)" -> (Adv, Form(RoundedDown): SemanticState)) +
    ("(rounded up)" -> (Adv, Form(RoundedUp): SemanticState)) +
    ("set" -> Seq(
      ((S/PP)/NP, λ {t: TargetAttribute => λ {num: Number => SetAttribute(t.target, t.attr, num)}}),
      (((S/PP)/PP)/N, λ {a: Attribute => λ {t: TargetObject => λ {num: Number => SetAttribute(t, a, num)}}})
    )) +
    ("spaces" -> (NP\Num, λ {num: Number => Spaces(num)})) +
    ("speed" -> Seq(
      (N, Form(Speed): SemanticState),
      (N\Num, λ {i: Scalar => AttributeAmount(i, Speed)}),
      (NP\Adj, λ {op: Operation => AttributeOperation(op, Speed)})
    )) +
    (("structure".s :+ "structures '") -> Seq(
      (N, Form(Structure): SemanticState),
      (NP/PP, λ {hand: Hand => CardsInHand(hand.player, Structure)})  // e.g. "All structures in your hand"
    )) +
    ("swap" -> Seq(
      ((S/N)/NP, λ {t: TargetObject => λ {attrs: Seq[Attribute] => SwapAttributes(t, attrs(0), attrs(1))}}),
      ((S/PP)/N, λ {attrs: Seq[Attribute] => λ {t: TargetObject => SwapAttributes(t, attrs(0), attrs(1))}})
    )) +
    ("take control" -> (S/PP, λ {t: TargetObject => TakeControl(Self, t)})) +
    ("takes damage" -> Seq(
      (S\NP, λ {c: Choose => AfterDamageReceived(All(c.collection))}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterDamageReceived(t)})
    )) +
    (Seq("target", "a target") -> (NP/NP, λ {c: Collection => Choose(c)})) +
    ("to" -> Seq(
      (PP/NP, identity),
      (PP/Num, identity)
    )) +
    ("that" -> ((NP\N)/S, λ {c: Condition => λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(c))}})) +
    (Seq("that player", "they") -> (NP, Form(ItP): SemanticState)) +
    ("the" -> (X/X, identity)) +
    ("this" / Seq("robot", "creature", "structure", "object") -> (NP, Form(ThisObject): SemanticState)) +
    ("total" -> ((Num/PP)/N, λ {a: Attribute => λ {c: Collection => AttributeSum(c, a)}})) +
    ("turn".s -> (NP\Adj, λ {p: TargetPlayer => Turn(p)})) +
    (Seq("when", "whenever", "after", "immediately after", "each time", "every time") ->
      ((S|S)|S, λ {t: Trigger => λ {a: Action => TriggeredAbility(t, a)}})
    ) +
    ("with" -> Seq(  // "with" = "that" + "has"
      ((NP\N)/NP, λ {ac: AttributeComparison => λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(ac))}}),
      (((NP\N)/N)/Num, λ {i: Scalar => λ {a: Attribute => λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(AttributeComparison(a, EqualTo(i))))}}}),
      (((NP\N)/N)/Adj, λ {c: Comparison => λ {a: Attribute => λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(AttributeComparison(a, c)))}}}),
      (((NP\N)/Adj)/N, λ {a: Attribute => λ {c: Comparison => λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(AttributeComparison(a, c)))}}})
    )) +
    ("within" -> ((NP\NP)/NP, λ {s: Spaces => λ {c: ObjectsMatchingConditions => ObjectsMatchingConditions(c.objectType, c.conditions :+ WithinDistanceOf(s.num, ThisObject))}})) +
    (Seq("you", "yourself") -> (NP, Form(Self): SemanticState)) +
    ("your" -> Seq(
      (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(ControlledBy(Self)))}),
      (NP/NP, λ {c: ObjectsMatchingConditions => ObjectsMatchingConditions(c.objectType, c.conditions :+ ControlledBy(Self))}),
      (Adj, Form(Self): SemanticState)
    )) +
    ("your energy" -> (NP, Form(EnergyAmount(Self)): SemanticState)) +
    ("your opponent" -> (NP, Form(Opponent): SemanticState)) +
    ("your opponent 's energy" -> (NP, Form(EnergyAmount(Opponent)): SemanticState)) +
    (Seq("your opponent 's", "all of your opponent 's") -> Seq(
      (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(ControlledBy(Opponent)))}),
      (NP/NP, λ {c: ObjectsMatchingConditions => ObjectsMatchingConditions(c.objectType, c.conditions :+ ControlledBy(Opponent))}),
      (Adj, Form(Opponent): SemanticState)
    )) +
    (Seq("'", "'s") -> ((NP\NP)/N, λ {a: Attribute => λ {t: TargetObject => TargetAttribute(t, a)}})) +
    ("\"" -> Seq(
      (Quoted/S, identity),
      (S\Quoted, identity)
    )) +
    (StrictIntegerMatcher -> (Num, {i: Int => Form(Scalar(i))})) +
    (NumberWordMatcher -> (Num, {i: Int => Form(Scalar(i))})) +
    (PrefixedIntegerMatcher("+") -> (Adj, {i: Int => Form(Plus(Scalar(i)))})) +
    (PrefixedIntegerMatcher("-") -> (Adj, {i: Int => Form(Minus(Scalar(i)))}))
}
