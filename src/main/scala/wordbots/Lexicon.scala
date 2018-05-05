package wordbots

import com.workday.montague.ccg._
import com.workday.montague.parser.ParserDict
import com.workday.montague.semantics._
import com.workday.montague.semantics.FunctionReaderMacro.λ

import scala.language.postfixOps

/**
  * Created by alex on 2/28/17.
  * reminder: N = noun, NP = noun phrase, V = verb, S = sentence, PP = propositional phrase
  * '\X'=needs X before this, '/X' = needs X after this, '|X' = needs X either before or after this
  **/
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
      (NP/N, λ {o: ObjectType => ChooseO(ObjectsInPlay(o))}),  // e.g. "a robot"
      (NP/NP, λ {c: ObjectCollection => ChooseO(c)}),  // e.g. "a robot you control"
      (NP/NP, λ {c: CardCollection => ChooseC(c)}),  // e.g. "(discard) a card"
      (Num, Form(Scalar(1)): SemanticState)  // e.g. "(draw) a card"
    )) +
    ("a player" -> (NP, Form(ChooseO(ObjectsInPlay(Kernel))): SemanticState)) +
    ("a tile" -> (NP, Form(ChooseO(AllTiles)): SemanticState)) +
    ("activate:" -> (S/S, λ {a: Action => ActivatedAbility(a)})) +
    ("adjacent" -> Seq(
      (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(AdjacentTo(ThisObject)))}),
      (NP/NP, λ {c: ObjectsMatchingConditions => ObjectsMatchingConditions(c.objectType, Seq(AdjacentTo(ThisObject)) ++ c.conditions)})
    )) +
    ("adjacent to" -> ((NP/NP)\N, λ {o: ObjectType => λ {t: TargetObject => ObjectsMatchingConditions(o, Seq(AdjacentTo(t)))}})) +
    (Seq("adjacent to a", "adjacent to an") ->
      ((S/NP)\NP, λ {o: TargetObject => λ {c: ObjectsMatchingConditions => CollectionExists(ObjectsMatchingConditions(c.objectType, c.conditions :+ AdjacentTo(o)))}})
    ) +
    ("after attacking" -> (S\S, λ {a: Action => TriggeredAbility(AfterAttack(ThisObject, AllObjects), a)})) +
    (Seq("all", "each", "every") -> Seq( // Also see Seq("each", "every") below for definitions that DON'T apply to "all".
      (NP/N, λ {o: ObjectType => ObjectsInPlay(o)}),
      (NP/N, λ {c: CardType => AllC(CardsInHand(AllPlayers, c))}),
      (NP/NP, λ {c: CardCollection => AllC(c)}),
      (NP/NP, λ {c: Collection => c}),
      (NP/PP, λ {c: Collection => c})
    )) +
    ("all" /?/ Seq("attributes", "stats") -> (N, Form(AllAttributes): SemanticState)) +
    (Seq("all players", "each player", "every player", "both players") -> (NP, Form(AllPlayers): SemanticState)) +
    (Seq("all your other", "all of your other", "your other") -> (NP/N, λ {o: ObjectType => Other(ObjectsMatchingConditions(o, Seq(ControlledBy(Self))))})) +
    ("and" -> Seq(
      (ReverseConj, λ {a: ParseNode => λ {b: ParseNode => Seq(a, b)}}),
      (conj, λ {b: ParseNode => λ {a: Seq[ParseNode] => a :+ b}}),
      ((S/S)\NP, λ {a: ParseNode => λ {b: ParseNode => (a, b)}})
    )) +
    ("at" -> ((S|S)/NP, λ {t: Trigger => λ {a: Action => TriggeredAbility(t, a)}})) +
    (Seq("at most", "up to") -> (Adj/Num, λ {num: Number => LessThanOrEqualTo(num)})) +
    (Seq("attack", "power") -> Seq(
      (N, Form(Attack): SemanticState),
      (N\Num, λ {i: Scalar => AttributeAmount(i, Attack)}),
      (NP\Adj, λ {op: Operation => AttributeOperation(op, Attack)}),
      (NP\Adj, λ {comp : Comparison => AttributeComparison(Attack, comp)}), // needed for "> x health"
      (NP\Num, λ {n: Number => AttributeComparison(Attack, EqualTo(n))}) // "...with X health"(implied "equal to" in there)
    )) +
    ("attacks" -> Seq(
      (S\NP, λ {c: ChooseO => AfterAttack(AllO(c.collection), AllObjects)}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterAttack(t, AllObjects)}),
      ((S\NP)/N, λ {o: ObjectType => λ {t: TargetObject => AfterAttack(t, o)}})
    )) +
    ("attacked last turn" -> (S, Form(HasProperty(AttackedLastTurn)): SemanticState)) +
    ("attacked this turn" -> (S, Form(HasProperty(AttackedThisTurn)): SemanticState)) +
    ("becomes" -> ((S\NP)/NP, λ {target: TargetCard => λ {source: TargetObject => Become(source, target)}})) +
    (Seq("beginning", "start") -> (NP/PP, λ {turn: Turn => BeginningOfTurn(turn.player)})) +
    ("by" -> (PP/Num, identity)) +
    (Seq("can move", "can move again", "gains a second move action") -> (S\NP, λ {t: TargetObject => CanMoveAgain(t)})) +
    (Seq("can attack", "can attack again") -> (S\NP, λ {t: TargetObject => CanAttackAgain(t)})) +
    (Seq("can move and attack", "can move and attack again") -> (S\NP, λ {t: TargetObject => CanMoveAndAttackAgain(t)})) +
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
    ("a copy of" -> (NP/NP, λ {t:TargetObject => CopyOfC(t)})) + // can this be decomposed further?
    (Seq("cost", "energy cost") -> Seq(
      (N, Form(Cost): SemanticState),
      (NP/Adj, λ {comp : Comparison => AttributeComparison(Cost, comp)}), // needed for "cost > x "
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
      (S/PP, λ {t: TargetObjectOrPlayer => DealDamage(t, AttributeValue(ThisObject, Attack))}),  // (by default, a robot deals damage equal to its power)
      (S\Num, λ {amount: Number => DealDamage(ChooseO(ObjectsInPlay(AllObjects)), amount)})  // (if no target is given, any target can be chosen)
    )) +
    ("damaged" -> (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(HasProperty(IsDamaged)))})) +
    (Seq("deal", "deals", "it deals", "take", "takes") -> (X|X, identity)) +  // e.g. deals X damage, takes X damage
    ("destroy" -> (S/NP, λ {t: TargetObject => Destroy(t)})) +
    ("destroyed" -> Seq(
      (S\NP, λ {c: ChooseO => AfterDestroyed(AllO(c.collection))}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterDestroyed(t)}),
      (S\NP, λ {o: TargetObject => TargetHasProperty(o, IsDestroyed)}) // Condition form (e.g. "If that robot is destroyed, [...]"
    )) +
    (Seq("doesn't deal damage when attacked", "only deals damage when attacking") -> (S\NP, λ {t: TargetObject => ApplyEffect(t, CannotFightBack)})) +
    ("draw" -> (S/NP, λ {c: Cards => Draw(Self, c.num)})) +
    ("draw".s -> ((S/NP)\NP, λ {p: TargetPlayer => λ {c: Cards => Draw(p, c.num)}})) +
    ("discard" -> (S/NP, λ {t: TargetCard => Discard(t)})) +
    ("discards" -> ((S/NP)\NP, λ {p: TargetPlayer => λ {c: RandomCards => Discard(RandomC(c.num, CardsInHand(p, c.cardType)))}})) +
    ("double" -> Seq(
      (S/NP, λ {ta: TargetAttribute => ModifyAttribute(ta.target, ta.attr, Multiply(Scalar(2)))}),
      ((S/PP)/N, λ {a: Attribute => λ {t: TargetObject => ModifyAttribute(t, a, Multiply(Scalar(2)))}}),
      (V/N, λ {a: Attribute => AttributeOperation(Multiply(Scalar(2)), a)})
    )) +
    (Seq("each", "every", "each player 's", "every player 's") -> Seq(
      (Adj, Form(AllPlayers): SemanticState),  // e.g. "each turn"
      (NP/PP, identity)  // e.g. "each of (your turns)"
    )) +
    ("end" -> (NP/PP, λ {turn: Turn => EndOfTurn(turn.player)})) +
    (Seq("end of turn", "end of the turn") -> (NP, Form(TurnsPassed(1)): SemanticState)) +
    (Seq("end of next turn", "end of the next turn") -> (NP, Form(TurnsPassed(2)): SemanticState)) +
    ("immediately" /?/ Seq("end the turn", "end your turn") -> (S, Form(EndTurn): SemanticState)) +
    ("enemy" -> Seq(
      (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(ControlledBy(Opponent)))}),
      (NP/NP, λ {c: ObjectsMatchingConditions => ObjectsMatchingConditions(c.objectType, Seq(ControlledBy(Opponent)) ++ c.conditions)})
    )) +
    ("energy" -> Seq(
      (NP|Num, λ {amount: Number => Energy(amount)}),
      (NP/Adj, λ {amount: Number => Energy(amount)}),
      (S\S, λ {aa: AttributeAdjustment => AttributeAdjustment(aa.target, Cost, aa.operation)})  // "X costs Y more" == "X costs Y more energy"
    )) +
    ("equal" -> Seq(
      (Adj/PP, identity),
      (Adj/PP, λ {num: Number => EqualTo(num)})
    )) +
    (("event".s ++ "event card".s) -> Seq(
      (N, Form(Event): SemanticState),
      (NP/PP, λ {hand: Hand => CardsInHand(hand.player, Event)})  // e.g. "All events in your hand"
    )) +
    (Seq("for each", "for every") -> (Adj/NP, λ {c: Collection => Count(c)})) +
    ("everything" -> (N, Form(AllObjects): SemanticState)) +
    ("everything adjacent to" -> (NP/NP, λ {t: TargetObject => AllO(ObjectsMatchingConditions(AllObjects, Seq(AdjacentTo(t))))})) +
    ("friendly" -> Seq(
      (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(ControlledBy(Self)))}),
      (NP/NP, λ {c: ObjectsMatchingConditions => ObjectsMatchingConditions(c.objectType, Seq(ControlledBy(Self)) ++ c.conditions)})
    )) +
    ("gain" -> Seq(
      (S/NP, λ {e: Energy => ModifyEnergy(Self, Plus(e.amount))}),  // Gain X energy.
      (S/NP, λ {l: Life => ModifyAttribute(ObjectsMatchingConditions(Kernel, Seq(ControlledBy(Self))), Health, Plus(l.amount))})  // Gain X life.
    )) +
    ("gains" -> Seq(
      ((S/NP)\NP, λ {p: TargetPlayer => λ {e: Energy => ModifyEnergy(p, Plus(e.amount))}}),  // Y gains X energy.
      (((S\NP)/N)/Num, λ {num: Number => λ {a: Attribute => λ {t: TargetObject => ModifyAttribute(t, a, Plus(num))}}})  // Y gains X (attribute).
    )) +
    (("get".s ++ "gain".s) -> Seq( // "[All robots] get/gain/have ..."
      (((S/N)/Num)\NP, λ {t: TargetObject => λ {i: Scalar => λ {a: Attribute => SetAttribute(t, a, i)}}}),  // "... X attack"
      (((S/N)/Adj)\NP, λ {t: TargetObject => λ {o: Operation => λ {a: Attribute => ModifyAttribute(t, a, o)}}}),  // "... +X attack"
      ((S/N)\NP, λ {t: TargetObject => λ {attrs: Seq[AttributeAmount] =>  // "... X attack and Y speed"
        MultipleActions(Seq(SaveTarget(t)) ++ attrs.map(a => ModifyAttribute(SavedTargetObject, a.attr, Plus(a.amount))))}}),
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
      (((S\NP)/N)/Adj, λ {o: Operation => λ {a: Attribute => λ {t: TargetObject => AttributeAdjustment(t, a, o)}}}),
      ((S/S)\NP, λ {t: TargetObject => λ {a: (AttributeOperation, Ability) =>  // "... +X attack and [ability]"
        MultipleAbilities(Seq(AttributeAdjustment(t, a._1.attr, a._1.op), HasAbility(t, a._2)))}})
    )) +
    (Seq("health", "life") -> Seq(
      (N, Form(Health): SemanticState),
      (N\Num, λ {i: Scalar => AttributeAmount(i, Health)}),
      (NP\Adj, λ {op: Operation => AttributeOperation(op, Health)}),
      (NP\Num, λ {n: Number => AttributeComparison(Health, EqualTo(n))}), // "...with X health"(implied "equal to" in there)
      (NP\Adj, λ {comp : Comparison => AttributeComparison(Health, comp)}), // "...greater than X health"
      (NP|Num, λ {amount: Number => Life(amount)}),
      (NP/Adj, λ {amount: Number => Life(amount)})
    )) +
    ("if" -> ((S|S)|S, λ {c: GlobalCondition => λ {a: Action => If(c, a)}})) +
    (Seq("in", "of", "from") -> (PP/NP, identity)) +
    ("instead" -> (S|S, λ {a: Action => Instead(a)})) +
    ("in combat" -> (S\S, λ {t: AfterDestroyed => AfterDestroyed(t.target, Combat)})) +
    (Seq("in play", "on the board") -> (NP\N, λ {o: ObjectType => ObjectsInPlay(o)})) +
    ("is" -> (X|X, identity)) +
    ("it" -> (NP, Form(ItO): SemanticState)) +
    ("its" -> (Num/N, λ {a: Attribute => AttributeValue(ItO, a)})) +
    ("its controller" -> (NP, Form(ControllerOf(ItO)): SemanticState)) +
    (Seq("its owner 's hand", "its controller 's hand", "their owner 's hands", "their controller 's hands") -> (NP, Form(ItsOwnersHand): SemanticState)) +
    (("kernel".s ++ "core".s) -> (N, Form(Kernel): SemanticState)) +
    ("less" -> (Adv\Num, λ {num: Number => Minus(num)})) +
    ("less than" -> (Adj/Num, λ {num: Number => LessThan(num)})) +
    (Seq("lose", "pay") -> (S/NP, λ {l: Life => DealDamage(Self, l.amount)})) +
    ("loses" -> (((S\NP)/N)/Num, λ {num: Number => λ {a: Attribute => λ {t: TargetObject => ModifyAttribute(t, a, Minus(num))}}})) +  // Y loses X (attribute).
    ("more" -> (Adv\Num, λ {num: Number => Plus(num)})) +
    ("move" -> ((S/NP)/NP, λ {t: TargetObject => λ {d: WithinDistance => MultipleActions(Seq(
      SaveTarget(t),
      MoveObject(SavedTargetObject, ChooseO(TilesMatchingConditions(Seq(WithinDistanceOf(d.spaces, SavedTargetObject), Unoccupied)))))
    )}})) +
    (Seq("more than", "greater than") -> (Adj/Num, λ {num: Number => GreaterThan(num)})) +
    ("moved last turn" -> (S, Form(HasProperty(MovedLastTurn)): SemanticState)) +
    ("moved this turn" -> (S, Form(HasProperty(MovedThisTurn)): SemanticState)) +
    ("moves" -> Seq(
      (S\NP, λ {c: ChooseO => AfterMove(AllO(c.collection))}), // For this and other triggers, replace Choose targets w/ All targets.
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
      ((NP/NP)\Num, λ {num: Number => λ {c: ObjectCollection => RandomO(num, c)}})
    )) +
    ("reduce" -> Seq(
      (((S/PP)/PP)/N, λ {a: Attribute => λ {t: TargetObject => λ {num: Number => ModifyAttribute(t, a, Minus(num))}}}),
      (((S/PP)/PP)/N, λ {a: Attribute => λ {t: TargetCard => λ {num: Number => ModifyAttribute(t, a, Minus(num))}}}),
      (((S/PP)/PP)/N, λ {a: Attribute => λ {c: CardsInHand => λ {num: Number => ModifyAttribute(AllC(c), a, Minus(num))}}})  // e.g. "Reduce the cost of robot cards in your hand by 1"
    )) +
    ("remove all abilities" -> (S/PP, λ {t: TargetObject => RemoveAllAbilities(t)})) +
    ("restore" -> Seq(
      ((S/PP)/N, λ {a: Attribute => λ {t: TargetObject => RestoreAttribute(t, a, None)}}),  // e.g. "Restore health to X"
      (((S/PP)/N)/Num, λ {n: Number => λ {a: Attribute => λ {t: TargetObject => RestoreAttribute(t, a, Some(n))}}}),  // e.g. "Restore N health to X"
      (S/NP, λ {ta: TargetAttribute => RestoreAttribute(ta.target, ta.attr, None)})  // e.g. "Restore X's health"
    )) +
    ("return" -> ((S/PP)/NP, λ {t: TargetObject => λ {_: ItsOwnersHand.type => ReturnToHand(t)}})) +
    (("robot".s :+ "robots '") -> Seq(
      (N, Form(Robot): SemanticState),
      (NP/PP, λ {hand: Hand => CardsInHand(hand.player, Robot)})  // e.g. "all robots in your hand"
    )) +
    ("rounded down" -> (Adv, Form(RoundedDown): SemanticState)) +
    ("rounded up" -> (Adv, Form(RoundedUp): SemanticState)) +
    ("set" -> Seq(
      ((S/PP)/NP, λ {t: TargetAttribute => λ {num: Number => SetAttribute(t.target, t.attr, num)}}),
      (((S/PP)/PP)/N, λ {a: Attribute => λ {t: TargetObject => λ {num: Number => SetAttribute(t, a, num)}}}),
      (((S/PP)/PP)/N, λ {as: Seq[Attribute] => λ {t: TargetObject => λ {num: Number => MultipleActions(Seq(SaveTarget(t)) ++ as.map(a => SetAttribute(SavedTargetObject, a, num)))}}})
    )) +
    ("spaces" -> Seq(
      (NP\Num, λ {num: Number => Spaces(num)}),
      (NP\Adj, λ {c: LessThanOrEqualTo => WithinDistance(c.num)})
    )) +
    ("speed" -> Seq(
      (N, Form(Speed): SemanticState),
      (N\Num, λ {i: Scalar => AttributeAmount(i, Speed)}),
      (NP\Adj, λ {op: Operation => AttributeOperation(op, Speed)}),
      (NP\Adj, λ {comp : Comparison => AttributeComparison(Speed, comp)}), // need for "> x speed"
      (NP\Num, λ {n: Number => AttributeComparison(Speed, EqualTo(n))}) // need for "...with X health"(implied "equal to" in there)
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
      (S\NP, λ {c: ChooseO => AfterDamageReceived(AllO(c.collection))}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterDamageReceived(t)})
    )) +
    (Seq("target", "a target") -> (NP/NP, λ {c: ObjectCollection => ChooseO(c)})) +
    (Seq("to", "equal to") -> Seq(
      (PP/NP, identity),
      (PP/Num, identity)
    )) +
    ("that" -> Seq(
      ((NP\N)/S, λ {c: Condition => λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(c))}}),
      ((NP\N)/S, λ {cs: Seq[Condition] => λ {o: ObjectType => ObjectsMatchingConditions(o, cs)}})
    )) +
    ("that" / Seq("robot", "creature", "structure", "object") -> (NP, Form(That): SemanticState)) +
    (Seq("that player", "they") -> (NP, Form(ItP): SemanticState)) +
    ("the" -> (X/X, identity)) +
    ("their" -> (Num/N, λ {a: Attribute => AttributeValue(They, a)})) +
    (Seq("then", "and", "to") -> ((S/S)\S, λ {a1: Action => λ {a2: Action => And(a1, a2)}})) +
    ("this" / Seq("robot", "creature", "structure", "object") -> (NP, Form(ThisObject): SemanticState)) +
    ("total" -> ((Num/PP)/N, λ {a: Attribute => λ {c: Collection => AttributeSum(c, a)}})) +
    ("turn".s -> (NP\Adj, λ {p: TargetPlayer => Turn(p)})) +
    ("until" -> ((S|S)|NP, λ {d: Duration => λ {a: Action => Until(d, a)}})) +
    (Seq("when", "whenever", "after", "immediately after", "each time", "every time") ->
      ((S|S)|S, λ {t: Trigger => λ {a: Action => TriggeredAbility(t, a)}})
    ) +
    ("with" -> Seq(  // "with" = "that" + "has"
      ((NP\N)/NP, λ {s: AttributeComparison => λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(s))}}),
      ((NP\N)/NP, λ {s: Seq[AttributeComparison] => λ {o: ObjectType => ObjectsMatchingConditions(o, s)}})
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
    (Seq("'", "'s") -> Seq(
      ((NP\NP)/N, λ {a: Attribute => λ {t: TargetObject => TargetAttribute(t, a)}}),
      ((NP\NP)/N, λ {a: Attribute => λ {t: TargetObject => AttributeValue(t, a)}})
    )) +
    ("\"" -> Seq(
      (Quoted/S, identity),
      (S\Quoted, identity)
    )) +
    (StrictIntegerMatcher -> (Num, {i: Int => Form(Scalar(i))})) +
    (NumberWordMatcher -> (Num, {i: Int => Form(Scalar(i))})) +
    (PrefixedIntegerMatcher("+") -> (Adj, {i: Int => Form(Plus(Scalar(i)))})) +
    (PrefixedIntegerMatcher("-") -> (Adj, {i: Int => Form(Minus(Scalar(i)))}))

  lazy val syntaxOnlyLexicon: ParserDict[CcgCat] = {
    ParserDict[CcgCat](
      Lexicon.lexicon.map.mapValues(_.map {case (syn, sem) => (syn, Ignored(""))}),
      Lexicon.lexicon.funcs.map(func => {str: String => func(str).map {case (syn, sem) => (syn, Ignored(""))}}),
      Lexicon.lexicon.fallbacks.map(func => {str: String => func(str).map {case (syn, sem) => (syn, Ignored(""))}})
    ).withTerms(categoriesMap)
  }

  lazy val categories: Seq[CcgCat] = {
    lexicon.map
      .flatMap(d => d._2.map(_._1))
      .toSeq
      .distinct
      .sortBy(cat => cat.category.count(raw"/|\\" contains _))  // Sort categories by increasing complexity.
  }

  lazy val categoriesMap: Map[String, Seq[(CcgCat, SemanticState)]] = {
    categories.map(cat => s"#${cat.category.toLowerCase.replaceAll("[\\(\\)]", "")}#" -> Seq(cat -> Ignored(""))).toMap
  }
  lazy val terminalCategoriesMap: Map[String, Seq[(CcgCat, SemanticState)]] = {
    categoriesMap.filterKeys(cat => cat.matches(raw"[^/|\\]*"))
  }

  def termsInCategory(category: CcgCat): Seq[String] = {
    lexicon.map.filter { case (term, definition) =>
      definition.exists(_._1 == category)
    }.keys.toSeq
  }
}
