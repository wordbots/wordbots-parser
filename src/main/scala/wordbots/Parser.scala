package wordbots

import java.io.PrintWriter

import com.workday.montague.ccg._
import com.workday.montague.parser._
import com.workday.montague.semantics._
import com.workday.montague.semantics.{λ => λP}
import com.workday.montague.semantics.FunctionReaderMacro.λ

object Parser extends SemanticParser[CcgCat](Lexicon.lexicon) {
  def parse(input: String): SemanticParseResult[CcgCat] = parse(input, tokenizer)

  override def main(args: Array[String]): Unit = {
    val input = args.mkString(" ")
    val result: SemanticParseResult[CcgCat] = parse(input)

    val output = result.bestParse.map(p => s"${p.semantic.toString} [${p.syntactic.toString}]").getOrElse("(failed to parse)")

    // If there's no parse, try to figure out what went wrong ... perhaps there's an unidentified token?
    if (result.bestParse.isEmpty) {
      val unrecognizedTokens = findUnrecognizedTokens(input)
      if (unrecognizedTokens.nonEmpty) {
        throw new RuntimeException(s"Unrecognized term(s): ${unrecognizedTokens.mkString(", ")}")
      }
    }

    println(s"Input: $input")
    // println(s"Tokens: ${tokenizer(input).mkString("[\"", "\", \"", "\"]")}")
    println(s"Parse result: $output")

    val code = result.bestParse.map(_.semantic) match {
      case Some(Form(v: AstNode)) => CodeGenerator.generateJS(v)
      case _ => "(n/a)"
    }

    println(s"Generated JS code: $code")

    // For debug purposes, output the best parse tree (if one exists) to SVG.
    result.bestParse.foreach(result => new PrintWriter("test.svg") { write(result.toSvg); close() })
  }

  def findUnrecognizedTokens(input: String): Seq[String] = {
    val tokens = tokenizer(input)
    val lexicon = Lexicon.lexicon

    tokens.filter { token =>
      !lexicon.map.keys.exists(_.contains(token)) && lexicon.funcs.forall(_(token).isEmpty)
    }
  }

  private def tokenizer(str: String): IndexedSeq[String] = {
    str.trim.toLowerCase.split("\\s+|[.?!,]").filter("" !=)
  }
}

object Lexicon {
  type PF = PartialFunction[AstNode, AstNode] // Partial function literals have to be explicitly typed because scala compiler is dumb.

  implicit class StringImplicits(val str: String) extends AnyVal {
    // "blah".s = ["blah", "blahs"]
    def s: Seq[String] = Seq(str, str + "s")

    // "my" / ["thing", "stuff"] = ["my thing", "my stuff"]
    def /(nextWords: Seq[String]): Seq[String] = nextWords.map(s"$str " +)

    // "my" /?/ ["thing", "stuff"] = ["thing", "stuff", "my thing", "my stuff"]
    def /?/(nextWords: Seq[String]): Seq[String] = nextWords ++ nextWords.map(s"$str " +)
  }

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
    ("and" -> (((S/PP)/V)\V, λ {a1: CurriedAction => λ {a2: CurriedAction => λ {t: Target => And(a1.action(t), a2.action(t))}}})) +
    ("at" -> ((S/S)/NP, λ {t: Trigger => λ {a: Action => At(t, a)}})) +
    ("attacks" -> (S\NP, λP ({case Choose(coll) => AfterAttack(All(coll))  // For this and other triggers, replace Choose targets w/ All targets.
                              case t: TargetObject => AfterAttack(t)}: PF))) +
    ("beginning" -> (NP/PP, λ {turn: Turn => BeginningOfTurn(turn.player)})) +
    ("by" -> (PP/Num, identity)) +
    (Seq("can move again", "gains a second move action") -> (S\NP, λ {t: TargetObject => CanMoveAgain(t)})) +
    ("can't attack" -> (S\NP, λ {t: TargetObject => ApplyEffect(t, CannotAttack)})) +
    ("can't be changed" -> (S\NP, λ {t: TargetAttribute => FreezeAttribute(t.target, t.attr)})) +
    ("card".s -> Seq(
      (N, Form(AnyCard): SemanticState),
      (NP\Num, λ {num: Number => Cards(num)}),
      (NP/Adj, λ {num: Number => Cards(num)}),
      (NP/PP, λ {hand: Hand => CardsInHand(hand.player)})
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
    ("destroy" -> (S/NP, λ {t: Target => Destroy(t)})) +
    ("destroyed" -> (S\NP, λP ({case Choose(coll) => AfterDestroyed(All(coll))
                                case t: TargetObject => AfterDestroyed(t)}: PF))) +
    ("draw" -> (S/NP, λ {c: Cards => Draw(Self, c.num)})) +
    ("discard" -> Seq(
      (S/NP, λ {c: Cards => Discard(Self, c.num)}),
      ((S/NP)\NP, λ {t: TargetPlayer => λ {c: Cards => Discard(t, c.num)}})
    )) +
    ("double" -> Seq(
      ((S/PP)/N, λ {a: Attribute => λ {t: Target => ModifyAttribute(t, a, Multiply(Scalar(2)))}}),
      (V/N, λ {a: Attribute => CurriedAction({t: Target => ModifyAttribute(t, a, Multiply(Scalar(2)))})})
    )) +
    ("each" -> Seq(
      (Adj, Form(AllPlayers): SemanticState),  // e.g. "each turn"
      (NP/PP, identity)  // e.g. "each of (your turns)"
    )) +
    ("end" -> (NP/PP, λ {turn: Turn => EndOfTurn(turn.player)})) +
    ("energy" -> Seq(
      (NP|Num, λ {amount: Number => Energy(amount)}),
      (NP/Adj, λ {amount: Number => Energy(amount)}),
      (S\S, λ ({case AttributeAdjustment(target, Cost, op) => AttributeAdjustment(target, Cost, op)}: PF))  // "X costs Y more/less" == "X costs Y more/less energy"
    )) +
    ("equal" -> (Adj/PP, identity)) +
    ("everything" -> (N, Form(AllObjects): SemanticState)) +
    ("everything adjacent to" -> (NP/NP, λ {t: TargetObject => All(ObjectsMatchingConditions(AllObjects, Seq(AdjacentTo(t))))})) +
    ("gain" -> (S/NP, λ {e: Energy => ModifyEnergy(Self, Plus(e.amount))})) +
    (Seq("gain", "gains") -> (((S\NP)/N)/Num, λ {num: Number => λ {a: Attribute => λ {t: TargetObject => ModifyAttribute(t, a, Plus(num))}}})) +
    ("give" -> (((S/N)/Adj)/NP, λ {t: Target => λ {o: Operation => λ {a: Attribute => ModifyAttribute(t, a, o)}}})) +
    ("hand" -> (NP\Adj, λ {p: TargetPlayer => Hand(p)})) +
    ("halve" -> Seq(
      (((S/PP)/Adv)/N, λ {a: Attribute => λ {r: Rounding => λ {t: Target => ModifyAttribute(t, a, Divide(Scalar(2), r))}}}),
      ((V/Adv)/N, λ {a: Attribute => λ {r: Rounding => CurriedAction({t: Target => ModifyAttribute(t, a, Divide(Scalar(2), r))})}})
    )) +
    (Seq("has", "have") -> Seq(
      ((S/N)/Adj, λ {c: Comparison => λ {a: Attribute => AttributeComparison(a, c)}}),
      (((S\NP)/N)/Adj, λ {o: Operation => λ {a: Attribute => λ {t: TargetObject => AttributeAdjustment(t, a, o)}}})
    )) +
    (Seq("health", "life") -> (N, Form(Health): SemanticState)) +
    (Seq("in", "of") -> (PP/NP, identity)) +
    ("in combat" -> (S\S, λ {t: AfterDestroyed => AfterDestroyed(t.target, Combat)})) +
    ("in play" -> (NP\N, λ {o: ObjectType => ObjectsInPlay(o)})) +
    ("is" -> (X|X, identity)) +
    ("it" -> (NP, Form(It): SemanticState)) +
    ("its" -> (Num/N, λ {a: Attribute => AttributeValue(ThisRobot, a)})) +
    ("its controller" -> (NP, Form(ControllerOf(It)): SemanticState)) +
    ("kernel" -> (N, Form(Kernel): SemanticState)) +
    ("less" -> (Adv\Num, λ {num: Number => Minus(num)})) +
    ("less than" -> (Adj/Num, λ {num: Number => LessThan(num)})) +
    ("more" -> (Adv\Num, λ {num: Number => Plus(num)})) +
    ("more than" -> (Adj/Num, λ {num: Number => GreaterThan(num)})) +
    ("must" -> (X/X, identity)) +
    ("number" -> (Num/PP, λP ({case c: Collection => Count(c)
                               case All(c)        => Count(c)}: PF))) +
    ("object".s -> (N, Form(AllObjects): SemanticState)) +
    ("or less" -> (Adj\Num, λ {num: Number => LessThanOrEqualTo(num)})) +
    ("or more" -> (Adj\Num, λ {num: Number => GreaterThanOrEqualTo(num)})) +
    ("play".s -> ((NP\N)\NP, λ {t: TargetPlayer => λ {c: CardType => CardPlay(t, c)}})) +
    ("played" -> (S\NP, λP ({case Choose(coll) => AfterPlayed(All(coll))
                              case t: TargetObject => AfterPlayed(t)}: PF))) +
    (Seq("power", "attack") -> (N, Form(Attack): SemanticState)) +
    ("reduce" -> (((S/PP)/PP)/N, λ {a: Attribute => λ {t: TargetObject => λ {num: Number => ModifyAttribute(t, a, Minus(num))}}})) +
    (("robot".s ++ "creature".s) -> (N, Form(Robot): SemanticState)) +
    ("(rounded down)" -> (Adv, Form(RoundedDown): SemanticState)) +
    ("(rounded up)" -> (Adv, Form(RoundedUp): SemanticState)) +
    ("set" -> (((S/PP)/PP)/N, λ {a: Attribute => λ {t: Target => λ {num: Number => SetAttribute(t, a, num)}}})) +
    ("speed" -> (N, Form(Speed): SemanticState)) +
    ("take control" -> (S/PP, λ {t: TargetObject => TakeControl(Self, t)})) +
    ("takes damage" -> (S\NP, λP ({case Choose(coll) => AfterDamageReceived(All(coll))
                                   case t: TargetObject => AfterDamageReceived(t)}: PF))) +
    ("to" -> Seq(
      (PP/NP, identity),
      (PP/Num, identity)
    )) +
    ("that" -> ((NP\N)/S, λ {c: Condition => λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(c))}})) +
    ("the" -> (X/X, identity)) +
    ("this" / Seq("robot", "creature") -> (NP, Form(ThisRobot): SemanticState)) +
    ("this" / Seq("robot's", "creature's") -> (NP/N, λ {a: Attribute => TargetAttribute(ThisRobot, a)})) +
    ("total" -> ((Num/PP)/N, λP {a: Attribute => λP ({case c: Collection => AttributeSum(c, a)
                                                      case All(c)        => AttributeSum(c, a)}: PF)})) +
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
}

case class PrefixedIntegerMatcher(prefix: String) extends TokenMatcher[Int] {
  def apply(str: String) = {
    try {
      if (str.startsWith(prefix)) {
        Seq(Integer.parseInt(str.stripPrefix(prefix)))
      } else {
        Nil
      }
    } catch {
      case nfe: NumberFormatException => Nil
    }
  }
}