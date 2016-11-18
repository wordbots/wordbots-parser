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
    val code = result.bestParse.map(_.semantic) match {
      case Some(Form(v: AstNode)) => CodeGenerator.generateJS(v)
      case _ => "(n/a)"
    }

    println(s"Input: $input")
    // println(s"Tokens: ${tokenizer(input).mkString("[\"", "\", \"", "\"]")}")
    println(s"Parse result: $output")
    println(s"Generated JS code: $code")

    // For debug purposes, output the best parse tree (if one exists) to SVG.
    result.bestParse.foreach(result => new PrintWriter("test.svg") { write(result.toSvg); close() })
  }

  private def tokenizer(str: String): IndexedSeq[String] = {
    str.trim.toLowerCase.split("\\s+|[.?!,]").filter("" !=)
  }
}

object Lexicon {
  type PF = PartialFunction[AstNode, AstNode] // Partial function literals have to be explicitly typed because scala compiler is dumb.

  val lexicon =  ParserDict[CcgCat]() +
    (Seq("a", "an") -> Seq(
      (NP/N, λ {o: ObjectType => Choose(ObjectsInPlay(o))}),
      (NP/NP, λ {c: Collection => Choose(c)})
    )) +
    ("adjacent" -> (NP/N, λ {o: ObjectType => ObjectsMatchingCondition(o, AdjacentTo(ThisRobot))})) +
    ("after attacking" -> (S\S, λ {a: Action => At(AfterAttack(ThisRobot), a)})) +
    (Seq("all", "each") -> Seq(
      (NP/N, λ {o: ObjectType => All(ObjectsInPlay(o))}),
      (NP/NP, λ {c: Collection => All(c)})
    )) +
    (Seq("all attributes", "all stats") -> (N, Form(AllAttributes): SemanticState)) +
    ("and" -> (((S/PP)/V)\V, λ {a1: CurriedAction => λ {a2: CurriedAction => λ {t: Target => And(a1.action(t), a2.action(t))}}})) +
    ("at" -> ((S/S)/NP, λ {t: Trigger => λ {a: Action => At(t, a)}})) +
    ("attacks" -> (S\NP, λ {o: TargetObject => AfterAttack(o)})) +
    ("beginning of each of your turns" -> (NP, Form(BeginningOfTurn(Self)): SemanticState)) +
    ("by" -> (PP/Num, identity)) +
    ("card in your hand" -> (NP, Form(CardsInHand): SemanticState)) +
    ("cards" -> Seq(
      (NP\Num, λ {num: Number => Cards(num)}),
      (NP/Adj, λ {num: Number => Cards(num)})
    )) +
    (Seq("control", "controls") -> ((NP\N)\NP, λ {p: TargetPlayer => λ {o: ObjectType => ObjectsMatchingCondition(o, ControlledBy(p))}})) +
    ("cost" -> (N, Form(Cost): SemanticState)) +
    ("damage" -> Seq(
      ((S/PP)\Num, λ {amount: Number => λ {t: Target => DealDamage(t, amount)}}),
      ((S\NP)\Num, λ {amount: Number => λ {t: Target => DealDamage(t, amount)}}),
      ((S/Adj)/PP, λ {t: Target => λ {amount: Number => DealDamage(t, amount)}}),
      ((S\NP)/Adj, λ {amount: Number => λ {t: Target => DealDamage(t, amount)}}),
      (S/PP, λ {t: Target => DealDamage(t, AttributeValue(ThisRobot, Attack))})  // (by default, a robot deals damage equal to its power)
    )) +
    (Seq("deal", "it deals", "takes") -> (X|X, identity)) +
    ("destroy" -> (S/NP, λ {t: Target => Destroy(t)})) +
    ("draw" -> (S/NP, λ {c: Cards => Draw(Self, c.num)})) +
    ("draw a card" -> (S, Form(Draw(Self, Scalar(1))): SemanticState)) +  // Special-cased because "a card" is ambiguous.
    ("discard" -> Seq(
      (S/NP, λ {c: Cards => Discard(Self, c.num)}),
      ((S/NP)\NP, λ {t: TargetPlayer => λ {c: Cards => Discard(t, c.num)}})
    )) +
    ("discard a card" -> Seq(  // Special-cased because "a card" is ambiguous.
      (S, Form(Discard(Self, Scalar(1))): SemanticState),
      (S\NP, λ {t: TargetPlayer => Discard(t, Scalar(1))})
    )) +
    ("double" -> Seq(
      ((S/PP)/N, λ {a: Attribute => λ {t: Target => ModifyAttribute(t, a, Multiply(Scalar(2)))}}),
      (V/N, λ {a: Attribute => CurriedAction({t: Target => ModifyAttribute(t, a, Multiply(Scalar(2)))})})
    )) +
    ("end of each turn" -> (NP, Form(EndOfTurn(AllPlayers)): SemanticState)) +
    ("energy" -> (N|Num, λ {amount: Number => Energy(amount)})) +
    ("equal" -> (Adj/PP, identity)) +
    ("gain" -> (S/N, λ {e: Energy => ModifyEnergy(Self, Plus(e.amount))})) +
    ("gains" -> (((S\NP)/N)/Num, λ {num: Number => λ {a: Attribute => λ {t: TargetObject => ModifyAttribute(t, a, Plus(num))}}})) +
    ("gains a second move action" -> (S\NP, λ {t: TargetObject => CanMoveAgain(t)})) +
    ("give" -> (((S/N)/Adj)/NP, λ {t: Target => λ {o: Operation => λ {a: Attribute => ModifyAttribute(t, a, o)}}})) +
    ("halve" -> Seq(
      (((S/PP)/Adv)/N, λ {a: Attribute => λ {r: Rounding => λ {t: Target => ModifyAttribute(t, a, Divide(Scalar(2), r))}}}),
      ((V/Adv)/N, λ {a: Attribute => λ {r: Rounding => CurriedAction({t: Target => ModifyAttribute(t, a, Divide(Scalar(2), r))})}})
    )) +
    ("has" -> ((S/N)/Adj, λ {c: Comparison => λ {a: Attribute => AttributeComparison(a, c)}})) +
    (Seq("health", "life") -> (N, Form(Health): SemanticState)) +
    ("in play" -> (NP\N, λ {o: ObjectType => ObjectsInPlay(o)})) +
    ("is" -> (X|X, identity)) +
    ("its" -> (Num/N, λ {a: Attribute => AttributeValue(ThisRobot, a)})) +
    ("kernel" -> (N, Form(Kernel): SemanticState)) +
    ("must" -> (X/X, identity)) +
    ("number" -> (Num/PP, λP ({case c: Collection => Count(c)
                               case All(c)        => Count(c)}: PF))) +
    ("or less" -> (Adj\Num, λ {num: Number => LessThanOrEqualTo(num)})) +
    ("or more" -> (Adj\Num, λ {num: Number => GreaterThanOrEqualTo(num)})) +
    ("played" -> (S\NP, λ {t: TargetObject => AfterPlayed(t)})) +
    (Seq("power", "attack") -> (N, Form(Attack): SemanticState)) +
    ("reduce" -> (((S/PP)/PP)/N, λ {a: Attribute => λ {t: TargetObject => λ {num: Number => ModifyAttribute(t, a, Minus(num))}}})) +
    (Seq("robot", "robots", "creature", "creatures") -> (N, Form(Robot): SemanticState)) +
    ("(rounded down)" -> (Adv, Form(RoundedDown): SemanticState)) +
    ("(rounded up)" -> (Adv, Form(RoundedUp): SemanticState)) +
    ("set" -> (((S/PP)/PP)/N, λ {a: Attribute => λ {t: Target => λ {num: Number => SetAttribute(t, a, num)}}})) +
    ("speed" -> (N, Form(Speed): SemanticState)) +
    ("takes damage" -> (S\NP, λ {t: TargetObject => AfterDamageReceived(t)})) +
    (Seq("to", "of") -> Seq(
      (PP/NP, identity),
      (PP/Num, identity)
    )) +
    ("that" -> ((NP\N)/S, λ {c: Condition => λ {o: ObjectType => ObjectsMatchingCondition(o, c)}})) +
    ("the" -> (X/X, identity)) +
    (Seq("this robot", "this creature") -> (NP, Form(ThisRobot): SemanticState)) +
    ("total" -> ((Num/PP)/N, λP {a: Attribute => λP ({case c: Collection => AttributeSum(c, a)
                                                      case All(c)        => AttributeSum(c, a)}: PF)})) +
    (Seq("when", "whenever") -> ((S/S)/S, λ {t: Trigger => λ {a: Action => At(t, a)}})) +
    (Seq("you", "yourself") -> (NP, Form(Self): SemanticState)) +
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