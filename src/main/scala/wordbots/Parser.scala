package wordbots

import java.io.PrintWriter

import com.workday.montague.ccg._
import com.workday.montague.parser._
import com.workday.montague.semantics._
import com.workday.montague.semantics.FunctionReaderMacro.λ

object Parser extends SemanticParser[CcgCat](Lexicon.lexicon) {
  override def main(args: Array[String]): Unit = {
    val input = args.mkString(" ")
    val result: SemanticParseResult[CcgCat] = parse(input)

    val output = result.bestParse.map(p => s"${p.semantic.toString} [${p.syntactic.toString}]").getOrElse("(failed to parse)")
    val code = result.bestParse.map(_.semantic) match {
      case Some(Form(v)) => CodeGenerator.generateJS(v.asInstanceOf[AstNode])
      case _ => "(n/a)"
    }

    println(s"Input: $input")
    println(s"Parse result: $output")
    println(s"Generated JS code: $code")

    // For debug purposes, output the best parse tree (if one exists) to SVG.
    result.bestParse.foreach(result => new PrintWriter("test.svg") { write(result.toSvg); close() })
  }
}

object Lexicon {

  val lexicon =  ParserDict[CcgCat]() +
    (Seq("a", "an") -> Seq(
      (NP/N, λ {o: ObjectType => Choose(ObjectsInPlay(o))}),
      (NP/NP, λ {c: Collection => Choose(c)})
    )) +
    (Seq("all") -> Seq(
      (NP/N, λ {o: ObjectType => All(ObjectsInPlay(o))}),
      (NP/NP, λ {c: Collection => All(c)})
    )) +
    (Seq("all attributes", "all stats") -> (N, Form(AllAttributes): SemanticState)) +
    ("attack" -> (N, Form(Attack): SemanticState)) +
    ("a card" -> (N, Form(Cards(Scalar(1))): SemanticState)) +
    (Seq("cards") -> Seq(
      (N\Num, λ {num: Number => Cards(num)}),
      (N/Adj, λ {num: Number => Cards(num)})
    )) +
    (Seq("control", "controls") -> ((NP\N)\NP, λ {p: Player => λ {o: ObjectType => ObjectsMatchingCondition(o, ControlledBy(p))}})) +
    ("damage" -> Seq(
      ((S/PP)\Num, λ {amount: Number => λ {t: Target => DealDamage(t, amount)}}),
      ((S/Adj)/PP, λ {t: Target => λ {amount: Number => DealDamage(t, amount)}})
    )) +
    ("deal" -> (S/S, identity)) +
    ("destroy" -> (S/NP, λ {t: Target => Destroy(t)})) +
    ("draw" -> (S/N, λ {c: Cards => Draw(Self, c.num)})) +
    ("discard" -> Seq(
      (S/N, λ {c: Cards => Discard(Self, c.num)}),
      ((S/N)\NP, λ {t: Target => λ {c: Cards => Discard(t, c.num)}})
    )) +
    ("energy" -> (N|Num, λ {amount: Number => Energy(amount)})) +
    ("equal" -> (Adj/PP, identity)) +
    ("gain" -> (S/N, λ {e: Energy => EnergyDelta(Self, Plus(e.amount))})) +
    ("give" -> (((S/N)/Adj)/NP, λ {t: Target => λ {d: Delta => λ {a: Attribute => AttributeDelta(t, a, d)}}})) +
    ("has" -> ((S/N)/Adj, λ {c: Comparison => λ {a: Attribute => AttributeComparison(a, c)}})) +
    ("health" -> (N, Form(Health): SemanticState)) +
    ("in play" -> (NP\N, λ {o: ObjectType => ObjectsInPlay(o)})) +
    ("kernel" -> (N, Form(Kernel): SemanticState)) +
    ("must" -> (X/X, identity)) +
    ("number" -> (Num/PP, λ {c: Collection => Count(c)})) +
    ("or less" -> (Adj\Num, λ {num: Number => LessThanOrEqualTo(num)})) +
    ("or more" -> (Adj\Num, λ {num: Number => GreaterThanOrEqualTo(num)})) +
    ("power" -> (N, Form(Attack): SemanticState)) +
    (Seq("robot", "robots", "creature", "creatures") -> (N, Form(Robot): SemanticState)) +
    ("set" -> (((S/PP)/PP)/N, λ {a: Attribute => λ {t: Target => λ {num: Number => SetAttribute(t, a, num)}}})) +
    ("speed" -> (N, Form(Speed): SemanticState)) +
    (Seq("to", "of") -> Seq(
      (PP/NP, identity),
      (PP/Num, identity)
    )) +
    ("that" -> ((NP\N)/S, λ {c: Condition => λ {o: ObjectType => ObjectsMatchingCondition(o, c)}})) +
    ("the" -> (X/X, identity)) +
    ("total" -> ((Num/PP)/N, λ {a: Attribute => λ {c: Collection => AttributeSum(c, a)}})) +
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