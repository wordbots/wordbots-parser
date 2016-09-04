package wordbots

import com.workday.montague.ccg._
import com.workday.montague.parser._
import com.workday.montague.semantics._

object Parser extends SemanticParser[CcgCat](Lexicon.lexicon)

object Lexicon {
  val lexicon =  ParserDict[CcgCat]() +
    (Seq("a", "an") -> Seq(
      (Num, Form(1)),
      (NP/N, λ {o: ObjectType => Choose(o, NoCondition)}),
      ((NP/Rel)/N, λ {o: ObjectType => λ {c: Condition => Choose(o, c)}}),
      (X/X, identity)
    )) +
    ("attack" -> (N, Form(Attack): SemanticState)) +
    (Seq("card", "cards") -> (N\Num, λ {num: Int => Cards(num)})) +
    ("damage" -> (N\Num, λ {amount: Int => Damage(amount)})) +
    ("deal" -> ((S/PP)/N, λ {d: Damage => λ {t: Target => DealDamage(t, d.amount)}})) +
    ("destroy" -> (S/NP, λ {t: Target => Destroy(t)})) +
    ("draw" -> (S/N, λ {c: Cards => Draw(Self, c.num)})) +
    ("discard" -> Seq(
      (S/N, λ {c: Cards => Discard(Self, c.num)}),
      ((S/N)\NP, λ {t: Target => λ {c: Cards => Discard(t, c.num)}})
    )) +
    ("energy" -> (N\Num, λ {amount: Int => Energy(amount)})) +
    ("gain" -> (S/N, λ {e: Energy => EnergyDelta(Self, Plus(e.amount))})) +
    ("give" -> (((S/N)/Adj)/NP, λ {t: Target => λ {d: Delta => λ {a: Attribute => AttributeDelta(t, a, d)}}})) +
    ("has" -> ((S/N)/Adj, λ {c: Comparison => λ {a: Attribute => AttributeComparison(a, c)}})) +
    ("health" -> (N, Form(Health): SemanticState)) +
    ("kernel" -> (N, Form(Kernel): SemanticState)) +
    ("must" -> (X/X, identity)) +
    ("or less" -> (Adj\Num, λ {num: Int => LessThanOrEqualTo(num)})) +
    ("or more" -> (Adj\Num, λ {num: Int => GreaterThanOrEqualTo(num)})) +
    ("robot" -> (N, Form(Robot): SemanticState)) +
    ("speed" -> (N, Form(Speed): SemanticState)) +
    ("to" -> (PP/NP, identity)) +
    ("that" -> (Rel/S, identity)) +
    (Seq("you", "yourself") -> (NP, Form(Self): SemanticState)) +
    ("your opponent" -> (NP, Form(Opponent): SemanticState)) +
    (IntegerMatcher -> (Num, {i: Int => Form(i)})) +
    (PrefixedIntegerMatcher("+") -> (Adj, {i: Int => Form(Plus(i))})) +
    (PrefixedIntegerMatcher("-") -> (Adj, {i: Int => Form(Minus(i))}))
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
