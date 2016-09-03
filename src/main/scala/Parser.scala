package wordbots

import com.workday.montague.ccg._
import com.workday.montague.parser._
import com.workday.montague.semantics._

object Parser extends SemanticParser[CcgCat](Lexicon.lexicon)

// Semantics

sealed trait Action
case class DealDamage(target: Target, num: Int) extends Action
case class Destroy(target: Target) extends Action
case class Draw(target: Target, num: Int) extends Action
case class EnergyDelta(target: Target, delta: Delta) extends Action

sealed trait Target
case class Choose(objectType: ObjectType, condition: Condition) extends Target
case object Self extends Target
case object Opponent extends Target

sealed trait Delta
case class Plus(num: Int) extends Delta
case class Minus(num: Int) extends Delta

sealed trait ObjectType
case object Robot extends ObjectType
case object Kernel extends ObjectType

sealed trait Condition
case object NoCondition extends Condition
case class AttributeComparison(attribute: Attribute, comparison: Comparison) extends Condition

sealed trait Attribute
case object Attack extends Attribute
case object Health extends Attribute
case object Speed extends Attribute

sealed trait Comparison
case class GreaterThanOrEqualTo(num: Int) extends Comparison
case class LessThanOrEqualTo(num: Int) extends Comparison

case class Cards(num: Int)
case class Damage(amount: Int)
case class Energy(amount: Int)

// Syntax

case object Adj extends TerminalCat { val category = "Adj" }
case object Num extends TerminalCat { val category = "#" }
case object Rel extends TerminalCat { val category = "Rel" }
case object Type extends TerminalCat { val category = "Type" }

// Lexicon

object Lexicon {
  val lexicon =  ParserDict[CcgCat]() +
    (Seq("a", "an") -> Seq(
      (Num, Form(1)),
      (N/Type, λ {o: ObjectType => Choose(o, NoCondition)}),
      ((N/Rel)/Type, λ {o: ObjectType => λ {c: Condition => Choose(o, c)}}),
      (X|X, identity)
    )) +
    (Seq("card", "cards") -> (N\Num, λ {num: Int => Cards(num)})) +
    ("damage" -> (N\Num, λ {amount: Int => Damage(amount)})) +
    ("deal" -> ((S/PP)/N, λ {d: Damage => λ {t: Target => DealDamage(t, d.amount)}})) +
    ("destroy" -> (S/N, λ {t: Target => Destroy(t)})) +
    ("draw" -> (S/N, λ {c: Cards => Draw(Self, c.num)})) +
    ("energy" -> (N\Num, λ {amount: Int => Energy(amount)})) +
    ("gain" -> (S/N, λ {e: Energy => EnergyDelta(Self, Plus(e.amount))})) +
    ("has" -> ((S/N)/Adj, λ {c: Comparison => λ {a: Attribute => AttributeComparison(a, c)}})) +
    ("kernel" -> (Type, Form(Kernel): SemanticState)) +
    ("or less" -> (Adj\Num, λ {num: Int => LessThanOrEqualTo(num)})) +
    ("or more" -> (Adj\Num, λ {num: Int => GreaterThanOrEqualTo(num)})) +
    ("robot" -> (Type, Form(Robot): SemanticState)) +
    ("speed" -> (N, Form(Speed): SemanticState)) +
    ("to" -> (PP/N, identity)) +
    ("that" -> (Rel/S, identity)) +
    (IntegerMatcher -> (Num, {i: Int => Form(i)}))
}
