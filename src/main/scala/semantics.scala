package wordbots

sealed trait Action
case class AttributeDelta(target: Target, attribute: Attribute, delta: Delta) extends Action
case class DealDamage(target: Target, num: Int) extends Action
case class Destroy(target: Target) extends Action
case class Discard(target: Target, num: Int) extends Action
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
