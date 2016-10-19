package wordbots

sealed trait AstNode
sealed trait Label extends AstNode

sealed trait Action extends AstNode
case class AttributeDelta(target: Target, attribute: Attribute, delta: Delta) extends Action
case class DealDamage(target: Target, num: Number) extends Action
case class Destroy(target: Target) extends Action
case class Discard(target: Target, num: Number) extends Action
case class Draw(target: Target, num: Number) extends Action
case class EnergyDelta(target: Target, delta: Delta) extends Action
case class SetAttribute(target: Target, attribute: Attribute, num: Number) extends Action

sealed trait Target extends AstNode
case class Choose(collection: Collection) extends Target
case class All(collection: Collection) extends Target
sealed trait Player extends Target
case object Self extends Player
case object Opponent extends Player

sealed trait Condition extends AstNode
case object NoCondition extends Condition
case class AttributeComparison(attribute: Attribute, comparison: Comparison) extends Condition
case class ControlledBy(player: Player) extends Condition

sealed trait Delta extends AstNode
case class Plus(num: Number) extends Delta
case class Minus(num: Number) extends Delta

sealed trait Comparison extends AstNode
case class GreaterThanOrEqualTo(num: Number) extends Comparison
case class LessThanOrEqualTo(num: Number) extends Comparison

sealed trait Number extends AstNode
case class Scalar(num: Int) extends Number
case class Count(collection: Collection) extends Number
case class AttributeSum(collection: Collection, attribute: Attribute) extends Number

sealed trait Collection extends AstNode
case class ObjectsInPlay(objectType: ObjectType) extends Collection
case class ObjectsMatchingCondition(objectType: ObjectType, condition: Condition) extends Collection

sealed trait ObjectType extends Label
case object Robot extends ObjectType
case object Kernel extends ObjectType

sealed trait Attribute extends Label
case object Attack extends Attribute
case object Health extends Attribute
case object Speed extends Attribute
case object AllAttributes extends Attribute

case class Cards(num: Number)
case class Damage(amount: Number)
case class Energy(amount: Number)
