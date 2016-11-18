package wordbots

sealed trait AstNode
sealed trait Label extends AstNode

case class At(trigger: Trigger, action: Action) extends AstNode

sealed trait Action extends AstNode
case class And(action1: Action, action2: Action) extends Action
case class CanMoveAgain(target: TargetObject) extends Action
case class DealDamage(target: Target, num: Number) extends Action
case class Destroy(target: Target) extends Action
case class Discard(target: TargetPlayer, num: Number) extends Action
case class Draw(target: TargetPlayer, num: Number) extends Action
case class ModifyAttribute(target: Target, attribute: Attribute, operation: Operation) extends Action
case class ModifyEnergy(target: Target, operation: Operation) extends Action
case class SetAttribute(target: Target, attribute: Attribute, num: Number) extends Action

sealed trait Trigger extends AstNode
case class AfterAttack(target: TargetObject) extends Trigger
case class AfterDamageReceived(target: TargetObject) extends Trigger
case class AfterPlayed(Target: TargetObject) extends Trigger
case class BeginningOfTurn(player: TargetPlayer) extends Trigger
case class EndOfTurn(player: TargetPlayer) extends Trigger

sealed trait Target extends AstNode
sealed trait TargetObject extends Target
case class Choose(collection: Collection) extends TargetObject
case class All(collection: Collection) extends TargetObject
case object ThisRobot extends TargetObject
sealed trait TargetPlayer extends Target
case object Self extends TargetPlayer
case object Opponent extends TargetPlayer
case object AllPlayers extends TargetPlayer

sealed trait Condition extends AstNode
case class AdjacentTo(obj: TargetObject) extends Condition
case class AttributeComparison(attribute: Attribute, comparison: Comparison) extends Condition
case class ControlledBy(player: TargetPlayer) extends Condition

sealed trait Operation extends AstNode
case class Plus(num: Number) extends Operation
case class Minus(num: Number) extends Operation
case class Multiply(num: Number) extends Operation
case class Divide(num: Number, rounding: Rounding) extends Operation

sealed trait Comparison extends AstNode
case class GreaterThanOrEqualTo(num: Number) extends Comparison
case class LessThanOrEqualTo(num: Number) extends Comparison

sealed trait Number extends AstNode
case class Scalar(num: Int) extends Number
case class Count(collection: Collection) extends Number
case class AttributeSum(collection: Collection, attribute: Attribute) extends Number
case class AttributeValue(obj: TargetObject, attribute: Attribute) extends Number

sealed trait Collection extends AstNode
case class CardsInHand(player: TargetPlayer) extends Collection
case class ObjectsInPlay(objectType: ObjectType) extends Collection
case class ObjectsMatchingCondition(objectType: ObjectType, condition: Condition) extends Collection

sealed trait ObjectType extends Label
case object Robot extends ObjectType
case object Kernel extends ObjectType

sealed trait Attribute extends Label
case object Attack extends Attribute
case object Cost extends Attribute
case object Health extends Attribute
case object Speed extends Attribute
case object AllAttributes extends Attribute

sealed trait Rounding extends Label
case object RoundedUp extends Rounding
case object RoundedDown extends Rounding

case class CurriedAction(action: Target => Action)

// These container classes are used to store state mid-parse but not expressed in the final parsed AST.
case class Cards(num: Number)
case class Damage(amount: Number)
case class Energy(amount: Number)
case class Hand(player: TargetPlayer)
case class Turn(player: TargetPlayer)
