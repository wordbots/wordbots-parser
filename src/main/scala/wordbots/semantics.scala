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
case class TakeControl(player: TargetPlayer, target: TargetObject) extends Action

sealed trait PassiveAbility extends AstNode
case class ApplyEffect(target: Target, effect: Effect) extends PassiveAbility
case class AttributeAdjustment(target: Target, attribute: Attribute, operation: Operation) extends PassiveAbility
case class FreezeAttribute(target: Target, attribute: Attribute) extends PassiveAbility

sealed trait Effect extends Label
case object CannotAttack extends Effect

sealed trait Trigger extends AstNode
case class AfterAttack(target: TargetObject) extends Trigger
case class AfterDamageReceived(target: TargetObject) extends Trigger
case class AfterDestroyed(target: TargetObject, cause: Event = AnyEvent) extends Trigger
case class AfterPlayed(Target: TargetObject) extends Trigger
case class BeginningOfTurn(player: TargetPlayer) extends Trigger
case class EndOfTurn(player: TargetPlayer) extends Trigger

sealed trait Target extends AstNode
sealed trait TargetObject extends Target
case class Choose(collection: Collection) extends TargetObject
case class All(collection: Collection) extends TargetObject
case object ThisRobot extends TargetObject
case object It extends TargetObject
sealed trait TargetPlayer extends Target
case object Self extends TargetPlayer
case object Opponent extends TargetPlayer
case object AllPlayers extends TargetPlayer
case class ControllerOf(t: TargetObject) extends TargetPlayer

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
case class GreaterThan(num: Number) extends Comparison
case class GreaterThanOrEqualTo(num: Number) extends Comparison
case class LessThan(num: Number) extends Comparison
case class LessThanOrEqualTo(num: Number) extends Comparison

sealed trait Number extends AstNode
case class Scalar(num: Int) extends Number
case class Count(collection: Collection) extends Number
case class AttributeSum(collection: Collection, attribute: Attribute) extends Number
case class AttributeValue(obj: TargetObject, attribute: Attribute) extends Number

sealed trait Collection extends AstNode
case object AllTiles extends Collection
case class CardsInHand(player: TargetPlayer, cardType: CardType = AnyCard) extends Collection
case class ObjectsInPlay(objectType: ObjectType) extends Collection
case class ObjectsMatchingConditions(objectType: ObjectType, conditions: Seq[Condition]) extends Collection

sealed trait CardType extends Label
case object AnyCard extends CardType
case object Event extends CardType
sealed trait ObjectType extends CardType
case object Robot extends ObjectType
case object Kernel extends ObjectType
case object Structure extends ObjectType
case object AllObjects extends ObjectType

sealed trait Event extends Label
case object AnyEvent extends Event
case object Combat extends Event

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
case class TargetAttribute(target: TargetObject, attr: Attribute)
case class CardPlay(player: TargetPlayer, cardType: CardType)
