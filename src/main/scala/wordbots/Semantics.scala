package wordbots

import scala.collection.mutable

// scalastyle:off number.of.methods
object Semantics {
  sealed trait ParseNode extends Product
  sealed trait AstNode extends ParseNode {
    def depthFirstTraverse: Seq[AstNode] = {
      val frontier = mutable.Queue(this)
      val nodesTraversed = mutable.ListBuffer[AstNode]()

      while (frontier.nonEmpty) {
        val node: AstNode = frontier.dequeue()
        val children: Seq[AstNode] = node.productIterator.toSeq.flatMap {
          case node: AstNode => Seq(node)
          case nodes: Seq[_] => nodes.collect { case a: AstNode => a }
          case _             => Seq.empty
        }

        frontier.enqueue(children: _*)
        nodesTraversed += node
      }

      nodesTraversed
    }

    def size: Int = depthFirstTraverse.length
  }

  sealed trait Label extends AstNode {
    lazy val name: String = getClass.getSimpleName.toLowerCase.split('$')(0)
  }
  trait MultiLabel extends Label { def labels: Seq[Label] }

  sealed trait Action extends AstNode
    case class MultipleActions(actions: Seq[Action]) extends Action
    object And { def apply(actions: Action*): MultipleActions = MultipleActions(actions) }

    case class ForEach(collection: Collection, action: Action) extends Action
    case class If(condition: GlobalCondition, action: Action) extends Action
    case class Instead(action: Action) extends Action
    case class Until(duration: Duration, action: Action) extends Action

    case class Become(source: TargetObject, target: TargetCard) extends Action
    case class CanAttackAgain(target: TargetObject) extends Action
    case class CanMoveAgain(target: TargetObject) extends Action
    case class CanMoveAndAttackAgain(target: TargetObject) extends Action
    case class DealDamage(target: TargetObjectOrPlayer, num: Number) extends Action
    case class Destroy(target: TargetObject) extends Action
    case class Discard(target: TargetCard) extends Action
    case class Draw(target: TargetPlayer, num: Number) extends Action
    case object EndTurn extends Action
    case class GiveAbility(target: TargetObject, ability: Ability) extends Action
    case class ModifyAttribute(target: TargetObjectCardOrPlayer, attribute: Attribute, operation: Operation) extends Action
    case class ModifyEnergy(target: TargetPlayer, operation: Operation) extends Action
    case class MoveCardsToHand(target: TargetCard, player: TargetPlayer) extends Action
    case class MoveObject(target: TargetObject, dest: TargetObjectOrTile) extends Action
    case class PayEnergy(target: TargetPlayer, amount: Number) extends Action
    case class RemoveAllAbilities(target: TargetObject) extends Action
    case class ReturnToHand(target: TargetObject, player: Option[TargetPlayer] = None) extends Action
    case class RestoreAttribute(target: TargetObjectOrPlayer, attribute: Attribute, num: Option[Number] = None) extends Action {
      attribute match {
        case Health =>
        case _ => throw new ClassCastException(s"Can't restore $attribute")
      }
    }
    case class RewriteText(target: TargetCard, textReplacements: Map[String, String]) extends Action
    case class SetAttribute(target: TargetObjectCardOrPlayer, attribute: Attribute, num: Number) extends Action
    case class ShuffleCardsIntoDeck(target: TargetCard, player: TargetPlayer) extends Action
    case class SpawnObject(target: TargetCard, dest: TargetTile, owner: TargetPlayer = Self) extends Action {
      target match {
        case c: GeneratedCard if c.name.isEmpty =>
          throw new ClassCastException("Can't spawn a GeneratedCard without a name")
        case _ =>
      }
    }
    case class SwapAttributes(target: TargetObject, attr1: Attribute, attr2: Attribute) extends Action
    case class SwapPositions(target1: TargetObject, target2: TargetObject) extends Action
    case class TakeControl(player: TargetPlayer, target: TargetObject) extends Action
    case class WinGame(player: TargetPlayer) extends Action

    case class SaveTarget(target: Target) extends Action

  sealed trait Ability extends AstNode
    case class MultipleAbilities(abilities: Seq[Ability]) extends Ability
    case class TriggeredAbility(trigger: Trigger, action: Action) extends Ability
    case class ActivatedAbility(action: Action) extends Ability
    sealed trait PassiveAbility extends Ability {
      def conditionOn(condition: GlobalCondition): PassiveAbility = this match {
        case ApplyEffect(target, effect) => ApplyEffect(ConditionTargetOn(target, condition), effect)
        case AttributeAdjustment(target, attribute, operation) => AttributeAdjustment(ConditionTargetOn(target, condition), attribute, operation)
        case HasAbility(target, ability) => HasAbility(ConditionTargetOn(target, condition), ability)
        case _ => throw new ClassCastException(s"Can't condition $this on $condition is meaningless!")
      }
    }
    case class ApplyEffect(target: TargetObjectOrCard, effect: Effect) extends PassiveAbility
    case class AttributeAdjustment(target: TargetObjectOrCard, attribute: Attribute, operation: Operation) extends PassiveAbility
    case class ConditionalAction(condition: GlobalCondition, action: Action) extends PassiveAbility
    case class FreezeAttribute(target: Target, attribute: Attribute) extends PassiveAbility
    case class HasAbility(target: TargetObjectOrCard, ability: Ability) extends PassiveAbility

  sealed trait Effect extends AstNode
    case object CanMoveOverObjects extends Effect with Label
    case object CannotActivate extends Effect with Label
    case object CannotAttack extends Effect with Label
    case object CannotFightBack extends Effect with Label
    case object CannotMove extends Effect with Label
    case class CanOnlyAttack(target: TargetObject) extends Effect
    case class CannotMoveTo(tiles: TileCollection) extends Effect

  sealed trait Trigger extends AstNode
    case class AfterAttack(target: TargetObject, attackedObjectType: ObjectType = AllObjects) extends Trigger
    case class AfterAttackedBy(target: TargetObject, attackerObjectType: ObjectType = AllObjects) extends Trigger
    case class AfterCardDraw(target: TargetPlayer, cardType: CardType = AnyCard) extends Trigger
    case class AfterCardEntersDiscardPile(target: TargetPlayer, cardType: CardType = AnyCard) extends Trigger
    case class AfterCardPlay(target: TargetPlayer, cardType: CardType = AnyCard) extends Trigger  // When a given card type is played.
    case class AfterDamageReceived(target: TargetObject, causingCardType: CardType = AnyCard) extends Trigger
    case class AfterDestroysOtherObject(targetObject: TargetObject, objectType: ObjectType = AllObjects) extends Trigger
    case class AfterDestroyed(target: TargetObject, cause: TriggerEvent = AnyEvent) extends Trigger
    case class AfterMove(Target: TargetObject) extends Trigger
    case class AfterPlayed(Target: TargetObject) extends Trigger  // When a specific card is played.
    case class BeginningOfTurn(player: TargetPlayer) extends Trigger
    case class EndOfTurn(player: TargetPlayer) extends Trigger

  sealed trait Target extends AstNode
    sealed trait TargetObjectCardOrPlayer extends Target
    sealed trait TargetObjectOrCard extends TargetObjectCardOrPlayer
    sealed trait TargetObjectOrPlayer extends TargetObjectCardOrPlayer
    sealed trait TargetObjectOrTile extends Target

    case class ConditionTargetOn(target: TargetObjectOrCard, condition: GlobalCondition) extends TargetObjectOrCard

    sealed trait TargetObject extends TargetObjectOrCard with TargetObjectOrPlayer with TargetObjectOrTile
      case class ChooseO(collection: ObjectCollection) extends TargetObject
      case class AllO(collection: ObjectCollection) extends TargetObject
      case class RandomO(num: Number, collection: ObjectCollection) extends TargetObject
      case class UnionO(targets: Seq[TargetObject]) extends TargetObject
      case object ThisObject extends TargetObject
      case object ItO extends TargetObject  // (Salient object)
      case object That extends TargetObject  // (Salient object, but preferring the undergoer of the action over the agent)
      case object They extends TargetObject  // (Salient object, but preferring the current object in an iteration over a collection)
      case object SavedTargetObject extends TargetObject
    sealed trait TargetCard extends TargetObjectOrCard
      case class CopyOfC(objToCopy: TargetObject) extends TargetCard
      case class ChooseC(collection: CardCollection) extends TargetCard
      case class AllC(collection: CardCollection) extends TargetCard
      case class RandomC(num: Number, collection: CardCollection) extends TargetCard
      case class GeneratedCard(
                                cardType: ObjectType,
                                attributes: Seq[AttributeAmount] = Seq.empty,
                                name: Option[String] = None
                              ) extends TargetCard {
        def getAttributeAmount(attribute: Attribute): Seq[Number] = {
          // This returns a Seq[Number] rather than Option[Number] because it's possible to, e.g.
          // write something like "a robot with 2 attack and 3 attack".
          // This kind of situation should pass parsing and fail validation, so we must be able to represent it.
          attributes.filter(_.attr == attribute).map(_.amount)
        }
      }
    sealed trait TargetTile extends TargetObjectOrTile
      case class ChooseT(collection: TileCollection) extends TargetTile
      case class AllT(collection: TileCollection) extends TargetTile
      case class RandomT(num: Number, collection: TileCollection) extends TargetTile
    sealed trait TargetPlayer extends Target with TargetObjectOrPlayer
      case object Self extends TargetPlayer
      case object Opponent extends TargetPlayer
      case object AllPlayers extends TargetPlayer with Collection
      case object ItP extends TargetPlayer  // (Salient player)
      case object TheyP extends TargetPlayer  // (Salient player, but preferring the current player in an iteration over a collection)
      case class ControllerOf(t: TargetObject) extends TargetPlayer

  sealed trait Condition extends AstNode
    sealed trait CardCondition extends Condition
    sealed trait ObjectCondition extends Condition
      case class AdjacentTo(obj: TargetObjectOrTile) extends ObjectCondition
      case class AttributeComparison(attribute: Attribute, comparison: Comparison) extends ObjectCondition with CardCondition
      case class ControlledBy(player: TargetPlayer) extends ObjectCondition
      case class ExactDistanceFrom(distance: Number, obj: TargetObject) extends ObjectCondition
      case class HasProperty(property: Property) extends ObjectCondition
      case object Unoccupied extends ObjectCondition
      case class WithinDistanceOf(distance: Number, obj: TargetObject) extends ObjectCondition

  sealed trait GlobalCondition extends AstNode
    case class CollectionCountComparison(coll: Collection, comparison: Comparison) extends GlobalCondition
    case class CollectionExists(coll: Collection) extends GlobalCondition
    case class TargetHasProperty(target: TargetObject, property: Property) extends GlobalCondition
    case class TargetMeetsCondition(target: TargetObject, condition: ObjectCondition) extends GlobalCondition

  sealed trait Operation extends AstNode {
    def times(factor: Number): Operation = this match {
      case Constant(num) => Constant(Times(num, factor))
      case Plus(num) => Plus(Times(num, factor))
      case Minus(num) => Minus(Times(num, factor))
      case _ => throw new ClassCastException(s"Multiplying $this by $factor is meaningless!")
    }
  }
    case class Constant(num: Number) extends Operation
    case class Plus(num: Number) extends Operation
    case class Minus(num: Number) extends Operation
    case class Multiply(num: Number) extends Operation
    case class Divide(num: Number, rounding: Rounding) extends Operation

  sealed trait Comparison extends AstNode
    case class EqualTo(num: Number) extends Comparison
    case class GreaterThan(num: Number) extends Comparison
    case class GreaterThanOrEqualTo(num: Number) extends Comparison
    case object IsEven extends Comparison
    case object IsOdd extends Comparison
    case class LessThan(num: Number) extends Comparison
    case class LessThanOrEqualTo(num: Number) extends Comparison

  sealed trait Number extends AstNode
    case class Scalar(num: Int) extends Number
    case class AttributeSum(collection: ObjectOrCardCollection, attribute: SingleAttribute) extends Number
    case class AttributeValue(obj: TargetObjectOrCard, attribute: SingleAttribute) extends Number
    case class Count(collection: Collection) extends Number
    case class EnergyAmount(player: TargetPlayer) extends Number
    case class MaximumEnergyAmount(player: TargetPlayer) extends Number
    case object ThatMuch extends Number  // salient amount

    case class Times(num1: Number, num2: Number) extends Number

  sealed trait Collection extends AstNode
    sealed trait ObjectOrCardCollection extends Collection

    sealed trait CardCollection extends ObjectOrCardCollection
      case class CardsInDiscardPile(player: TargetPlayer, cardType: CardType = AnyCard, conditions: Seq[CardCondition] = Seq()) extends CardCollection
      case class CardsInHand(player: TargetPlayer, cardType: CardType = AnyCard, conditions: Seq[CardCondition] = Seq()) extends CardCollection
    sealed trait ObjectCollection extends ObjectOrCardCollection with TargetObject
      object ObjectsInPlay { def apply(objectType: ObjectType): ObjectCollection = ObjectsMatchingConditions(objectType, Seq()) }
      case class ObjectsMatchingConditions(objectType: ObjectType, conditions: Seq[ObjectCondition]) extends ObjectCollection
      case class Other(collection: ObjectCollection) extends ObjectCollection
    sealed trait TileCollection extends Collection with TargetTile
      case object AllTiles extends TileCollection
      case class TilesMatchingConditions(conditions: Seq[ObjectCondition]) extends TileCollection

  sealed trait CardType extends Label
    case object AnyCard extends CardType
    case object Event extends CardType
    sealed trait ObjectType extends CardType
      case object Robot extends ObjectType
      case object Kernel extends ObjectType
      case object Structure extends ObjectType
      case object AllObjects extends ObjectType
      case class MultipleObjectTypes(labels: Seq[ObjectType]) extends ObjectType with MultiLabel

  sealed trait TriggerEvent extends Label
    case object AnyEvent extends TriggerEvent
    case object Combat extends TriggerEvent

  sealed trait Attribute extends Label
    sealed trait SingleAttribute extends Attribute
      case object Attack extends SingleAttribute
      case object Cost extends SingleAttribute
      case object Health extends SingleAttribute
      case object Speed extends SingleAttribute
    case class MultipleAttributes(labels: Seq[Attribute]) extends Attribute with MultiLabel
      case object AllAttributes extends Attribute

  sealed trait Property extends Label
    case object AttackedLastTurn extends Property
    case object AttackedThisTurn extends Property
    case object MovedLastTurn extends Property
    case object MovedThisTurn extends Property
    case object IsDamaged extends Property
    case object IsDestroyed extends Property

  sealed trait Duration extends AstNode
    case class TurnsPassed(turns: Int) extends Duration

  sealed trait Rounding extends Label
    case object RoundedUp extends Rounding
    case object RoundedDown extends Rounding

  case class AttributeAmount(amount: Number, attr: Attribute) extends AstNode

  // The below container classes are used to store state mid-parse but not expressed in the final parsed AST.

  sealed trait IntermediateNode extends ParseNode

  // Nullary containers:
  case object AllEnergy extends IntermediateNode
  case object ItsOwnersHand extends IntermediateNode

  // Unary containers:
  case class Cards(num: Number) extends IntermediateNode
  case class CardComparison(comp: Comparison) extends IntermediateNode
  case class Damage(amount: Number) extends IntermediateNode
  case class Deck(player: TargetPlayer) extends IntermediateNode
  case class DiscardPile(player: TargetPlayer) extends IntermediateNode
  case class EnemyObject(objectType: ObjectType) extends IntermediateNode  // used, e.g. in parsing "whenever this robot destroys an enemy object"
  case class Energy(amount: Number) extends IntermediateNode
  case class EnergyComparison(comp: Comparison) extends IntermediateNode
  case class Hand(player: TargetPlayer) extends IntermediateNode
  case class Life(amount: Number) extends IntermediateNode
  case class Name(name: String) extends IntermediateNode
  case class RelativeTo(obj: TargetObject) extends IntermediateNode
  case class Spaces(num: Number) extends IntermediateNode
  case class Text(text: String) extends IntermediateNode
  case class Turn(player: TargetPlayer) extends IntermediateNode
  case class WithinDistance(spaces: Number) extends IntermediateNode

  // Binary containers:
  case class AttributeOperation(op: Operation, attr: Attribute) extends IntermediateNode
  case class CardPlay(player: TargetPlayer, cardType: CardType) extends IntermediateNode
  case class RandomCards(num: Number, cardType: CardType) extends IntermediateNode
  case class TargetAttribute(target: TargetObjectOrPlayer, attr: Attribute) extends IntermediateNode {
    if (target.isInstanceOf[TargetPlayer] && attr != Health) {
      throw new ClassCastException(s"expected Health, got $attr")
    }
  }
  case class TextReplacement(from: Text, to: Text) extends IntermediateNode
}
