package wordbots
/*
* scoring: good is positive, bad is negative. applying to enemy multiplies by -1.
* */

object CostEstimator {
  def estimateCost(node: AstNode, mode:Option[String]): String =
    (baseCost(mode)(genericEstimate(node))).toString

  private def baseCost(mode:Option[String]) : Float=>Float ={
    mode match{
      case Some("Object") => (x => 1 + 1.5f * x)//objects are more expensive than events and have a higher base cost
      case Some("Event") => (x => 0 + 1 * x)
      case _ => (x => 1 * x)
    }
  }

  // scalastyle:off method.length
  // scalastyle:off cyclomatic.complexity
  //scalastyle:off magic.number
  //estimate the cost of an AST node
  private def astEst(node: AstNode): Float = {
    node match{
      // Meta
      case If(condition, action)           => 1 * childCosts(node).product
      case MultipleActions(actions)        => 1 * childCosts(node).sum
      case MultipleAbilities(abilities)    => 1 * childCosts(node).sum
      case Until(TurnsPassed(num), action) => 1 * childCosts(node).sum

      // Actions: Normal
      case BecomeACopy(source, target)    => 1 * childCosts(node).sum
      case CanAttackAgain(target)         => 1 * childCosts(node).sum
      case CanMoveAgain(target)           => 1 * childCosts(node).sum
      case CanMoveAndAttackAgain(target)  => 1 * childCosts(node).sum
      case DealDamage(target, num)        => -1 * childCosts(node).product
      case Destroy(target)                => -2 * childCosts(node).sum
      case Discard(target)                => -1 * childCosts(node).sum
      case Draw(target,num)               => 1 * childCosts(node).product
      case EndTurn                        => 1
      case GiveAbility(target, ability)   => 1 * childCosts(node).product
      case ModifyAttribute(target, attr, op)=>1 * childCosts(node).product
      case ModifyEnergy(target, op)       => 1 * childCosts(node).product
      case MoveObject(target, dest)       => 1 * childCosts(node).sum
      case PayEnergy(target, amount)      => -0.5f * childCosts(node).product
      case RemoveAllAbilities(target)     => 1 * childCosts(node).sum
      case RestoreAttribute(target, Health, Some(num)) => 1 * childCosts(node).sum
      case RestoreAttribute(target, Health, None) => 1 * childCosts(node).sum
      case ReturnToHand(target)           => 1 * childCosts(node).sum
      case SetAttribute(target, attr, num)=> 1 * childCosts(node).product
      case SwapAttributes(target, attr1, attr2) => 1 * childCosts(node).sum//TODO FIX
      case TakeControl(player, target)    => 2 * childCosts(node).product

      // Actions: Utility
      case SaveTarget(target)             => 1 * childCosts(node).sum

      // Activated and triggered abilities
      case ActivatedAbility(action)       => 1 * childCosts(node).sum
      case TriggeredAbility(trigger, Instead(action)) => 1 * childCosts(node).sum
      case TriggeredAbility(trigger, action) => 1 * childCosts(node).sum

      // Passive abilities
      case ApplyEffect(target, effect)    => 1 * childCosts(node).sum
      case AttributeAdjustment(target, attr, op) => 1 * childCosts(node).sum
      case FreezeAttribute(target, attr)  => 1 * childCosts(node).sum
      case HasAbility(target, ability)    => 1 * childCosts(node).sum

      // Effects
      case CanOnlyAttack(target)          => 1 * childCosts(node).sum

      // Triggers
      case AfterAttack(targetObj, objectType) => 1 * childCosts(node).sum
      case AfterCardPlay(targetPlayer, cardType) => 1 * childCosts(node).sum
      case AfterDamageReceived(targetObj) => 1 * childCosts(node).sum
      case AfterDestroyed(targetObj, cause) => 1 * childCosts(node).sum
      case AfterMove(targetObj)           => 1 * childCosts(node).sum
      case AfterPlayed(targetObj)         => 1 * childCosts(node).sum
      case BeginningOfTurn(targetPlayer)  => 1 * childCosts(node).sum
      case EndOfTurn(targetPlayer)        => 1 * childCosts(node).sum

      // Target objects
      case ChooseO(collection)            => 2 * childCosts(node).sum
      case AllO(collection)               => 3 * childCosts(node).sum
      case RandomO(num, collection)       => 1 * childCosts(node).sum
      case ThisObject                     => 1
      case ItO                            => 1
      case ItP                            => 1
      case That                           => 1
      case They                           => 1
      case SavedTargetObject              => 1

      // Target cards
      case ChooseC(collection)            => 1 * childCosts(node).sum
      case AllC(collection)               => 1 * childCosts(node).sum
      case RandomC(num, collection)       => 0.5f * childCosts(node).sum

      // Target players
      case Self                           => 1
      case Opponent                       => -1
      case AllPlayers                     => 1
      case ControllerOf(targetObject)     => 1 * childCosts(node).sum

      // Conditions
      case AdjacentTo(obj)                => 1 * childCosts(node).sum
      case AttributeComparison(attr, comp)=> 1 * childCosts(node).product
      case ControlledBy(player)           => 1 * childCosts(node).sum
      case HasProperty(property)          => 1 * childCosts(node).sum
      case Unoccupied                     => 1
      case WithinDistanceOf(distance, obj)=> 1 * childCosts(node).sum

      // Global conditions
      case CollectionExists(coll)         =>  1 * childCosts(node).sum
      case TargetHasProperty(target, property) => 1 * childCosts(node).sum

      // Arithmetic operations
      case Constant(num)                  => 1 * childCosts(node).sum
      case Plus(num)                      => 1 * childCosts(node).sum
      case Minus(num)                     => -1 * childCosts(node).sum
      case Multiply(num)                  => 2 * childCosts(node).sum
      case Divide(num, RoundedDown)       => 1 * childCosts(node).sum
      case Divide(num, RoundedUp)         => 1 * childCosts(node).sum

      // Comparisons
      case EqualTo(num)                   => 0.5f * childCosts(node).sum
      case GreaterThan(num)               => 1 * childCosts(node).sum
      case GreaterThanOrEqualTo(num)      => 1 * childCosts(node).sum
      case LessThan(num)                  => 1 * childCosts(node).sum
      case LessThanOrEqualTo(num)         => 1 * childCosts(node).sum


      // Numbers
      case Scalar(int)                    => scala.math.pow(childCosts(node).sum,2).toFloat
      case AttributeSum(collection, attr) => 1 * childCosts(node).sum
      case AttributeValue(obj, attr)      => 1 * childCosts(node).sum
      case Count(collection)              => 1 * childCosts(node).sum
      case EnergyAmount(player)           => 1 * childCosts(node).sum

      // Collections
      case AllTiles                       => 1
      case CardsInHand(player, cardType)  => 1 * childCosts(node).sum
      case ObjectsMatchingConditions(objType, conditions) => 1 * childCosts(node).product
      case Other(collection)              => 1 * childCosts(node).sum
      case TilesMatchingConditions(conditions) => 1 * childCosts(node).sum

      // Labels
      case m: MultiLabel                  => {println("multilabel. what is it?");1}
      case l: Label                       => 1

      case _                              => 1 * childCosts(node).sum
    }
  }


  //try to calculate a cost for anything and everything
  private def genericEstimate(a:Any): Float ={
  val v = genericEstimateZ(a);println(a.toString + " has estimate " + v);v
  }

  private def genericEstimateZ(a:Any): Float = a match{
    case n:AstNode => astEst(n)//AST node, complex.
    case n:Int => n
    case c:Seq[Any] => c.map(child=>genericEstimate(child)).product //conditional series are multiplied - maybe?
    //case c:Seq[Action] => c.map(child=>genericEstimate(child)).sum  //hmm. might want to mult instead of add sometimes - configurable?
      // scalastyle:off regex
    case _=> println("error unknown type in generic estimate."); 0
    // scalastyle:on regex
    }

  //for each child of the node, run genericEstimate()
  private def childCosts(node: AstNode) :Iterator[Float]= {
    println("object " + node.toString + "has " + node.productArity + " children.");
    node.productIterator.map[Float](child => genericEstimate(child))
  }
}

