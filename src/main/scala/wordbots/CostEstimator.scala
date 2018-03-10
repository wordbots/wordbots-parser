package wordbots
/*
* scoring: good is positive, bad is negative. applying to enemy multiplies by -1.
* */

object CostEstimator {
  def estimateCost(node: AstNode, mode:Option[String]): String =
    {println("\n\n---new cost---");(baseCost(mode)(genericEstimate(node))).toString}

  private def baseCost(mode:Option[String]) : Float=>Float ={
    mode match{
      case Some("Object") => (x => x)
      case Some("Event") => (x => x)
      case _ => (x =>  x)
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
      case BecomeACopy(source, target)    => 1 * childCosts(node).product
      case CanAttackAgain(target)         => 1 * childCosts(node).product
      case CanMoveAgain(target)           => 1 * childCosts(node).product
      case CanMoveAndAttackAgain(target)  => 1 * childCosts(node).product
      case DealDamage(target, num)        => -1 * childCosts(node).product
      case Destroy(target)                => -2 * childCosts(node).product
      case Discard(target)                => -1 * childCosts(node).product
      case Draw(target,num)               => 1 * childCosts(node).product
      case EndTurn                        => 1
      case GiveAbility(target, ability)   => 1 * childCosts(node).product
      case ModifyAttribute(target, attr, op)=>1 * childCosts(node).product
      case ModifyEnergy(target, op)       => 1 * childCosts(node).product
      case MoveObject(target, dest)       => 1 * childCosts(node).product
      case PayEnergy(target, amount)      => -0.5f * childCosts(node).product
      case RemoveAllAbilities(target)     => 1 * childCosts(node).product
      case RestoreAttribute(target, Health, Some(num)) => 1 * childCosts(node).product
      case RestoreAttribute(target, Health, None) => 1 * childCosts(node).product
      case ReturnToHand(target)           => 1 * childCosts(node).product
      case SetAttribute(target, attr, num)=> 1 * childCosts(node).product
      case SwapAttributes(target, attr1, attr2) => 1 * childCosts(node).product
      case TakeControl(player, target)    => 2 * childCosts(node).product

      // Actions: Utility
      case SaveTarget(target)             => 1 * childCosts(node).product

      // Activated and triggered abilities
      case ActivatedAbility(action)       => 1 * childCosts(node).product
      case TriggeredAbility(trigger, Instead(action)) => 1 * childCosts(node).product
      case TriggeredAbility(trigger, action) => 1 * childCosts(node).product

      // Passive abilities
      case ApplyEffect(target, effect)    => 1 * childCosts(node).product
      case AttributeAdjustment(target, attr, op) => 1 * childCosts(node).product
      case FreezeAttribute(target, attr)  => 1 * childCosts(node).product
      case HasAbility(target, ability)    => 1 * childCosts(node).product

      // Effects
      case CanOnlyAttack(target)          => 1 * childCosts(node).product

      // Triggers
      case AfterAttack(targetObj, objectType) => 1 * childCosts(node).product
      case AfterCardPlay(targetPlayer, cardType) => 1 * childCosts(node).product
      case AfterDamageReceived(targetObj) => 1 * childCosts(node).product
      case AfterDestroyed(targetObj, cause) => 0.8f * childCosts(node).product
      case AfterMove(targetObj)           => 2 * childCosts(node).product
      case AfterPlayed(targetObj)         => 1 * childCosts(node).product
      case BeginningOfTurn(targetPlayer)  => 2 * childCosts(node).product
      case EndOfTurn(targetPlayer)        => 2 * childCosts(node).product

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
      case ControllerOf(targetObject)     => 1 * childCosts(node).product

      // Conditions
      case AdjacentTo(obj)                => 1 * childCosts(node).product
      case AttributeComparison(attr, comp)=> 1 * childCosts(node).product
      case ControlledBy(player)           => 1 * childCosts(node).product
      case HasProperty(property)          => 1 * childCosts(node).product
      case Unoccupied                     => 1
      case WithinDistanceOf(distance, obj)=> 1 * childCosts(node).sum

      // Global conditions
      case CollectionExists(coll)         =>  1 * childCosts(node).product
      case TargetHasProperty(target, property) => 1 * childCosts(node).product

      // Arithmetic operations
      case Constant(num)                  => 1 * childCosts(node).product
      case Plus(num)                      => 1 * childCosts(node).product
      case Minus(num)                     => -1 * childCosts(node).product
      case Multiply(num)                  => 2 * childCosts(node).product
      case Divide(num, RoundedDown)       => 1 * childCosts(node).product
      case Divide(num, RoundedUp)         => 1 * childCosts(node).product

      // Comparisons
      case EqualTo(num)                   => 0.5f * childCosts(node).product
      case GreaterThan(num)               => 1 * childCosts(node).product
      case GreaterThanOrEqualTo(num)      => 1 * childCosts(node).product
      case LessThan(num)                  => 1 * childCosts(node).product
      case LessThanOrEqualTo(num)         => 1 * childCosts(node).product


      // Numbers
      case Scalar(int)                    => scala.math.pow(childCosts(node).product,2).toFloat
      case AttributeSum(collection, attr) => 1 * childCosts(node).sum
      case AttributeValue(obj, attr)      => 1 * childCosts(node).sum
      case Count(collection)              => 1 * childCosts(node).sum
      case EnergyAmount(player)           => 1 * childCosts(node).sum

      // Collections
      case AllTiles                       => 1
      case CardsInHand(player, cardType)  => 1 * childCosts(node).product
      case ObjectsMatchingConditions(objType, conditions) => 1 * childCosts(node).product
      case Other(collection)              => 1 * childCosts(node).sum
      case TilesMatchingConditions(conditions) => 1 * childCosts(node).product

      // Labels
      case m: MultiLabel                  => {println("multilabel. what is it?");1}
      case l: Label                       => 1

      case _                              => 1 * childCosts(node).product
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

