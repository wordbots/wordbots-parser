package wordbots

import scala.util.{Failure, Success, Try}

case class ValidationError(message: String) extends Exception(message)

sealed trait ValidationMode
case object ValidateObject extends ValidationMode
case object ValidateEvent extends ValidationMode
case object ValidateUnknownCard extends ValidationMode


case class AstValidator(mode: ValidationMode = ValidateUnknownCard) {
  val baseRules: Seq[AstRule] = Seq(
    NoUnimplementedRules,
    NoChooseInTriggeredAction,
    NoModifyingCostOfObjects,
    OnlyRestoreHealth,
    OnlyThisObjectPlayed,
    ValidGeneratedCard
  )

  val rules: Seq[AstRule] = mode match {
    case ValidateObject => baseRules :+ MustBeAbility
    case ValidateEvent => baseRules ++ Seq(MustBeAction, NoThis)
    case ValidateUnknownCard => baseRules
  }

  def validate(ast: AstNode): Try[Unit] = {
    Try(rules.foreach(_.validate(ast).get))
  }
}

/**
  * So why do you have to stick validateChildren() in your validate()?
  * why not just have a function that returns the success status for this node/, and apply it to all nodes in an outer loop?
  * well, it's because
  * 1) it's the same amount of code in the derived classes
  * 2) there's no explicit .asInstanceOf or .isInstanceOf involved this way - it's all done through match
  * 3) you end up writing the same code anyways, this way is more functional-y
  */
sealed trait AstRule {
  def validate(node: AstNode): Try[Unit]

  def validateChildren(rule: AstRule, parentNode: AstNode): Try[Unit] = {
    def validateRecursively(node: Any): Unit = {
      node match {
        case childNode: AstNode =>
          rule.validate(node.asInstanceOf[AstNode])
            .flatMap(_ => validateChildren(rule, childNode))
            .get
        case childNodes: Seq[_] =>
          childNodes.foreach(validateRecursively)
        case _ => ()
      }
    }

    Try {
      for (childNode <- parentNode.productIterator) {
        validateRecursively(childNode)
      }
    }
  }
}

object NoUnimplementedRules extends AstRule {
  override def validate(node: AstNode): Try[Unit] = {
    node match {
      case FreezeAttribute(_, _) => Failure(ValidationError("FreezeAttribute is not implemented yet."))
      case n: AstNode => validateChildren(this, n)
    }
  }
}

object NoChooseInTriggeredAction extends AstRule {
  object NoChooseTarget extends AstRule {
    override def validate(node: AstNode): Try[Unit] = {
      node match {
        case ChooseC(_) => Failure(ValidationError("Choosing targets not allowed for triggered actions."))
        case ChooseO(_) => Failure(ValidationError("Choosing targets not allowed for triggered actions."))
        case n: AstNode => validateChildren(this, n)
      }
    }
  }

  override def validate(node: AstNode): Try[Unit] = {
    node match {
      case TriggeredAbility(AfterPlayed(_), action) => Success()  // Choosing targets *is* allowed for AfterPlayed triggers.
      case TriggeredAbility(_, action) => validateChildren(NoChooseTarget, node)
      case n: AstNode => validateChildren(this, n)
    }
  }
}

object NoModifyingCostOfObjects extends AstRule {
  override def validate(node: AstNode): Try[Unit] = {
    node match {
      case AttributeAdjustment(target, Cost, _) =>
        target match {
          case _: TargetObject => Failure(ValidationError("Can't modify the cost of objects on the board."))
          case _ => validateChildren(this, node)
        }
      case n: AstNode => validateChildren(this, n)
    }
  }
}

object OnlyRestoreHealth extends AstRule {
  override def validate(node: AstNode): Try[Unit] = {
    node match {
      case RestoreAttribute(_, Health, _) => Success()
      case RestoreAttribute(_, a, _) => Failure(ValidationError(s"Only Health can be restored, not $a"))
      case n: AstNode => validateChildren(this, n)
    }
  }
}

object OnlyThisObjectPlayed extends AstRule {
  override def validate(node: AstNode): Try[Unit] = {
    node match {
      case AfterPlayed(ThisObject) => Success()
      case AfterPlayed(ItO) => Success()
      case AfterPlayed(_) => Failure(ValidationError("AfterPlayed can only refer to ThisObject or ItO."))
      case n: AstNode => validateChildren(this, n)
    }
  }
}

object MustBeAbility extends AstRule {
  override def validate(node: AstNode): Try[Unit] = {
    node match {
      case _: Ability => Success()
      case _ => Failure(ValidationError("Not a valid passive, triggered, or activated ability."))
    }
  }
}

object MustBeAction extends AstRule {
  override def validate(node: AstNode): Try[Unit] = {
    node match {
      case TriggeredAbility(_, _) => Failure(ValidationError("Events can't have triggered abilities."))
      case ActivatedAbility(_) => Failure(ValidationError("Events can't have activated abilities."))
      case _: PassiveAbility => Failure(ValidationError("Events can't have passive abilities."))
      case _ => Success()
    }
  }
}

object NoThis extends AstRule {
  override def validate(node: AstNode): Try[Unit] = {
    node match {
      case ThisObject => Failure(ValidationError(s"Events can't refer to the current object"))
      case n: AstNode => validateChildren(this, n)
    }
  }
}

//does the ObjMatchCond in this have exactly the conditions needed to specify a card?
object ValidGeneratedCard extends AstRule{

  // a comparison like "{attribute} == {number}"?
  val attributeEquivalencyComp :Attribute => Condition => Boolean =
    att =>(c=>c match{
      case AttributeComparison(a2,EqualTo(_)) if a2 == att=> true
      case _ => false
    })

  // put targetConds in a seq so we can give an error specific to the condition that failed.
  //list of (ConditionToVerify, ErrorIfConditionIsFalse)
  val targetConds : Seq[(ObjectsMatchingConditions => Boolean, String)] = Seq(
    (_.conditions.count(attributeEquivalencyComp(Attack)) == 1,"Attack must be specified exactly once"),
    (_.conditions.count(attributeEquivalencyComp(Speed)) == 1, "Speed must be specified exactly once"),
    (_.conditions.count(attributeEquivalencyComp(Health)) == 1, "Health must be specified exactly once"),
    (_.objectType != Event,"Cannot transform into an event"),
    ((_.conditions.length <= 3), "Too many conditions for inner card creation.")
  )

  override def validate (node: AstNode) : Try[Unit] =
    node match {
      case GenerateC(obj) => satisfiesAllTargetConditions(obj, targetConds) match {
        case Success(_) => validateChildren(this, node)
        case f => f
      }
      case _ => validateChildren(this, node)
    }

  val getIfFail : (((Boolean, String)) => Option[String]) =
    {case(x,y) =>if(x){None}else{Some(y)}}

  val satisfiesAllTargetConditions:(ObjectsMatchingConditions, Seq[(ObjectsMatchingConditions => Boolean, String)]) => Try[Unit] = (conds,targs)=>
    targs.map(x=>(x._1(conds),x._2)).flatMap(getIfFail(_)) match{
      case Seq() => Success()
      case s => Failure(ValidationError(s.mkString(", ")))
    }
}