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
    OnlyRestoreHealth
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
        case Choose(_) => Failure(ValidationError("Choosing targets not allowed for triggered actions."))
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
          case _: ObjectCollection => Failure(ValidationError("Can't modify the cost of objects on the board."))
          case All(_: ObjectCollection) => Failure(ValidationError("Can't modify the cost of objects on the board."))
          case Choose(_: ObjectCollection) => Failure(ValidationError("Can't modify the cost of objects on the board."))
          case Random(_, _: ObjectCollection) => Failure(ValidationError("Can't modify the cost of objects on the board."))
          case ThisObject => Failure(ValidationError("Can't modify the cost of objects on the board."))
          case ItO => Failure(ValidationError("Can't modify the cost of objects on the board."))
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
