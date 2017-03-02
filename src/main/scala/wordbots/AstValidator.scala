package wordbots

import scala.util.{Failure, Success, Try}

case class ValidationError(message: String) extends Exception(message)

sealed trait ValidationMode
case object ValidateObject extends ValidationMode
case object ValidateEvent extends ValidationMode
case object ValidateUnknownCard extends ValidationMode

case class AstValidator(mode: ValidationMode = ValidateUnknownCard) {
  val rules: Seq[AstRule] = mode match {
    case ValidateObject => Seq(
      MustBeTriggeredOrPassiveAbility,
      NoChooseInTriggeredAction
    )
    case ValidateEvent => Seq(
      MustBeAction,
      NoChooseInTriggeredAction
    )
    case ValidateUnknownCard => Seq(
      NoChooseInTriggeredAction
    )
  }

  def validate(ast: AstNode): Try[Unit] = {
    Try(rules.foreach(_.validate(ast).get))
  }
}

sealed trait AstRule {
  def validate(node: AstNode): Try[Unit]

  def validateChildren(rule: AstRule, node: AstNode): Try[Unit] = {
    Try {
      for (childNode <- node.productIterator) {
        childNode match {
          case childNode: AstNode =>
            rule.validate(childNode.asInstanceOf[AstNode])
              .flatMap(_ => validateChildren(rule, childNode))
              .get
          case _ => Success()
        }
      }
    }
  }
}

object MustBeTriggeredOrPassiveAbility extends AstRule {
  override def validate(node: AstNode): Try[Unit] = {
    node match {
      case At(_, _) => Success()
      case _: PassiveAbility => Success()
      case _ => Failure(ValidationError("Not a valid passive or triggered ability."))
    }
  }
}

object MustBeAction extends AstRule {
  override def validate(node: AstNode): Try[Unit] = {
    node match {
      case At(_, _) => Failure(ValidationError("Events can't have triggered abilities."))
      case _: PassiveAbility => Failure(ValidationError("Events can't have passive abilities."))
      case _ => Success()
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
      case At(AfterPlayed(_), action) => Success()  // Choosing targets *is* allowed for AfterPlayed triggers.
      case At(_, action) => validateChildren(NoChooseTarget, node)
      case n: AstNode => validateChildren(this, n)
    }
  }
}
