package wordbots

import wordbots.Semantics._

import scala.util.{Failure, Success, Try}

case class ValidationError(message: String) extends Exception(message)

sealed trait ValidationMode
case object ValidateObject extends ValidationMode
case object ValidateEvent extends ValidationMode
case object ValidateRandomlyGeneratedAction extends ValidationMode
case object ValidateUnknownCard extends ValidationMode

case class AstValidator(mode: ValidationMode = ValidateUnknownCard) {
  val baseRules: Seq[AstRule] = Seq(
    NoUnimplementedRules,
    NoChooseOrRewriteInTriggeredAction,
    NoModifyingCostOfObjects,
    OnlyRestoreHealth,
    OnlyThisObjectPlayed,
    ValidGeneratedCard,
    NoChooseAfterRandom
  )

  val rules: Seq[AstRule] = mode match {
    case ValidateObject => baseRules :+ MustBeAbility
    case ValidateEvent => baseRules ++ Seq(MustBeAction, NoThis)
    case ValidateRandomlyGeneratedAction => baseRules ++ Seq(MustBeAction, NoInstead)
    case ValidateUnknownCard => baseRules
  }

  def validate(ast: AstNode): Try[Unit] = {
    Try(rules.foreach(_.validate(ast).get))
  }

  def isValid(ast: AstNode): Boolean = validate(ast).isSuccess
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

  protected def validateChildren(rule: AstRule, parentNode: AstNode): Try[Unit] = {
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

/**
 * Disallow the following behaviors within a triggered ability (excepted AfterPlayed, which can be treated more like an action):
 *    * choosing targets (because there's no UI support for having a player choose targets during event execution)
 *    * rewriting card text (because calling the parser is expensive and should only happen from direct player interaction)
 */
object NoChooseOrRewriteInTriggeredAction extends AstRule {
  object NoChooseTargetOrRewrite extends AstRule {
    override def validate(node: AstNode): Try[Unit] = {
      node match {
        case ChooseC(_) => Failure(ValidationError("Choosing targets not allowed for triggered actions."))
        case ChooseO(_) => Failure(ValidationError("Choosing targets not allowed for triggered actions."))
        case RewriteText(_, _) => Failure(ValidationError("Rewriting text not allowed for triggered actions."))
        case n: AstNode => validateChildren(this, n)
      }
    }
  }

  override def validate(node: AstNode): Try[Unit] = {
    node match {
      case TriggeredAbility(AfterPlayed(_), _) => Success()  // Choosing targets and rewriting text *is* allowed for AfterPlayed triggers.
      case TriggeredAbility(_, _) => validateChildren(NoChooseTargetOrRewrite, node)  // (but not for any other trigger).
      case n: AstNode => validateChildren(this, n)
    }
  }
}

object NoModifyingCostOfObjects extends AstRule {
  override def validate(node: AstNode): Try[Unit] = {
    node match {
      case AttributeAdjustment(target, Cost, _) =>
        target match {
          case _: TargetObject => Failure(ValidationError("Can't modify the cost of objects on the board (try \"robot cards\" instead of \"robots\", etc)."))
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
      case GiveAbility(_, _) => Success()  // Don't validate this inside GiveAbility clauses to allow, e.g., 'Give all robots "Activate: Destroy this robot"'
      case n: AstNode => validateChildren(this, n)
    }
  }
}

object NoInstead extends AstRule {
  override def validate(node: AstNode): Try[Unit] = {
    node match {
      case Instead(_) => Failure(ValidationError(s"Can't use Instead in this context"))
      case n: AstNode => validateChildren(this, n)
    }
  }
}

/** Validates that generated cards have exactly one of each desired attribute. */
object ValidGeneratedCard extends AstRule {
  override def validate(node: AstNode): Try[Unit] = {
    node match {
      case c@GeneratedCard(cardType, _, _) => Try {
        val attributes = (c.getAttributeAmount(Attack).size, c.getAttributeAmount(Health).size, c.getAttributeAmount(Speed).size)
        val expectedAttrs = cardType match {
          case Robot => (1, 1, 1)
          case Structure => (0, 1, 0)
          case _ => throw ValidationError(s"Invalid generated card type: $cardType")
        }

        if (attributes == expectedAttrs) {
          validateChildren(this, c)
        } else {
          throw ValidationError(s"Wrong # of (Attack, Health, Speed) attributes for a generated $cardType card (expected $expectedAttrs, got $attributes)")
        }
      }
      case n: AstNode => validateChildren(this, n)
    }
  }
}

/** Validates that targets can't be chosen after (assuming DFS evaluation order) a random selection is made,
  * to prevent "gaming" the system by canceling target selection when dissatisfied with the random choice. */
object NoChooseAfterRandom extends AstRule {
  override def validate(node: AstNode): Try[Unit] = Try {
    var randomOperation: Option[AstNode] = None
    for { n <- node.depthFirstTraverse } {
      n match {
        case _: RandomC | _: RandomO | _: RandomT =>
          randomOperation = Some(n)
        case _: ChooseC | _: ChooseO | _: ChooseT if randomOperation.isDefined =>
          throw ValidationError(s"Can't ask player to select a target ($n) after a random operation (${randomOperation.get}) to prevent 're-rolling'")
        case _ =>
      }
    }
  }
}
