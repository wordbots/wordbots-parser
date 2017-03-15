package wordbots

object CodeGenerator {
  def generateJS(node: AstNode): String = g(node)

  // scalastyle:off method.length
  // scalastyle:off cyclomatic.complexity
  private def g(node: AstNode): String = {
    node match {
      case At(trigger, action) => s"(function () { setTrigger(${g(trigger)}, ${g(action)}); })"
      case If(condition, action) => s"(function () { if (${g(condition)}) { (${g(action)})(); } })"

      // Actions
      case And(action1, action2) => s"(function () { ${g(action1)}(); ${g(action2)}(); })"
      case CanMoveAgain(target) => s"(function () { actions['canMoveAgain'](${g(target)}); })"
      case DealDamage(target, num) => s"(function () { actions['dealDamage'](${g(target)}, ${g(num)}); })"
      case Destroy(target) => s"(function () { actions['destroy'](${g(target)}); })"
      case Discard(target) => s"(function () { actions['discard'](${g(target)}); })"
      case Draw(target, num) => s"(function () { actions['draw'](${g(target)}, ${g(num)}); })"
      case ModifyAttribute(target, attr, op) => s"(function () { actions['modifyAttribute'](${g(target)}, ${g(attr)}, ${g(op)}); })"
      case ModifyEnergy(target, op) => s"(function () { actions['modifyEnergy'](${g(target)}, ${g(op)}); })"
      case SetAttribute(target, attr, num) => s"(function () { actions['setAttribute'](${g(target)}, ${g(attr)}, ${g(num)}); })"
      case TakeControl(player, target) => s"(function () { actions['takeControl'](${g(player)}, ${g(target)}); })"

      // Passive abilities
      case ApplyEffect(target, effect) =>
        s"(function () { setAbility(abilities['applyEffect'](function () { return ${g(target)}; }, ${g(effect)})); })"
      case AttributeAdjustment(target, attr, op) =>
        s"(function () { setAbility(abilities['attributeAdjustment'](function () { return ${g(target)}; }, ${g(attr)}, ${g(op)})); })"
      case FreezeAttribute(target, attr) =>
        s"(function () { setAbility(abilities['freezeAttribute'](function () { return ${g(target)}; }, ${g(attr)})); })"

      // Triggers
      case AfterAttack(targetObj) => s"triggers['afterAttack'](function () { return ${g(targetObj)}; })"
      case AfterDamageReceived(targetObj) => s"triggers['afterDamageReceived'](function () { return ${g(targetObj)}; })"
      case AfterDestroyed(targetObj, cause) => s"triggers['afterDestroyed'](function () { return ${g(targetObj)}; }, ${g(cause)})"
      case AfterPlayed(targetObj) => s"triggers['afterPlayed'](function () { return ${g(targetObj)}; })"
      case BeginningOfTurn(targetPlayer) => s"triggers['beginningOfTurn'](function () { return ${g(targetPlayer)}; })"
      case EndOfTurn(targetPlayer) => s"triggers['endOfTurn'](function () { return ${g(targetPlayer)}; })"

      // Target objects
      case Choose(collection) => s"targets['choose'](${g(collection)})"
      case All(collection) => s"targets['all'](${g(collection)})"
      case ThisRobot => "targets['thisRobot']()"
      case ItO => "targets['it']()"
      case ItP => "targets['it']()"

      // Target players
      case Self => "targets['self']()"
      case Opponent => "targets['opponent']()"
      case AllPlayers => "targets['allPlayers']()"
      case ControllerOf(targetObject) => s"targets['controllerOf'](${g(targetObject)})"

      // Conditions
      case AdjacentTo(obj) => s"conditions['adjacentTo'](${g(obj)})"
      case AttributeComparison(attr, comp) => s"conditions['attributeComparison'](${g(attr)}, ${g(comp)})"
      case ControlledBy(player) => s"conditions['controlledBy'](${g(player)})"

      // Global conditions
      case CollectionExists(coll) => s"(${g(coll)}.length > 0)"

      // Arithmetic operations
      case Plus(num) => s"function (x) { return x + ${g(num)}; }"
      case Minus(num) => s"function (x) { return x - ${g(num)}; }"
      case Multiply(num) => s"function (x) { return x * ${g(num)}; }"
      case Divide(num, RoundedDown) => s"function (x) { return Math.floor(x / ${g(num)}); }"
      case Divide(num, RoundedUp) => s"function (x) { return Math.ceil(x / ${g(num)}); }"

      // Comparisons
      case GreaterThan(num) => s"(function (x) { return x > ${g(num)}; })"
      case GreaterThanOrEqualTo(num) => s"(function (x) { return x >= ${g(num)}; })"
      case LessThan(num) => s"(function (x) { return x < ${g(num)}; })"
      case LessThanOrEqualTo(num) => s"(function (x) { return x <= ${g(num)}; })"

      // Numbers
      case Scalar(int) => s"$int"
      case Count(collection) => s"count(${g(collection)})"
      case AttributeSum(collection, attr) => s"attributeSum(${g(collection)}, ${g(attr)})"
      case AttributeValue(obj, attr) => s"attributeValue(${g(obj)}, ${g(attr)})"

      // Collections
      case AllTiles => s"allTiles()"
      case CardsInHand(player, cardType) => s"cardsInHand(${g(player)}, ${g(cardType)})"
      case ObjectsInPlay(objType) => s"objectsInPlay(${g(objType)})"
      case ObjectsMatchingConditions(objType, conditions) => s"objectsMatchingConditions(${g(objType)}, ${conditions.map(g).mkString("[", ", ", "]")})"

      // Labels
      case m: MultiLabel => m.labels.map(g).mkString("[", ", ", "]")
      case l: Label => s"'${getLabelName(l)}'"
    }
  }
  // scalastyle:on method.length
  // scalastyle:on cyclomatic.complexity

  private def getLabelName(label: Label): String = {
    label.getClass.getSimpleName.toLowerCase.split('$')(0)
  }
}
