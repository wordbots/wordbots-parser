package wordbots

object CodeGenerator {
  def generateJS(node: AstNode): String = g(node)

  private def g(node: AstNode): String = {
    node match {
      case At(trigger, action) => s"setTrigger(${g(trigger)}, ${g(action)});"

      // Actions
      case And(action1, action2) => s"(function () { ${g(action1)}(); ${g(action2)}(); })"
      case DealDamage(target, num) => s"(function () { actions['dealDamage'](${g(target)}, ${g(num)}); })"
      case Destroy(target) => s"(function () { actions['destroy'](${g(target)}); })"
      case Discard(target, num) => s"(function () { actions['discard'](${g(target)}, ${g(num)}); })"
      case Draw(target, num) => s"(function () { actions['draw'](${g(target)}, ${g(num)}); })"
      case ModifyAttribute(target, attr, op) => s"(function () { actions['modifyAttribute'](${g(target)}, ${g(attr)}, ${g(op)}); })"
      case ModifyEnergy(target, op) => s"(function () { actions['modifyEnergy'](${g(target)}, ${g(op)}); })"
      case SetAttribute(target, attr, num) => s"(function () { actions['setAttribute'](${g(target)}, ${g(attr)}, ${g(num)}); })"

      // Triggers
      case AfterAttack(obj) => s"triggers['afterAttack'](${g(obj)})"
      case BeginningOfTurn(player) => s"triggers['beginningOfTurn'](${g(player)})"
      case EndOfTurn(player) => s"triggers['endOfTurn'](${g(player)})"

      // Target objects
      case Choose(collection) => s"targets['choose'](${g(collection)})"
      case All(collection) => s"targets['all'](${g(collection)})"
      case ThisRobot => "targets['thisRobot']()"

      // Target players
      case Self => "targets['self']()"
      case Opponent => "targets['opponent']()"
      case AllPlayers => "targets['allPlayers']()"

      // Conditions
      case AdjacentTo(obj) => s"conditions['adjacentTo'](${g(obj)})"
      case AttributeComparison(attr, comp) => s"conditions['attributeComparison'](${g(attr)}, ${g(comp)})"
      case ControlledBy(player) => s"conditions['controlledBy'](${g(player)})"

      // Arithmetic operations
      case Plus(num) => s"function (x) { return x + ${g(num)}; }"
      case Minus(num) => s"function (x) { return x - ${g(num)}; }"
      case Multiply(num) => s"function (x) { return x * ${g(num)}; }"
      case Divide(num, RoundedDown) => s"function (x) { return Math.floor(x / ${g(num)}); }"
      case Divide(num, RoundedUp) => s"function (x) { return Math.ceil(x / ${g(num)}); }"

      // Comparisons
      case GreaterThanOrEqualTo(num) => s"(function (x) { return x >= ${g(num)}; })"
      case LessThanOrEqualTo(num) => s"(function (x) { return x <= ${g(num)}; })"

      // Numbers
      case Scalar(int) => s"$int"
      case Count(collection) => s"count(${g(collection)}})"
      case AttributeSum(collection, attr) => s"attributeSum(${g(collection)}, ${g(attr)})"
      case AttributeValue(obj, attr) => s"attributeValue(${g(obj)}, ${g(attr)})"

      // Collections
      case ObjectsInPlay(objType) => s"objectsInPlay(${g(objType)})"
      case ObjectsMatchingCondition(objType, condition) => s"objectsMatchingCondition(${g(objType)}, ${g(condition)})"
        
      // Labels
      case l: Label => s"'${getLabelName(l)}'"
    }
  }

  private def getLabelName(label: Label): String = {
    label.getClass.getSimpleName.toLowerCase.split('$')(0)
  }
}
