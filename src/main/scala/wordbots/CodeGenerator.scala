package wordbots

object CodeGenerator {
  def generateJS(node: AstNode): String = g(node)

  private def g(node: AstNode): String = {
    node match {
       // Actions
      case AttributeDelta(target, attr, delta) => s"(function () { actions['attributeDelta'](${g(target)}, ${g(attr)}, ${g(delta)}); })"
      case DealDamage(target, num) => s"(function () { actions['dealDamage'](${g(target)}, ${g(num)}); })"
      case Destroy(target) => s"(function () { actions['destroy'](${g(target)}); })"
      case Discard(target, num) => s"(function () { actions['discard'](${g(target)}, ${g(num)}); })"
      case Draw(target, num) => s"(function () { actions['draw'](${g(target)}, ${g(num)}); })"
      case EnergyDelta(target, delta) => s"(function () { actions['energyDelta'](${g(target)}, ${g(delta)}); })"
      case SetAttribute(target, attr, num) => s"(function () { actions['setAttribute'](${g(target)}, ${g(attr)}, ${g(num)}); })"

      // Targets
      case Choose(collection) => s"targets['choose'](${g(collection)})"
      case All(collection) => s"targets['all'](${g(collection)})"
      case Self => "targets['self']()"
      case Opponent => "targets['opponent']()"

      // Conditions
      case NoCondition => "null"
      case AttributeComparison(attr, comp) => s"conditions['attributeComparison'](${g(attr)}, ${g(comp)})"
      case ControlledBy(player) => s"conditions['controlledBy'](${g(player)})"

      // Deltas
      case Plus(num) => s"${g(num)}"
      case Minus(num) => s"-${g(num)}"

      // Comparisons
      case GreaterThanOrEqualTo(num) => s"(function (x) { return x >= ${g(num)}; })"
      case LessThanOrEqualTo(num) => s"(function (x) { return x <= ${g(num)}; })"

      // Numbers
      case Scalar(int) => s"$int"
      case Count(collection) => s"count(${g(collection)}})"
      case AttributeSum(collection, attr) => s"attributeSum(${g(collection)}, ${g(attr)})"

      // Misc
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
