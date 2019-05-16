package wordbots

import org.mozilla.javascript._

import scala.util.Try

object CodeGenerator {
  val compilerEnv = new CompilerEnvirons

  def generateJS(node: AstNode): Try[String] = {
    for {
      js <- Try { g(node) }
      validatedJs <- validateJS(js)
    } yield validatedJs
  }

  def escape(str: String): String = str.replaceAllLiterally("\\\"", "\\\\\\\"")  // For those following along at home, it's \" -> \\\

  def validateJS(jsString: String): Try[String] = {
    Try {
      val parser = new Parser(compilerEnv)
      parser.parse(jsString, "", 1)
      jsString
    }
  }

  // scalastyle:off method.length
  // scalastyle:off cyclomatic.complexity
  private def g(node: AstNode): String = {
    node match {
      // Meta
      case If(condition, action) => s"(function () { if (${g(condition)}) { (${g(action)})(); } })"
      case MultipleActions(actions) => s"(function () { ${actions.map(action => s"${g(action)}();").mkString(" ")} })"
      case MultipleAbilities(abilities) => s"(function () { ${abilities.map(ability => s"${g(ability)}();").mkString(" ")} })"
      case Until(TurnsPassed(num), action) => s"(function () { save('duration', $num); ${g(action)}(); save('duration', null); })"

      // Actions: Normal
      case Become(source, target) => s"(function () {actions['become'](${g(source)},${g(target)});})"
      case CanAttackAgain(target) => s"(function () { actions['canAttackAgain'](${g(target)}); })"
      case CanMoveAgain(target) => s"(function () { actions['canMoveAgain'](${g(target)}); })"
      case CanMoveAndAttackAgain(target) => s"(function () { actions['canMoveAndAttackAgain'](${g(target)}); })"
      case DealDamage(target, num) => s"(function () { actions['dealDamage'](${g(target)}, ${g(num)}); })"
      case Destroy(target) => s"(function () { actions['destroy'](${g(target)}); })"
      case Discard(target) => s"(function () { actions['discard'](${g(target)}); })"
      case Draw(target, num) => s"(function () { actions['draw'](${g(target)}, ${g(num)}); })"
      case EndTurn => "(function () { actions['endTurn'](); })"
      case GiveAbility(target, ability) => s"""(function () { actions['giveAbility'](${g(target)}, \\"${escape(g(ability))}\\"); })"""
      case ModifyAttribute(target, attr, op) => s"(function () { actions['modifyAttribute'](${g(target)}, ${g(attr)}, ${g(op)}); })"
      case ModifyEnergy(target, op) => s"(function () { actions['modifyEnergy'](${g(target)}, ${g(op)}); })"
      case MoveObject(target, dest) => s"(function () { actions['moveObject'](${g(target)}, ${g(dest)}); })"
      case PayEnergy(target, amount) => s"(function () { actions['payEnergy'](${g(target)}, ${g(amount)}); })"
      case RemoveAllAbilities(target) => s"(function () { actions['removeAllAbilities'](${g(target)}); })"
      case RestoreAttribute(target, Health, Some(num)) => s"(function () { actions['restoreHealth'](${g(target)}, ${g(num)}); })"
      case RestoreAttribute(target, Health, None) => s"(function () { actions['restoreHealth'](${g(target)}); })"
      case ReturnToHand(target) => s"(function () { actions['returnToHand'](${g(target)}); })"
      case SetAttribute(target, attr, num) => s"(function () { actions['setAttribute'](${g(target)}, ${g(attr)}, ${g(num)}); })"
      case SpawnObject(card, dest) => s"(function () { actions['spawnObject'](${g(card)}, ${g(dest)}); })"
      case SwapAttributes(target, attr1, attr2) => s"(function () { actions['swapAttributes'](${g(target)}, ${g(attr1)}, ${g(attr2)}); })"
      case TakeControl(player, target) => s"(function () { actions['takeControl'](${g(player)}, ${g(target)}); })"

      // Actions: Utility
      case SaveTarget(target) => s"(function () { save('target', ${g(target)}); })"

      // Activated and triggered abilities
      case ActivatedAbility(action) =>
        s"""(function () { setAbility(abilities['activated'](function () { return ${g(ThisObject)}; }, \\"${escape(g(action))}\\")); })"""
      case TriggeredAbility(trigger, Instead(action)) => s"(function () { setTrigger(${g(trigger)}, ${g(action)}, {override: true}); })"
      case TriggeredAbility(trigger, action) => s"(function () { setTrigger(${g(trigger)}, ${g(action)}); })"

      // Passive abilities
      case ApplyEffect(target, effect) =>
        s"(function () { setAbility(abilities['applyEffect'](function () { return ${g(target)}; }, ${g(effect)})); })"
      case AttributeAdjustment(target, attr, op) =>
        s"(function () { setAbility(abilities['attributeAdjustment'](function () { return ${g(target)}; }, ${g(attr)}, ${g(op)})); })"
      case FreezeAttribute(target, attr) =>
        s"(function () { setAbility(abilities['freezeAttribute'](function () { return ${g(target)}; }, ${g(attr)})); })"
      case HasAbility(target, ability) =>
        s"""(function () { setAbility(abilities['giveAbility'](function () { return ${g(target)}; }, \\"${escape(g(ability))}\\")); })"""

      // Effects
      case CanOnlyAttack(target) => s"'canonlyattack', {target: ${g(target)}}"

      // Triggers
      case AfterAttack(targetObj, objectType) => s"triggers['afterAttack'](function () { return ${g(targetObj)}; }, ${g(objectType)})"
      case AfterCardPlay(targetPlayer, cardType) => s"triggers['afterCardPlay'](function () { return ${g(targetPlayer)}; }, ${g(cardType)})"
      case AfterDamageReceived(targetObj) => s"triggers['afterDamageReceived'](function () { return ${g(targetObj)}; })"
      case AfterDestroyed(targetObj, cause) => s"triggers['afterDestroyed'](function () { return ${g(targetObj)}; }, ${g(cause)})"
      case AfterMove(targetObj) => s"triggers['afterMove'](function () { return ${g(targetObj)}; })"
      case AfterPlayed(targetObj) => s"triggers['afterPlayed'](function () { return ${g(targetObj)}; })"
      case BeginningOfTurn(targetPlayer) => s"triggers['beginningOfTurn'](function () { return ${g(targetPlayer)}; })"
      case EndOfTurn(targetPlayer) => s"triggers['endOfTurn'](function () { return ${g(targetPlayer)}; })"

      // Target objects
      case ChooseO(collection) => s"targets['choose'](${g(collection)})"
      case AllO(collection) => s"targets['all'](${g(collection)})"
      case RandomO(num, collection) => s"targets['random'](${g(num)}, ${g(collection)})"
      case ThisObject => "targets['thisRobot']()"
      case ItO => "targets['it']()"
      case ItP => "targets['itP']()"
      case That => "targets['that']()"
      case They => "targets['they']()"
      case SavedTargetObject => "load('target')"

      // Target cards
      case ChooseC(collection) => s"targets['choose'](${g(collection)})"
      case AllC(collection) => s"targets['all'](${g(collection)})"
      case RandomC(num, collection) => s"targets['random'](${g(num)}, ${g(collection)})"
      case CopyOfC(objToCopy) => s"targets['copyOf'](${g(objToCopy)})"
      case card@GeneratedCard(cardType, _, name) =>
        val attributesObjStr = Seq(Attack, Health, Speed).map { attr =>
          s"'${attr.name}': ${card.getAttributeAmount(attr).headOption.map(g).getOrElse("null")}"
        }.mkString("{", ", ", "}")
        s"targets['generateCard'](${g(cardType)}, $attributesObjStr, ${name.map(n => s"'${n}'")getOrElse("null")})"

      // Target players
      case Self => "targets['self']()"
      case Opponent => "targets['opponent']()"
      case AllPlayers => "targets['allPlayers']()"
      case ControllerOf(targetObject) => s"targets['controllerOf'](${g(targetObject)})"

      // Conditions
      case AdjacentTo(obj) => s"conditions['adjacentTo'](${g(obj)})"
      case AttributeComparison(attr, comp) => s"conditions['attributeComparison'](${g(attr)}, ${g(comp)})"
      case ControlledBy(player) => s"conditions['controlledBy'](${g(player)})"
      case ExactDistanceFrom(distance, obj) => s"conditions['exactDistanceFrom'](${g(distance)}, ${g(obj)})"
      case HasProperty(property) => s"conditions['hasProperty'](${g(property)})"
      case Unoccupied => s"conditions['unoccupied']()"
      case WithinDistanceOf(distance, obj) => s"conditions['withinDistanceOf'](${g(distance)}, ${g(obj)})"

      // Global conditions
      case CollectionExists(coll) => s"globalConditions['collectionExists'](${g(coll)})"
      case TargetHasProperty(target, property) => s"globalConditions['targetHasProperty'](${g(target)}, ${g(property)})"

      // Arithmetic operations
      case Constant(num) => s"function (x) { return ${g(num)}; }"
      case Plus(num) => s"function (x) { return x + ${g(num)}; }"
      case Minus(num) => s"function (x) { return x - ${g(num)}; }"
      case Multiply(num) => s"function (x) { return x * ${g(num)}; }"
      case Divide(num, RoundedDown) => s"function (x) { return Math.floor(x / ${g(num)}); }"
      case Divide(num, RoundedUp) => s"function (x) { return Math.ceil(x / ${g(num)}); }"

      // Comparisons
      case EqualTo(num) => s"(function (x) { return x === ${g(num)}; })"
      case GreaterThan(num) => s"(function (x) { return x > ${g(num)}; })"
      case GreaterThanOrEqualTo(num) => s"(function (x) { return x >= ${g(num)}; })"
      case LessThan(num) => s"(function (x) { return x < ${g(num)}; })"
      case LessThanOrEqualTo(num) => s"(function (x) { return x <= ${g(num)}; })"

      // Numbers
      case Scalar(int) => s"$int"
      case AttributeSum(collection, attr) => s"attributeSum(${g(collection)}, ${g(attr)})"
      case AttributeValue(obj, attr) => s"attributeValue(${g(obj)}, ${g(attr)})"
      case Count(collection) => s"count(${g(collection)})"
      case EnergyAmount(player) => s"energyAmount(${g(player)})"

      // Collections
      case AllTiles => s"allTiles()"
      case CardsInHand(player, cardType) => s"cardsInHand(${g(player)}, ${g(cardType)})"
      case ObjectsMatchingConditions(objType, conditions) => s"objectsMatchingConditions(${g(objType)}, ${conditions.map(g).mkString("[", ", ", "]")})"
      case Other(collection) => s"other(${g(collection)})"
      case TilesMatchingConditions(conditions) => s"tilesMatchingConditions(${conditions.map(g).mkString("[", ", ", "]")})"

      // Labels
      case m: MultiLabel => m.labels.map(g).mkString("[", ", ", "]")
      case l: Label => s"'${l.name}'"
    }
  }
  // scalastyle:on method.length
  // scalastyle:on cyclomatic.complexity
}
