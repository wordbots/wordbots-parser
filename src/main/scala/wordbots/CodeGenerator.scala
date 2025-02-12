package wordbots


import scala.collection.mutable
import scala.util.Try

object JavaScriptValidator {
  import org.mozilla.javascript._
  import org.mozilla.javascript.ast._

  val compilerEnv = new CompilerEnvirons

  /**
    * Throw if jsString is invalid JavaScript.
    */
  def assertIsValidJS(jsString: String): Unit = {
    val parser = new Parser(compilerEnv)

    // Parse the given JS string ...
    val ast: AstRoot = parser.parse(jsString, "", 1)
    // ... and also parse any stringified function embedded within
    StringCollector.stringifiedFunctionsInAst(ast).foreach(assertIsValidJS)
  }

  /**
    * A [[NodeVisitor]] that keeps track of all [[StringLiteral]]s found within an [[AstNode]].
    */
  private class StringCollector extends NodeVisitor {
    val stringLiteralsFound: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()

    def visit(node: AstNode): Boolean = {
      node match {
        case str: StringLiteral => stringLiteralsFound += str.getValue()
        case _ =>
      }
      true
    }
  }

  object StringCollector {
    def stringifiedFunctionsInAst(ast: AstRoot): Seq[String] = {
      val stringCollector = new StringCollector()
      ast.visitAll(stringCollector)
      stringCollector.stringLiteralsFound.filter(_.contains("function"))
    }
  }
}

object CodeGenerator {
  import Semantics._

  private var escapeLevel = 0

  def generateJS(node: AstNode): Try[String] = Try {
    escapeLevel = 0
    val jsString = g(node)
    JavaScriptValidator.assertIsValidJS(jsString)
    jsString
  }

  private def escape(str: => String) = {
    escapeLevel += 1
    val unescapedStr = str
    val escapedStr = unescapedStr.replaceAllLiterally("\"", s"""${"\\" * escapeLevel}"""")
    escapeLevel -= 1
    escapedStr
  }

  // Defer execution of a JS function to as late as possible, e.g. so that it works correctly with 'they' when iterating over a collection
  private def deferred(str: String) = {
    s""""(function () { return ${escape(str)}; })""""
  }

  // scalastyle:off method.length
  // scalastyle:off cyclomatic.complexity
  private def g(node: AstNode): String = {
    node match {
      // Meta
      case ForEach(collection, action) => s"(function () { actions['forEach'](${g(collection)}, ${g(action)}); })"
      case If(condition, action) => s"(function () { if (${g(condition)}) { (${g(action)})(); } })"
      case MultipleActions(actions) => s"(function () { ${actions.map(action => s"${g(action)}();").mkString(" ")} })"
      case MultipleAbilities(abilities) => s"(function () { ${abilities.map(ability => s"${g(ability)}();").mkString(" ")} })"
      case Until(TurnsPassed(num), action) => s"(function () { save('duration', $num); ${g(action)}(); save('duration', null); })"

      // Actions: Normal
      case Become(source, target) => s"(function () { actions['become'](${g(source)}, ${g(target)}); })"
      case CanActivateAgain(target) => s"(function () { actions['canActivateAgain'](${g(target)}); })"
      case CanAttackAgain(target) => s"(function () { actions['canAttackAgain'](${g(target)}); })"
      case CanMoveAgain(target) => s"(function () { actions['canMoveAgain'](${g(target)}); })"
      case CanMoveAndAttackAgain(target) => s"(function () { actions['canMoveAndAttackAgain'](${g(target)}); })"
      case DealDamage(target, num) => s"(function () { actions['dealDamage'](${g(target)}, ${g(num)}); })"
      case Destroy(target) => s"(function () { actions['destroy'](${g(target)}); })"
      case Discard(target) => s"(function () { actions['discard'](${g(target)}); })"
      case Draw(target, num) => s"(function () { actions['draw'](${g(target)}, ${g(num)}); })"
      case EndTurn => "(function () { actions['endTurn'](); })"
      case GiveAbility(target, ability) => s"""(function () { actions['giveAbility'](${g(target)}, "${escape(g(ability))}"); })"""
      case ModifyAttribute(target, attr, op) => s"(function () { actions['modifyAttribute'](${g(target)}, ${g(attr)}, ${g(op)}); })"
      case ModifyEnergy(target, op) => s"(function () { actions['modifyEnergy'](${g(target)}, ${g(op)}); })"
      case MoveCardsToHand(target, player) => s"(function () { actions['moveCardsToHand'](${g(target)}, ${g(player)}); })"
      case MoveObject(target, dest) => s"(function () { actions['moveObject'](${g(target)}, function () { return ${g(dest)}; }); })"
      case PayEnergy(target, amount) => s"(function () { actions['payEnergy'](${g(target)}, ${g(amount)}); })"
      case RemoveAllAbilities(target) => s"(function () { actions['removeAllAbilities'](${g(target)}); })"
      case RestoreAttribute(target, Health, Some(num)) => s"(function () { actions['restoreHealth'](${g(target)}, ${g(num)}); })"
      case RestoreAttribute(target, Health, None) => s"(function () { actions['restoreHealth'](${g(target)}); })"
      case ReturnToHand(target, player) => s"(function () { actions['returnToHand'](${g(target)}, ${g(player)}); })"
      case RewriteText(target, textReplacements) => s"(function () { actions['rewriteText'](${g(target)}, ${textReplacements.map(t => s"'${t._1.replaceAllLiterally("'", "\\'")}': '${t._2.replaceAllLiterally("'", "\\'")}'").mkString("{ ", ", ", " }")}); })"
      case SetAttribute(target, attr, num) => s"(function () { actions['setAttribute'](${g(target)}, ${g(attr)}, ${deferred(g(num))}); })"
      case ShuffleCardsIntoDeck(target, player) => s"(function () { actions['shuffleCardsIntoDeck'](${g(target)}, ${g(player)}); })"
      case SpawnObject(card, dest, owner) => s"(function () { actions['spawnObject'](${g(card)}, ${g(dest)}, ${g(owner)}); })"
      case SwapAttributes(target, attr1, attr2) => s"(function () { actions['swapAttributes'](${g(target)}, ${g(attr1)}, ${g(attr2)}); })"
      case SwapPositions(target1, target2) => s"(function () { actions['swapPositions'](${g(target1)}, ${g(target2)}); })"
      case TakeControl(player, target) => s"(function () { actions['takeControl'](${g(player)}, ${g(target)}); })"
      case WinGame(player) => s"(function () { actions['winGame'](${g(player)}); })"

      // Actions: Utility
      case SaveTarget(target) => s"(function () { save('target', ${g(target)}); })"

      // Activated and triggered abilities
      case ActivatedAbility(action) =>
        s"""(function () { setAbility(abilities['activated'](function () { return ${g(ThisObject)}; }, "${escape(g(action))}")); })"""
      case TriggeredAbility(trigger, Instead(action)) => s"(function () { setTrigger(${g(trigger)}, ${g(action)}, {override: true}); })"
      case TriggeredAbility(trigger, action) => s"(function () { setTrigger(${g(trigger)}, ${g(action)}); })"

      // Passive abilities
      case ApplyEffect(target, effect) =>
        s"(function () { setAbility(abilities['applyEffect'](function () { return ${g(target)}; }, ${g(effect)})); })"
      case AttributeAdjustment(target, attr, op) =>
        s"(function () { setAbility(abilities['attributeAdjustment'](function () { return ${g(target)}; }, ${g(attr)}, ${g(op)})); })"
      case ConditionalAction(condition, action) =>
        s"""(function () { setAbility(abilities['conditionalAction'](function () { return ${g(condition)}; }, "${escape(g(action))}")); })"""
      case FreezeAttribute(target, attr) =>
        s"(function () { setAbility(abilities['freezeAttribute'](function () { return ${g(target)}; }, ${g(attr)})); })"
      case HasAbility(target, ability) =>
        s"""(function () { setAbility(abilities['giveAbility'](function () { return ${g(target)}; }, "${escape(g(ability))}")); })"""

      // Effects
      case CanOnlyAttack(target) => s"'canonlyattack', {target: ${g(target)}}"
      case CannotMoveTo(tiles) => s"'cannotmoveto', {tiles: ${deferred(g(tiles))}}"

      // Triggers
      case AfterAttack(targetObj, objectType) => s"triggers['afterAttack'](function () { return ${g(targetObj)}; }, ${g(objectType)})"
      case AfterAttackedBy(targetObj, objectType) => s"triggers['afterAttackedBy'](function () { return ${g(targetObj)}; }, ${g(objectType)})"
      case AfterCardDraw(targetPlayer, cardType) => s"triggers['afterCardDraw'](function () { return ${g(targetPlayer)}; }, ${g(cardType)})"
      case AfterCardEntersDiscardPile(targetPlayer, cardType) => s"triggers['afterCardEntersDiscardPile'](function () { return ${g(targetPlayer)}; }, ${g(cardType)})"
      case AfterCardPlay(targetPlayer, cardType) => s"triggers['afterCardPlay'](function () { return ${g(targetPlayer)}; }, ${g(cardType)})"
      case AfterDamageReceived(targetObj, cardType) => s"triggers['afterDamageReceived'](function () { return ${g(targetObj)}; }, ${g(cardType)})"
      case AfterDealsDamage(targetObj, objectType) => s"triggers['afterDealsDamage'](function () { return ${g(targetObj)}; }, ${g(objectType)})"
      case AfterDestroysOtherObject(targetObj, objectType) => s"triggers['afterDestroysOtherObject'](function () { return ${g(targetObj)}; }, ${g(objectType)})"
      case AfterDestroyed(targetObj, cause) => s"triggers['afterDestroyed'](function () { return ${g(targetObj)}; }, ${g(cause)})"
      case AfterMove(targetObj) => s"triggers['afterMove'](function () { return ${g(targetObj)}; })"
      case AfterPlayed(targetObj) => s"triggers['afterPlayed'](function () { return ${g(targetObj)}; })"
      case BeginningOfTurn(targetPlayer) => s"triggers['beginningOfTurn'](function () { return ${g(targetPlayer)}; })"
      case EndOfTurn(targetPlayer) => s"triggers['endOfTurn'](function () { return ${g(targetPlayer)}; })"

      // Targets
      case ConditionTargetOn(target, condition) => s"targets['conditionOn'](${g(target)}, function () { return ${g(condition)}; })"

      // Target objects
      case ChooseO(collection, num) => s"targets['choose'](${g(collection)}, ${g(num)})"
      case AllO(collection) => s"targets['all'](${g(collection)})"
      case RandomO(num, collection) => s"targets['random'](${g(num)}, ${g(collection)})"
      case UnionO(targets) => s"targets['union']([ ${targets.map(g).mkString(", ")} ])"
      case ThisObject => "targets['thisRobot']()"
      case ItO => "targets['it']()"
      case ItP => "targets['itP']()"
      case That => "targets['that']()"
      case They => "targets['they']()"
      case TheyP => "targets['theyP']()"
      case SavedTargetObject => "load('target')"

      // Target tiles
      case ChooseT(collection, num) => s"targets['choose'](${g(collection)}, ${g(num)})"
      case AllT(collection) => s"targets['all'](${g(collection)})"
      case RandomT(num, collection) => s"targets['random'](${g(num)}, ${g(collection)})"
      case SavedTargetTile => "load('target')"

      // Target cards
      case ChooseC(collection, num) => s"targets['choose'](${g(collection)}, ${g(num)})"
      case AllC(collection) => s"targets['all'](${g(collection)})"
      case RandomC(num, collection) => s"targets['random'](${g(num)}, ${g(collection)})"
      case CopyOfC(objToCopy) => s"targets['copyOf'](${g(objToCopy)})"
      case card@GeneratedCard(cardType, _, name) => {
        val attributesObjStr = Seq(Attack, Health, Speed).map { attr =>
          s"'${attr.name}': ${card.getAttributeAmount(attr).headOption.map(g).getOrElse("null")}"
        }.mkString("{", ", ", "}")
        s"targets['generateCard'](${g(cardType)}, $attributesObjStr, ${name.map(n => s"'${n.replaceAllLiterally("'", "\'")}'").getOrElse("null")})"
      }

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
      case NotC(cond) => s"conditions['not'](${g(cond)})"
      case Unoccupied => s"conditions['unoccupied']()"
      case WithinDistanceOf(distance, obj) => s"conditions['withinDistanceOf'](${g(distance)}, ${g(obj)})"

      // Global conditions
      case CollectionCountComparison(coll, comp) => s"globalConditions['collectionCountComparison'](${g(coll)}, ${g(comp)})"
      case CollectionExists(coll) => s"globalConditions['collectionExists'](${g(coll)})"
      case NotGC(cond) => s"!(${g(cond)})"
      case TargetHasProperty(target, property) => s"globalConditions['targetHasProperty'](${g(target)}, ${g(property)})"
      case TargetMeetsCondition(target, condition) => s"globalConditions['targetMeetsCondition'](${g(target)}, ${g(condition)})"

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
      case IsEven => "(function (x) { return x % 2 == 0; })"
      case IsOdd => "(function (x) { return x % 2 == 1; })"
      case LessThan(num) => s"(function (x) { return x < ${g(num)}; })"
      case LessThanOrEqualTo(num) => s"(function (x) { return x <= ${g(num)}; })"

      // Numbers
      case Scalar(int) => s"$int"
      case AttributeSum(collection, attr) => s"attributeSum(${g(collection)}, ${g(attr)})"
      case AttributeValue(obj, attr) => s"attributeValue(${g(obj)}, ${g(attr)})"
      case Count(collection) => s"count(${g(collection)})"
      case EnergyAmount(player) => s"energyAmount(${g(player)})"
      case Half(num, RoundedDown) => s"(Math.floor(${g(num)} / 2))"
      case Half(num, RoundedUp) => s"(Math.ceil(${g(num)} / 2))"
      case MaximumEnergyAmount(player) => s"maximumEnergyAmount(${g(player)})"
      case NumberOfObjectsDestroyedThisTurn => s"numberOfObjectsDestroyedThisTurn()"
      case ThatMuch => "thatMuch()"
      case Times(n1, n2) => s"((${g(n1)}) * (${g(n2)}))"

      // Collections
      case AllTiles => s"allTiles()"
      case CardsInDiscardPile(player, cardType, conditions) => s"cardsInDiscardPile(${g(player)}, ${g(cardType)}, ${conditions.map(g).mkString("[", ", ", "]")})"
      case CardsInHand(player, cardType, conditions) => s"cardsInHand(${g(player)}, ${g(cardType)}, ${conditions.map(g).mkString("[", ", ", "]")})"
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

  private def g(nodeOpt: Option[AstNode]): String = nodeOpt.map(g).getOrElse("null")
}
