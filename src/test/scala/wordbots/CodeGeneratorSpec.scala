package wordbots

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

class CodeGeneratorSpec extends FlatSpec with Matchers {
  import Semantics._

  it should "be able to generate valid JavaScript for abilities within abilities" in {
    CodeGenerator.generateJS(ActivatedAbility(GiveAbility(ItO, ApplyEffect(ItO, CannotMove)))) shouldEqual
      Success("(function () { setAbility(abilities['activated'](function () { return targets['thisRobot'](); }, \"(function () { actions['giveAbility'](targets['it'](), \\\"(function () { setAbility(abilities['applyEffect'](function () { return targets['it'](); }, 'cannotmove')); })\\\"); })\")); })")
  }

  it should "be able to generate valid JavaScript for abilities within abilities within abilities" in {
    CodeGenerator.generateJS(ActivatedAbility(GiveAbility(ItO, ActivatedAbility(EndTurn)))) shouldEqual
      Success("(function () { setAbility(abilities['activated'](function () { return targets['thisRobot'](); }, \"(function () { actions['giveAbility'](targets['it'](), \\\"(function () { setAbility(abilities['activated'](function () { return targets['thisRobot'](); }, \\\\\\\"(function () { actions['endTurn'](); })\\\\\\\")); })\\\"); })\")); })")
  }
}
