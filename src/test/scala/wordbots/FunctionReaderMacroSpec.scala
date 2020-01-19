package wordbots

import com.workday.montague.semantics._
import com.workday.montague.semantics.FunctionReaderMacro.λ
import org.scalatest.{FlatSpec, Matchers}

/**
 * Tests human-readable representations of various Wordbots lexicon items.
 * Note: Technically this is a test of montague functionality (see [[FunctionReaderMacro]]), not of Wordbots itself.
 */
class FunctionReaderMacroSpec extends FlatSpec with Matchers {
  import Semantics._

  it should "pretty-print 'cards'" in {
    λ {hand: Hand => CardsInHand(hand.player)}.toString shouldEqual
      "Lambda(hand: Hand => CardsInHand(hand.player))"
  }

  it should "pretty-print 'give'" in {
    λ {t: TargetObject => λ {i: Scalar => λ {a: Attribute => ModifyAttribute(t, a, Plus(i))}}}.toString shouldEqual
      "Lambda(t: TargetObject => Lambda(i: Scalar => Lambda(a: Attribute => ModifyAttribute(t, a, Plus(i)))))"
  }

  it should "pretty-print 'robot'" in {
    λ {d: DiscardPile => CardsInDiscardPile(d.player, Robot)}.toString shouldEqual
      "Lambda(d: DiscardPile => CardsInDiscardPile(d.player, Robot))"
  }
}
