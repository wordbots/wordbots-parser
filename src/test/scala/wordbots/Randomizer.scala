package wordbots

import com.danielasfregola.randomdatagenerator.RandomDataGenerator
import org.scalacheck.Gen.oneOf
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen}

import scala.reflect.runtime.universe._
import scala.util.{Random, Try}

// NOTE: This takes a while to compile because of shapeless magic.
object Randomizer extends RandomDataGenerator {
  implicit lazy val arbInt: Arbitrary[Int] = Arbitrary(oneOf(0, 1, 2, 3))

  import Semantics._

  def action(): Option[Action] = safeRandom[Action]() match {
    case Some(Instead(_)) => None
    case a => a
  }

  def ability(): Option[Ability] = safeRandom[Ability]()

  /**
    * Generate a random list of JS actions (for testing).
    */
  def main(): Unit = {
    val NUM_ACTIONS = 100
    val MAX_ACTION_SIZE = 20

    val actions: Set[Action] =
      Stream.continually { action() }
        .flatten
        .filter(_.size < MAX_ACTION_SIZE)
        .distinct
        .take(NUM_ACTIONS)
        .toSet

    println("[")
    for { a <- actions } {
      println(s"""  "${CodeGenerator.generateJS(a).get}",""")
    }
    println("]")
  }

  private def safeRandom[T: WeakTypeTag: Arbitrary](seed: Seed = Seed(Random.nextLong)): Option[T] = {
    val gen = Gen.infiniteStream(implicitly[Arbitrary[T]].arbitrary)

    Try {
      gen.apply(Gen.Parameters.default, seed)
    }.flatMap { stream => Try {
      stream.get.take(1).head
    }}.toOption
  }
}
