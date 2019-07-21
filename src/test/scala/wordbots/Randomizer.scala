package wordbots

import com.danielasfregola.randomdatagenerator.RandomDataGenerator
import io.circe.Json
import io.circe.syntax._
import org.scalacheck.Gen.oneOf
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen}

import scala.reflect.runtime.universe._
import scala.util.Try

/**
  * Tools for generating random valid Wordbots ASTs.
  *
  * To run:
  *   (optionally) export RANDOM_DATA_GENERATOR_SEED=<seed>
  *   sbt "test:run-main wordbots.Randomizer"
  *
  * NOTE: This takes a while to compile because of shapeless magic.
  */
object Randomizer extends RandomDataGenerator {
  import Semantics._

  implicit lazy val arbInt: Arbitrary[Int] = Arbitrary(oneOf(0, 1, 2, 3))

  var currentSeed: Seed = seed

  def randomAction: Option[Action] = safeRandom[Action]
  def randomAbility: Option[Ability] = safeRandom[Ability]

  /**
    * Generate a random list of JS actions (for testing).
    */
  def main(args: Array[String] = Array()): Unit = {
    val NUM_ACTIONS = 100
    val MAX_ACTION_SIZE = 20

    print(s"Generating $NUM_ACTIONS random actions of size < $MAX_ACTION_SIZE ")

    val actions: Set[Action] =
      Stream.continually { randomAction }
        .flatten
        .filter(_.size < MAX_ACTION_SIZE)
        .filter(AstValidator(ValidateRandomlyGeneratedAction).isValid)
        .distinct
        .take(NUM_ACTIONS)
        .toSet

    // This intentionally throws an exception if CodeGenerator returns None for any action.
    val json: Json = actions.map(CodeGenerator.generateJS(_).get).asJson
    println(s"\n$json")
  }

  private def safeRandom[T: WeakTypeTag: Arbitrary]: Option[T] = {
    print(".")
    currentSeed = currentSeed.next

    Try {
      implicitly[Arbitrary[T]].arbitrary
        .apply(Gen.Parameters.default, currentSeed)
    }.toOption.flatten
  }
}
