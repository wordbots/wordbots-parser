package wordbots

import com.workday.montague.parser.TokenMatcher

/**
  * Created by alex on 3/22/17.
  */
object NumberWordMatcher extends TokenMatcher[Int] {
  val numberWords = Seq("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")

  def apply(str: String): Seq[Int] = {
    numberWords.indexOf(str) match {
      case idx if idx > -1 => Seq(idx)
      case _               => Nil
    }
  }
}
