package wordbots

import com.workday.montague.parser.TokenMatcher

/**
  * Created by alex on 4/11/17.
  */
object StrictIntegerMatcher extends TokenMatcher[Int] {
  def apply(str: String): Seq[Int] = {
    if (str forall Character.isDigit) {
      Seq(Integer.parseInt(str))
    } else {
      Nil
    }
  }
}
