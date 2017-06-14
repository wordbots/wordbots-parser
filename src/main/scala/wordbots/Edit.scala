package wordbots

import com.workday.montague.ccg.CcgCat

// Representation of an edit that can be made to an invalid sentence to make it valid.
sealed trait Edit {
  def apply(words: Seq[String]): Seq[String]
  def description(words: Seq[String]): String

  val categories = Map(
    "Noun" -> "a noun", "NP" -> "a noun phrase", "Num" -> "a number",
    "Adj" -> "an adjective", "Adv" -> "an adverb", "Rel" -> "a relative clause", "S" -> "a sentence"
  )
}

case class Delete(idx: Int) extends Edit {
  def apply(words: Seq[String]): Seq[String] = Seq(words.patch(idx, Nil, 1).mkString(" "))

  def description(words: Seq[String]): String = s"syntax error - unexpected word '${words(idx)}'"
}

case class Replace(idx: Int, pos: CcgCat) extends Edit {
  def apply(words: Seq[String]): Seq[String] = Lexicon.termsInCategory(pos).map(term => words.patch(idx, Seq(term), 1).mkString(" "))

  def description(words: Seq[String]): String = {
    val context = if (idx > 0) s"after '${words(idx - 1)}'" else s"before '${words(idx + 1)}'"
    s"syntax error - expected ${categories(pos.toString)} $context but got '${words(idx)}' instead"
  }
}

case class Insert(idx: Int, pos: CcgCat) extends Edit {
  def apply(words: Seq[String]): Seq[String] = Lexicon.termsInCategory(pos).map(term => words.patch(idx, Seq(term), 0).mkString(" "))

  def description(words: Seq[String]): String = s"syntax error - '${words(idx)}' should be followed by ${categories(pos.toString)}"
}