package wordbots

import com.workday.montague.ccg.CcgCat

// Representation of an edit that can be made to an invalid sentence to make it valid.
sealed trait Edit {
  def apply(words: Seq[String]): Seq[String]
  def description(words: Seq[String]): String

  protected def describeCat(cat: CcgCat): String = Map(
    "Noun" -> "a noun", "NP" -> "a noun phrase", "Num" -> "a number",
    "Adj" -> "an adjective", "Adv" -> "an adverb", "Rel" -> "a relative clause", "S" -> "a sentence"
  ).getOrElse(cat.category, s"a ${cat.category}")
}

case class Delete(idx: Int) extends Edit {
  def apply(words: Seq[String]): Seq[String] = Seq(words.patch(idx, Nil, 1).mkString(" "))

  def description(words: Seq[String]): String = s"syntax error - unexpected word '${words(idx)}'"
}

case class DeleteChunk(fromIdx: Int, toIdx: Int) extends Edit {
  def apply(words: Seq[String]): Seq[String] = Seq(words.patch(fromIdx, Nil, toIdx - fromIdx + 1).mkString(" "))

  def description(words: Seq[String]): String = s"syntax error - unexpected phrase '${words.slice(fromIdx, toIdx).mkString(" ")}'"
}

case class Replace(idx: Int, pos: CcgCat) extends Edit {
  def apply(words: Seq[String]): Seq[String] = Lexicon.termsInCategory(pos).map(term => words.patch(idx, Seq(term), 1).mkString(" ").capitalize)

  def description(words: Seq[String]): String = {
    val context = if (idx > 0) s"after '${words(idx - 1)}'" else s"before '${words(idx + 1)}'"
    s"syntax error - expected ${describeCat(pos)} $context but got '${words(idx)}' instead"
  }
}

case class ExactReplace(idx: Int, term: String) extends Edit {
  def apply(words: Seq[String]): Seq[String] = Seq(words.patch(idx, Seq(term), 1).mkString(" ").capitalize)

  def description(words: Seq[String]): String = {
    val context = if (idx > 0) s"after '${words(idx - 1)}'" else s"before '${words(idx + 1)}'"
    s"syntax error - expected '$term' $context but got '${words(idx)}' instead"
  }
}

case class Insert(idx: Int, pos: CcgCat) extends Edit {
  def apply(words: Seq[String]): Seq[String] = Lexicon.termsInCategory(pos).map(term => words.patch(idx, Seq(term), 0).mkString(" ").capitalize)

  def description(words: Seq[String]): String = {
    if (idx > 0) {
      s"syntax error - '${words(idx - 1)}' should be followed by ${describeCat(pos)}"
    } else {
      s"syntax error - should start with ${describeCat(pos)}"
    }
  }
}
