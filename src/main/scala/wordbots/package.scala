/**
  * Created by alex on 10/1/17.
  */
package object wordbots {
  // For ease of testing in console:
  // wordbots.parse("...") = Wordbots.Parser.main(Array("..."))
  def parse(cmd: String): Unit = {
    Parser.main(Array(cmd))
  }

  /** A simple tokenizer that normalizes sentneces and treats punctuation appropriately. Used by [[Parser.tokenizer]] and elsewhere. */
  val simpleTokenizer: String => IndexedSeq[String] = { str: String =>
    str
      .trim
      .toLowerCase
      .replaceAll("[\u202F\u00A0]", " ")  // treat special space characters as spaces
      .replaceAll("[\u0091\u0092\u2018\u2019]", "'")  // replace "smart quotes" with regular quotes (see http://www.java2s.com/ref/java/java-string-replace-microsoft-smart-quotes-curly-and.html )
      .replaceAll("[\\u0093\\u0094\\u201c\\u201d]", "\"")  // ibid
      .replaceAllLiterally("\' ", " \' ")  // add spaces before and after <'> to make it a separate token
      .replaceAllLiterally("\'s", " \'s ")  // add spaces before and after <'s> to make it a separate token
      .replaceAll("""["<>]""", " $0 ")  // add spaces before and after { " < > }, to make them separate tokens
      .split("""\s+|[.?!,()]""")  // tokenize by splitting on spaces and punctuation
      .filter("" !=)  // ignore empty tokens
  }
}
