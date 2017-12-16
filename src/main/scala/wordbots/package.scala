/**
  * Created by alex on 10/1/17.
  */
package object wordbots {
  // For ease of testing in console:
  // wordbots.parse("...") = Wordbots.Parser.main(Array("..."))
  def parse(cmd: String): Unit = {
    Parser.main(Array(cmd))
  }
}
