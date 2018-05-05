package wordbots

import com.workday.montague.ccg.{CcgCat, TerminalCat}

case object Adj extends TerminalCat { val category = "Adj" }
case object Adv extends TerminalCat { val category = "Adv" }
case object N extends TerminalCat { val category = "Noun" }  // montague treats "N" and "NP" as synonyms, but we don't want this by default.
case object Num extends TerminalCat { val category = "#" }
case object Quoted extends TerminalCat { val category = "\"" }
case object V extends TerminalCat { val category = "V" }

// Reverse conjunction: (X/X)\X for any category X (as opposed to regular conjunction, which is (X\X)/X ).
// This can be useful when you want the arguments to a conjunction to be evaluated from left-to-right.
// NOTE: Strictly speaking, this is not a terminal category, but wordbots.montague.ccg.CcgCat is a sealed trait.
// TODO: Either unseal CcgCat or add ReverseConj to montague.
case object ReverseConj extends TerminalCat {
  override def getForwardApplication(right: CcgCat): Option[CcgCat] = None
  override def getBackwardApplication(left: CcgCat): Option[CcgCat] = Some(left/left)
  val category = "((X/X)\\X)"
}