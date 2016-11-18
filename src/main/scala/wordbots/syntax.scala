package wordbots

import com.workday.montague.ccg.TerminalCat

case object Adj extends TerminalCat { val category = "Adj" }
case object Adv extends TerminalCat { val category = "Adv" }
case object N extends TerminalCat { val category = "Noun" }  // montague treats "N" and "NP" as synonyms, but we don't want this by default.
case object Num extends TerminalCat { val category = "#" }
case object Rel extends TerminalCat { val category = "Rel" }
case object V extends TerminalCat { val category = "V" }
