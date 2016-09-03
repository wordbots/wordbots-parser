package wordbots

import com.workday.montague.ccg.TerminalCat

case object Adj extends TerminalCat { val category = "Adj" }
case object N extends TerminalCat { val category = "N*" }  // montague defines "N" == "NP", but that's not what we want
case object Num extends TerminalCat { val category = "#" }
case object Rel extends TerminalCat { val category = "Rel" }