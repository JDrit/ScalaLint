package edu.rit.csh.linter.parser

import edu.rit.csh.linter.language.Patterns.{PatternBinder, TypedPattern}
import fastparse.all._
import fastparse.core.Parser

object Patterns {
  import Types._
  import Literals._

  // 8.1.1 Variable Patterns
  val simplePattern = "_" | varId

  // 8.1.2 Typed Patterns
  val pattern1 = ((varId | "_").! ~ ":" ~ typPat).map { TypedPattern.tupled }

  // 8.1.3 Pattern Binders
  val pattern2 = (varId ~ "@" ~ pattern3).map { PatternBinder.tupled }

  // 8.1.4 - 8.1.9

  val simplePattern =
    ( literal.map
    |
    )

  // 8.2 Type Patterns
  def typPat = typ
}
