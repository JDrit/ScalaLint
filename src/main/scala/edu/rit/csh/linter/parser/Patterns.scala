package edu.rit.csh.linter.parser

import edu.rit.csh.linter.language.Literals.SymbolLiteral
import edu.rit.csh.linter.language.Patterns._
import fastparse.all._
import fastparse.core.Parser

object Patterns {
  import Types._
  import Literals._

  val wildCard = P("_").map { _ => '_ }
  val at = P("@").map { _ => '@ }

  // 8.1.1 Variable Patterns
  val simplePattern: Parser[Pattern] =
      ( (varId | wildCard).map { VariablePattern }
      | literal.map { case lit => LiteralPattern(lit) }
      | (stableId ~ "(" ~ (patterns ~ ",").? ~ (varId ~ "@").? ~ "_" ~ "@" ~ ")").map {
          case (sid, patterns, vid) => SequencePattern(sid, patterns.getOrElse(Seq.empty), vid)
        }
      | (stableId ~ "(" ~ patterns.? ~ ")").map { case (id, patts) => ConstructorPattern(id, patts.getOrElse(Seq.empty)) }
      | stableId.map { StableIdPattern }
      )


  // 8.1.2 Typed Patterns
  val pattern1 = ((varId | wildCard) ~ ":" ~ typPat).map { TypedPattern.tupled }

  // 8.1.3 Pattern Binders
  //val pattern2 = (varId ~ "@" ~ pattern3).map { PatternBinder.tupled }

  // 8.1.4 - 8.1.9


  // 8.2 Type Patterns
  def typPat = typ

  def pattern = (pattern1 ~ ("|" ~ pattern1).rep).map { case (pt, pts) => pts :+ pt }

  def patterns: Parser[Seq[Pattern]] = (pattern ~ ("," ~ pattern).rep).map { case (pt, pts) => pt ++ pts.flatten }

}
