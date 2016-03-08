package edu.rit.csh.linter.parser

import edu.rit.csh.linter.language.Patterns._
import fastparse.all._
import fastparse.core.Parser

object Patterns {
  import Types._
  import Literals._

  val wildCard = P("_").map { _ => '_ }

  // 8.2 Type Patterns
  val typPat = typ

  // 8.1.2 Typed Patterns
  val pattern1 = ((varId | wildCard) ~ ":" ~ typPat).map { TypedPattern.tupled } | pattern2

  val pattern3 = (simplePattern ~ (id ~ nl.? ~ simplePattern).rep).map { case (pt, pts) => pts :+ pt }

  // 8.1.3 Pattern Binders
  val pattern2 = (varId ~ "@" ~ pattern3).map { PatternBinder.tupled }


  val pattern = (pattern1 ~ ("|" ~ pattern1).rep).map { case (pt, pts) => pts :+ pt }

  val patterns: Parser[Seq[Pattern]] = (pattern ~ ("," ~ pattern).rep).map { case (pt, pts) => pt ++ pts.flatten }


  val literalPattern = literal.map { case lit => LiteralPattern(lit) }
  val variablePattern = (varId | wildCard).map { VariablePattern }
  val stableIdPattern = stableId.map { case sid =>
    val first = sid.toString.charAt(1)
    if (first >= 'a' && first <= 'z') {
      if (sid.toString.contains("."))
        StableIdPattern(sid)
      else
        VariablePattern(sid)
    } else {
      StableIdPattern(sid)
    }
  }
  val constructorPattern = (stableId ~ "(" ~ patterns.? ~ ")")
    .map { case (id, patts) => ConstructorPattern(id, patts.getOrElse(Seq.empty)) }
  val tuplePattern = "(" ~ patterns.map { TuplePattern } ~ ")"
  val sequencePattern = (stableId ~ "(" ~ (patterns ~ ",").? ~ (varId ~ "@").? ~ "_" ~ "@" ~ ")")
    .map { case (sid, pts, vid) => SequencePattern(sid, pts.getOrElse(Seq.empty), vid) }

  val simplePattern: Parser[Pattern] =
      ( literalPattern
      | sequencePattern
      | tuplePattern
      | constructorPattern
      | stableIdPattern
      | variablePattern )


}
