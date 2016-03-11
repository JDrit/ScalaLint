package edu.rit.csh.linter.parser

import edu.rit.csh.linter.language.Patterns._
import fastparse.WhitespaceApi
import fastparse.all._
import fastparse.core.Parser

object Patterns {
  import Types._
  import Literals._

  val whitespace = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }

  import whitespace._

  val wildCard = P("_").map { _ => '_ }

  // 8.2 Type Patterns
  val typPat = typ

  val patterns: Parser[Seq[Pattern]] = P((pattern ~ ("," ~ pattern).rep).map { case (pt, pts) => pts.+:(pt) })

  val literalPattern = P(literal.map { case lit => LiteralPattern(lit) })
  val variablePattern = P(varId | wildCard).map { VariablePattern }
  val stableIdPattern = P(stableId.map { case sid =>
    val first = sid.toString.charAt(1)
    if (first >= 'a' && first <= 'z') {
      if (sid.toString.contains("."))
        StableIdPattern(sid)
      else
        VariablePattern(sid)
    } else {
      StableIdPattern(sid)
    }
  })
  val constructorPattern = P((stableId ~ "(" ~ patterns.? ~ ")")
    .map { case (id, patts) => ConstructorPattern(id, patts.getOrElse(Seq.empty):_*) })
  val tuplePattern: Parser[TuplePattern] = P("(" ~ patterns ~ ")").map { case pts => TuplePattern(pts:_*) }
  val sequencePattern = P(stableId ~ "(" ~ (variablePattern ~ ",").rep  ~ (varId ~ "@").? ~ "_" ~ "*" ~ ")")
    .map { case (sid, pts, varid) => SequencePattern(sid, pts, varid) }

  val simplePattern: Parser[Pattern] =
      P( tuplePattern
       | literalPattern
       | sequencePattern
       | constructorPattern
       | stableIdPattern
       | variablePattern
       )

  val pattern3: Parser[Pattern] = P(simplePattern ~ (id ~ nl.? ~ simplePattern).rep).map {
    case (sp, Nil) => sp
    case (sp, lst) => toConstructor(sp, lst.toList)
  }

  private def toConstructor(start: Pattern, input: List[(Symbol, Pattern)]): Pattern = input match {
    case Nil => throw new RuntimeException("entered empty list")
    case (op, sp) :: Nil => ConstructorPattern(op, start, sp)
    case (op, sp) :: pts => ConstructorPattern(op, start, toConstructor(sp, pts))
  }

  val test: List[Int] = 1 :: (2 :: List())

  // 8.1.3 Pattern Binders
  val pattern2 = P((varId ~ "@" ~ pattern3).map { BindingPattern.tupled } | pattern3)

  // 8.1.2 Typed Patterns
  val pattern1 = P(pattern2 | ((varId | wildCard) ~ ":" ~ typPat).map { TypedPattern.tupled })

  val pattern: Parser[Pattern] = P(pattern1 ~ ("|" ~ pattern1).rep).map {
    case (pt, Nil) => pt
    case (p1, pts) => AlternativePattern(pts.+:(p1):_*)
  }
}
