package edu.rit.csh.linter.parser


import edu.rit.csh.linter.language.Types._
import fastparse.all._
import fastparse.core.Parser

object Types {
  import Literals._

  val typ: Parser[Typ] = P("").!.map { TypeDesignator }



  val types = P(typ ~ ("," ~ typ).rep).map {
    case (ty, tys) => tys :+ ty
  }

  val typeArgs: Parser[Seq[Typ]] = P("[" ~ types ~ "]")

  val classQualifier = P("[" ~ Literals.id ~ "]")

  val stableId: Parser[String] =
    (
      ((id ~ ".").? ~ ("this" ~ "." ~ id | "super" ~ classQualifier.? ~ "." ~ id) | id)
      ~ ("." ~ id).rep
    ).!

  val path: Parser[String] = (stableId | (id ~ ".").? ~ "this").!

  val simpleType: Parser[SimpleType] = {
    val left = stableId.map { TypeDesignator } | (path ~ "." ~ typ).map { SingletonType.tupled }
    (left ~ typeArgs.?).map {
      case (st, Some(args)) => ParameterizedType(st, args)
      case (st, None) => st
    } | (left ~ id.?).map {
        case (st, Some(idStr)) => TypeProjection(st, idStr)
        case (st, None) => st
      }
  }

  val annotation: Parser[Annotation] = ("@" ~ simpleType ~ Expressions.argumentExprs.rep)
    .map { case (st, exprs) => Annotation(st, exprs) }

  val annotType: Parser[AnnotatedType] = (simpleType ~ annotation.rep)
    .map { case (st, annots) => AnnotatedType(st, annots) }

  // 3.2.7 Compound Types

  // TODO
  val compoundType: Parser[CompoundType] =
    (annotType ~ ("with" ~ annotType).rep).map { case (at, ats) => CompoundType(ats :+ at) }

  // InfixType ::= CompoundType {id [nl] CompoundType}
  val infixType = (compoundType ~ (id ~ nl.? ~ compoundType).rep).map {
    case (ct, cts) => cts.foldLeft(InfixType(ct, None)) {
      case (left, (op, right)) => InfixType(left, Some(op, right))
    }
  }
}
