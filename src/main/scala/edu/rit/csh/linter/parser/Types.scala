package edu.rit.csh.linter.parser


import edu.rit.csh.linter.language.Types._
import fastparse.all._
import fastparse.core.Parser

object Types {
  import Literals._
  import Declarations._
  import Annotations._

  val types = P(typ ~ ("," ~ typ).rep).map {
    case (ty, tys) => tys :+ ty
  }

  val typeArgs: Parser[Seq[Typ]] = P("[" ~ types ~ "]")

  val classQualifier = P("[" ~ id ~ "]")

  val stableId: Parser[Symbol] = P(((id ~ ".").? ~ ("this" ~ "." ~ id | "super" ~ classQualifier.? ~ "." ~ id) | id) ~ ("." ~ id).rep).!.map { case str => Symbol(str) }

  val path: Parser[Symbol] = P(stableId | (id ~ ".").? ~ "this").!.map { case str => Symbol(str) }

  val simpleType: Parser[SimpleType] = {
    val left = P(
        path.!.filter(_.endsWith(".type")).map { case str => SingletonType(Symbol(str.substring(0, str.length - 5))) }
      | stableId.map { TypeDesignator }
      | "(" ~ types.map { case typs => TupleType(typs: _*) } ~ ")")

    P(left ~ (typeArgs | "#" ~ id).?).map {
      case (st, None) => st
      case (st, Some(id: Symbol)) => TypeProjection(st, id)
      case (st, Some(args: Seq[Typ])) => ParameterizedType(st, args: _*)
    }
  }


  // 3.2.6 Annotated Types

  // AnnotType  ::=  SimpleType {Annotation}
  val annotType: Parser[AnnotatedType] = (simpleType ~ annotation.rep)
    .map { case (tp, attos) => AnnotatedType(tp, attos:_*) }

  // 3.2.7 Compound Types

  // TODO
  val compoundType: Parser[CompoundType] =
    (annotType ~ ("with" ~ annotType).rep).map { case (at, ats) => CompoundType(ats :+ at :_*) }

  // 3.2.8

  // InfixType ::= CompoundType {id [nl] CompoundType}
  val infixType = (compoundType ~ (id ~ nl.? ~ compoundType).rep).map {
    case (ct, cts) => cts.foldLeft(InfixType(ct, None)) { case (left, (op, right)) =>
      InfixType(left, Some(op, right))
    }
  }

  // 3.2.9 Function Types

  val functionArgs: Parser[Seq[Typ]] =
    ( infixType.map { ty => Seq(ty) }
    | ("(" ~ (paramType ~ ("," ~ paramType).rep).? ~ ")").map {
        case Some((pt, pts)) => pts :+ pt
        case None => Seq.empty
      }
    )

  // 3.2.10 Existential Types

  val existentialDcl = ("type" ~ typeDcl) | ("val" ~ valDcl)

  val typ = stableId.map { TypeDesignator }

  //val typ: Parser[FunctionType] = (functionArgs ~ "=>" ~ typ).map { FunctionType.tupled }

}
