package edu.rit.csh.linter.parser

import edu.rit.csh.linter.language.Declarations.{ValDcl, Dependency, TypeParam, VariantTypeParam}
import edu.rit.csh.linter.language.Types.Typ
import fastparse.all._
import fastparse.core.Parser

object Declarations {
  import Types._
  import Literals._
  import Annotations._
  import Dependency._

  // 4.1 Value Declarations and Definitions

  val ids = (id ~ ("," ~ id).rep).map { case (id, ids) => ids :+ id }

  // ValDcl ::= ids ‘:’ Type
  val valDcl = (ids ~ ":" ~ typ).map { case (ids, typ) => ValDcl(ids, typ) }

//  val patDef =

  val patVarDef = "val"



  val dcl = "val" ~ valDcl

  // 4.3 Type Declarations and Type Aliases

  val typeDcl = id ~ (typeParamClause).? ~ (">:" ~ typ).? ~ ("<:" ~ typ).?

  // 4.4 Type Parameters

  // TypeParamClause ::= ‘[’ VariantTypeParam {‘,’ VariantTypeParam} ‘]’
  def typeParamClause: Parser[Seq[VariantTypeParam]] =
    ("[" ~ variantTypeParam ~ ("," ~ variantTypeParam).rep ~ "]").map { case (ty, tys) => tys :+ ty }

  // VariantTypeParam ::= {Annotation} [‘+’ | ‘-’] TypeParam
  def variantTypeParam: Parser[VariantTypeParam] = (annotation.rep ~ ("+" | "-").!.? ~ typeParam).map {
    case (annotations, None, param) => VariantTypeParam(annotations, None, param)
    case (annotations, Some("+"), param) => VariantTypeParam(annotations, Some(Covariant), param)
    case (annotations, Some("-"), param) => VariantTypeParam(annotations, Some(Contravariant), param)
  }

  // TypeParam ::= (id | ‘_’) [TypeParamClause] [‘>:’ Type] [‘<:’ Type] [‘:’ Type]
  def typeParam: Parser[TypeParam] = ((id | "_").! ~ typeParamClause.? ~ (">:" ~ typ).? ~ ("<:" ~ typ).? ~ (":" ~ typ).?).map { TypeParam.tupled }




  // 4.6
  val paramType: Parser[Typ] =
    ( typ
    | "=>" ~ typ  // By-Name Parameter
    | typ ~ "*"   // Repeated Parameters
    )




}
