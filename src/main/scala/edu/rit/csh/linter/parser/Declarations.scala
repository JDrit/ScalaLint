package edu.rit.csh.linter.parser

import edu.rit.csh.linter.language.Declarations._
import edu.rit.csh.linter.language.Types.Typ
import fastparse.all._
import fastparse.core.Parser

object Declarations {
  import Types._
  import Literals._
  import Annotations._
  import Dependency._

  // 4.1 Value Declarations and Definitions

  val ids = P(id ~ ("," ~ id).rep).map { case (id, ids) => ids :+ id }



  // ValDcl ::= ids ‘:’ Type
  val valDcl = P(ids ~ ":" ~ typ).map { ValDcl.tupled }
  val varDcl = P(ids ~ ":" ~ typ).map { VarDcl.tupled }
  val funDcl = P(funSig ~ ":" ~ typ)

  val dcl = P("val" ~/ valDcl | "var" ~/ varDcl)

  // 4.3 Type Declarations and Type Aliases

  val typeDcl = P(id ~ (typeParamClause).? ~ (">:" ~ typ).? ~ ("<:" ~ typ).?)

  // 4.4 Type Parameters

  // TypeParamClause ::= ‘[’ VariantTypeParam {‘,’ VariantTypeParam} ‘]’
  val typeParamClause: Parser[Seq[VariantTypeParam]] =
    P("[" ~ variantTypeParam ~ ("," ~ variantTypeParam).rep ~ "]").map { case (ty, tys) => tys :+ ty }

  // VariantTypeParam ::= {Annotation} [‘+’ | ‘-’] TypeParam
  val variantTypeParam: Parser[VariantTypeParam] = P(annotation.rep ~ ("+" | "-").!.? ~ typeParam).map {
    case (annotations, None, param) => VariantTypeParam(annotations, None, param)
    case (annotations, Some("+"), param) => VariantTypeParam(annotations, Some(Covariant), param)
    case (annotations, Some("-"), param) => VariantTypeParam(annotations, Some(Contravariant), param)
  }

  val typeParam: Parser[TypeParam] = P((id | "_").! ~ typeParamClause.? ~ (">:" ~ typ).? ~ ("<:" ~ typ).? ~ (":" ~ typ).?).map { TypeParam.tupled }

  // 4.6 Function Declarations and Definitions
  val funTypeParamClause = P("{" ~ typeParam.rep(min = 1, sep = ",") ~ "}")

  val param: Parser[Parameter] = ???

  val params = P(param.rep(min = 1, sep = ","))

  val paramClause = P(nl.? ~ "(" ~ params.? ~ ")")

  val paramClauses = P(paramClause.rep ~ (nl.? ~ "(" ~ "implicit" ~ params ~ ")").?)

  val funSig = P(id ~ funTypeParamClause.? ~ paramClauses)



  val paramType: Parser[ParamType] =
    ( typ.map { RegularParamType }
    | ("=>" ~/ typ).map { ByNameParamType }  // By-Name Parameter
    | (typ ~ "*").map { RepeatedParamType }   // Repeated Parameters
    )

}
