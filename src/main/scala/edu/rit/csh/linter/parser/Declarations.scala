package edu.rit.csh.linter.parser

import edu.rit.csh.linter.language.Declarations._
import edu.rit.csh.linter.language.Types.Typ
import fastparse.WhitespaceApi
import fastparse.all._
import fastparse.core.Parser

object Declarations {
  import Types._
  import Literals._
  import Annotations._
  import Dependency._
  import Expressions._

  val whitespace = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import whitespace._


  // 4.1 Value Declarations and Definitions

  val ids = P(id ~ ("," ~ id).rep).map { case (id, ids) => ids :+ id }

  // ValDcl ::= ids ‘:’ Type
  val valDcl = P(ids ~ ":" ~ typ).map { ValDcl.tupled }
  val varDcl = P(ids ~ ":" ~ typ).map { VarDcl.tupled }
  val typeDcl = P(id ~ (typeParamClause).? ~ (">:" ~ typ).? ~ ("<:" ~ typ).?).map {
    case (name, Some(typParams), lower, upper) => TypeDcl(name, typParams, lower, upper)
    case (name, None, lower, upper) => TypeDcl(name, Seq.empty, lower, upper)
  }
  val funDcl = P(funSig ~ ":" ~ typ).map {
    case (id, typeClause, paramClause, result) => FunDcl(id, typeClause.getOrElse(Seq.empty), paramClause, result)
  }


  val dcl = P("val" ~/ valDcl | "var" ~/ varDcl | "def" ~/ funDcl)

  // 4.3 Type Declarations and Type Aliases


  val typeDef = P(id ~ typeParamClause.? ~ "=" ~ typ).map { case (id, typeParams, typ) =>
    TypeDef(id, typeParams.getOrElse(Seq.empty), typ )
  }

  val definition = P("type" ~/ nl.rep ~ typeDef)

  // 4.4 Type Parameters

  // VariantTypeParam ::= {Annotation} [‘+’ | ‘-’] TypeParam
  val variantTypeParam: Parser[VariantTypeParam] = P(annotation.rep ~ ("+" | "-").!.? ~ typeParam).map {
    case (annotations, None, param) => VariantTypeParam(annotations, None, param)
    case (annotations, Some("+"), param) => VariantTypeParam(annotations, Some(Covariant), param)
    case (annotations, Some("-"), param) => VariantTypeParam(annotations, Some(Contravariant), param)
  }

  // TypeParamClause ::= ‘[’ VariantTypeParam {‘,’ VariantTypeParam} ‘]’
  val typeParamClause: Parser[Seq[VariantTypeParam]] =
    P("[" ~ variantTypeParam ~ ("," ~ variantTypeParam).rep ~ "]").map { case (ty, tys) => tys :+ ty }

  val typeParam: Parser[TypeParam] = P((id | "_").! ~ typeParamClause.? ~ (">:" ~ typ).? ~ ("<:" ~ typ).? ~ (":" ~ typ).?).map { TypeParam.tupled }

  // 4.6 Function Declarations and Definitions
  val funTypeParamClause = P("{" ~ typeParam.rep(min = 1, sep = ",") ~ "}")

  val paramType: Parser[ParamType] =
    ( typ.map { RegularParamType }
    | ("=>" ~/ typ).map { ByNameParamType }  // By-Name Parameter
    | (typ ~ "*").map { RepeatedParamType }) // Repeated Parameters

  val param: Parser[Parameter] = P(annotation.rep ~ id ~ (":" ~ paramType).? ~ ("=" ~ expr).?).map { RegularParameter.tupled }

  val params: Parser[Seq[Parameter]] = P(param.rep(min = 1, sep = ","))

  val paramClause = P(nl.? ~ "(" ~ params.? ~ ")").map { case (pt) => pt.getOrElse(Seq.empty) }

  val paramClauses: Parser[Seq[Seq[Parameter]]] = P(paramClause.rep ~ (nl.? ~ "(" ~ "implicit" ~ params ~ ")").?).map {
    case (pts, None) => pts
    case (pts, Some(implicits)) => pts ++ Seq(implicits.map {
      case RegularParameter(annots, name, typ, expr) => ImplicitParameter(annots, name, typ, expr)
    })
  }

  val funSig = P(id ~ funTypeParamClause.? ~ paramClauses)

}
