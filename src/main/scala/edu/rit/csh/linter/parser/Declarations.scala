package edu.rit.csh.linter.parser

import edu.rit.csh.linter.language.Declarations._
import edu.rit.csh.linter.language.Expressions.DefaultExpression
import edu.rit.csh.linter.language.Patterns.VariablePattern
import fastparse.WhitespaceApi
import fastparse.all._
import fastparse.core.Parser

object Declarations {
  import Types._
  import Literals._
  import Annotations._
  import Dependency._
  import Expressions._
  import Patterns._
  import Classes._

  val whitespace = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import whitespace._


  // 4.1 Value Declarations and Definitions

  val ids = P(id.rep(min = 1, sep = ","))

  // ValDcl ::= ids ‘:’ Type
  val valDcl = P(ids ~ ":" ~ typ).map { ValDcl.tupled }
  val varDcl = P(ids ~ ":" ~ typ).map { VarDcl.tupled }
  val typeDcl = P(id ~ (typeParamClause).? ~ (">:" ~ typ).? ~ ("<:" ~ typ).?).map {
    case (name, Some(typParams), lower, upper) => TypeDcl(name, typParams, lower, upper)
    case (name, None, lower, upper) => TypeDcl(name, Seq.empty, lower, upper)
  }
  val funDcl = P(funSig ~ ":" ~ typ).map {
    case (id, typeClause, pc, result) => FunDcl(id, typeClause.getOrElse(Seq.empty), pc, result)
  }


  val dcl = P("val" ~/ valDcl | "var" ~/ varDcl | "def" ~/ funDcl | "type" ~/ typeDcl)

  val patDef = P(pattern2.rep(min = 1, sep = ",") ~ (":" ~ typ).? ~ "=" ~ expr).map(PatternDef.tupled)

  val varDef: Parser[PatternDef] = P(
    patDef | (ids ~ ":" ~ typ ~ "=" ~ "_").map { case(ids, typ) => PatternDef(ids.map(VariablePattern), Some(typ), DefaultExpression(typ)) })

  val patVarDef = P( ("val" ~/ patDef).map(ValDef) | ("var" ~/ varDef).map(VarDef))

  // 4.3 Type Declarations and Type Aliases


  val typeDef = P(id ~ typeParamClause.? ~ "=" ~ typ).map { case (id, typeParams, typ) =>
    TypeDef(id, typeParams.getOrElse(Seq.empty), typ )
  }

  // 4.6 Function Declarations and Definitions
  val funDef =
    P( (funSig ~ (":" ~ typ).? ~ "=" ~/ expr)
     | (funSig ~ nl.? ~ "{" ~/ block ~ "}")
     | ("this" ~ paramClause ~ paramClauses)
     | ("=" ~ constrExpr ~ nl.? ~ constrBlock) )

  val definition = P(patVarDef | "def" ~/ funDef | "type" ~/ nl.rep ~ typeDef | tmplDef)

  // 4.4 Type Parameters

  // VariantTypeParam ::= {Annotation} [‘+’ | ‘-’] TypeParam
  val variantTypeParam: Parser[VariantTypeParam] = P(annotation.rep ~ ("+" | "-").!.? ~ typeParam).map {
    case (annotations, None, param) => VariantTypeParam(param, annotations)
    case (annotations, Some("+"), param) => VariantTypeParam(param, annotations, Some(Covariant))
    case (annotations, Some("-"), param) => VariantTypeParam(param, annotations, Some(Contravariant))
  }

  // TypeParamClause ::= ‘[’ VariantTypeParam {‘,’ VariantTypeParam} ‘]’
  val typeParamClause: Parser[Seq[VariantTypeParam]] =
    P("[" ~ variantTypeParam ~ ("," ~ variantTypeParam).rep ~ "]").map { case (ty, tys) => tys :+ ty }

  val typeParam: Parser[TypeParam] = P((id | wildCard) ~ typeParamClause.? ~ (">:" ~ typ).? ~ ("<:" ~ typ).? ~ (":" ~ typ).?)
    .map { case (id, tpc, lower, upper, context) => TypeParam(id, tpc, lower, upper, context) }

  // 4.6 Function Declarations and Definitions
  val funTypeParamClause = P("[" ~ typeParam.rep(min = 1, sep = ",") ~ "]")

  val paramType: Parser[ParamType] = P( (typ ~ "*".!.?).map {
      case (typ, Some(_)) => RepeatedParamType(typ)
      case (typ, None) => RegularParamType(typ)
    } | ("=>" ~/ typ).map { ByNameParamType })

  val param: Parser[Parameter] = P(annotation.rep ~ id ~ (":" ~ paramType).? ~ ("=" ~ expr).?)
    .map { case (annots, name, pts, exprD) => RegularParameter(name, annots, pts, exprD) }

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
