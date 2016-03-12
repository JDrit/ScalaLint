package edu.rit.csh.linter.parser


import edu.rit.csh.linter.language.Declarations.Declaration
import edu.rit.csh.linter.language.Types._
import fastparse.WhitespaceApi
import fastparse.all._
import fastparse.core.Parser

object Types {
  import Literals._
  import Declarations._
  import Annotations._

  val whitespace = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import whitespace._

  val types = P(typ ~ ("," ~ typ).rep).map { case (ty, tys) => tys :+ ty }

  val typeArgs: Parser[Seq[Typ]] = P("[" ~/ types ~/ "]")

  val classQualifier = P("[" ~/ id ~/ "]")

  val stableId: Parser[Symbol] = P(((id ~ ".").? ~ ("this" ~ "." ~ id | "super" ~ classQualifier.? ~ "." ~ id) | id) ~ ("." ~ id).rep).!.map { case str => Symbol(str) }

  val path: Parser[Symbol] = P(stableId | (id ~ ".").? ~ "this").!.map { case str => Symbol(str) }

  val simpleType: Parser[SimpleType] = {
    val left = P(
      ("(" ~/ types ~ ")").map { case typs => TupleType(typs: _*) }
      | path.!.filter(_.endsWith(".type")).map { case str => SingletonType(Symbol(str.substring(0, str.length - 5))) }
      | stableId.map { TypeDesignator })

    P(left ~ (typeArgs | "#" ~ id).?).map {
      case (st, Some(id: Symbol)) => TypeProjection(st, id)
      case (st, Some(args: Seq[_])) => ParameterizedType(st, args.asInstanceOf[Seq[Typ]]: _*)
      case (st, _) => st
    }
  }

  // 3.2.6 Annotated Types

  // AnnotType  ::=  SimpleType {Annotation}
  val annotType: Parser[AnnotatedType] = P(simpleType ~ annotation.rep)
    .map { case (tp, attos) => AnnotatedType(tp, attos:_*) }

  // 3.2.7 Compound Types

  val refineStat: Parser[Declaration] = P(dcl | "type" ~ typeDef)

  val refinement = P(nl.? ~ "{" ~ refineStat.rep(min = 1, sep = ";") ~ "}")

  val compoundType: Parser[CompoundType] =
    P( (annotType.rep(min = 1, sep = "with") ~ refinement.?).map { case (ats, refine) =>
          CompoundType(ats, refine.getOrElse(Seq.empty))
        }
     | refinement.map { case refine => CompoundType(Seq.empty, refine)} )

  // 3.2.8

  // InfixType ::= CompoundType {id [nl] CompoundType}
  val infixType = P(compoundType ~ (id ~ nl.? ~ compoundType).rep).map {
    case (ct, cts) => cts.foldLeft(InfixType(ct, None)) { case (left, (op, right)) =>
      InfixType(left, Some(op, right))
    }
  }

  // 3.2.9 Function Types

  val functionArgs: Parser[Seq[Typ]] =
    P( infixType.map { ty => Seq(ty) }
     | ("(" ~/ (paramType ~ ("," ~ paramType).rep).? ~/ ")").map {
        case Some((pt, pts)) => pts.:+(pt)
        case None => Seq.empty
       }
     )

  // 3.2.10 Existential Types

  val existentialDcl: Parser[Declaration] = P(("type" ~/ typeDcl) | ("val" ~/ valDcl))

  val existentialClause: Parser[Seq[Declaration]] = P("forSome" ~ "{" ~ existentialDcl.rep(min = 1, sep = ";") ~ "}")

  val typ: Parser[Typ] =
    P( (functionArgs ~ "=>" ~/ typ).map { FunctionType.tupled }
     | (infixType ~ existentialClause.?).map { case (infix, eClause) => }
     )

}
