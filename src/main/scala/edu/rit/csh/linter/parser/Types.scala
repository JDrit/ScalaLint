package edu.rit.csh.linter.parser


import edu.rit.csh.linter.language.Declarations.Declaration
import edu.rit.csh.linter.language.Patterns.{ConstructorPattern, Pattern}
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

  val types = P(typ.rep(min = 1, sep = ","))

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
  val annotType: Parser[Typ] = P(simpleType ~ annotation.rep).map {
    case (tp, Nil) => tp
    case (tp, attos) => AnnotatedType(tp, attos:_*)
  }

  // 3.2.7 Compound Types

  val refineStat: Parser[Declaration] = P(dcl | "type" ~ typeDef)

  val refinement: Parser[Seq[Declaration]] = P(nl.? ~ "{" ~ refineStat.rep(min = 1, sep = ";") ~ "}")

  val compoundType: Parser[Typ] =
    P( (annotType.rep(min = 1, sep = "with") ~ refinement.?).map {
        case (ats, None) => if (ats.length == 1) ats.head
                            else CompoundType(ats, Seq.empty)
        case (ats, Some(refines)) => CompoundType(ats, refines)
      }
     | refinement.map { case refine => CompoundType(Seq.empty, refine)} )

  // 3.2.8

  val infixType: Parser[Typ] = P(compoundType ~ (id ~ nl.? ~ compoundType).rep).map {
    case (ct, Nil) => ct
    case (ct, cts) => toInfix(ct, cts.toList)
  }

  private def toInfix(start: Typ, input: List[(Symbol, Typ)]): Typ = input match {
    case Nil => start
    case (op, sp) :: Nil => InfixType(start, op, sp)
    case (op, sp) :: pts => InfixType(start, op, toInfix(sp, pts))
  }

  // 3.2.9 Function Types

  val functionArgs: Parser[Seq[Typ]] =
    P( infixType.map { ty => Seq(ty) } | ("(" ~/ paramType.rep(min = 0, sep = ",") ~/ ")"))

  // 3.2.10 Existential Types

  val existentialDcl: Parser[Declaration] = P(("type" ~/ typeDcl) | ("val" ~/ valDcl))

  val existentialClause: Parser[Seq[Declaration]] = P("forSome" ~ "{" ~ existentialDcl.rep(min = 1, sep = ";") ~ "}")

  val typ: Parser[Typ] =
    P( (functionArgs ~ "=>" ~/ typ).map { FunctionType.tupled }
     | (infixType ~ existentialClause.?).map {
          case (it, None) => it
          case (it, Some(ec)) => ExistentialType(it, ec:_*)
       }
     )

}
