package edu.rit.csh.linter.parser

import edu.rit.csh.linter.language.Expressions._
import fastparse.all._
import fastparse.core.Parser

object Expressions {
  import Literals._

  val expr1: Parser[Expression] =
    P( ("if" ~ "(" ~ expr ~ ")" ~ nl.rep ~ expr ~ (semi.? ~ "else" ~ expr).?).map {
        case (cond, thenExpr, elseExpr) => IfExp(cond, thenExpr, elseExpr)
      }
     | ("while" ~ "(" ~ expr ~ ")" ~ nl.rep ~ expr).map { case (cond, exp) => WhileExp(cond, exp) }
     ).opaque("expression")

  val expr: Parser[Expression] = expr1

  val exprs: Parser[Seq[Expression]] = P(expr ~ ("," ~ expr).rep).map { case (e, es) => es :+ e }


  // TODO
  val simpleExpr1: Parser[Expression] =
    P( Literals.literal.map { case lit => LiteralExpression(lit) }
     | Types.path.map { DesignatorExpression } )

  val simpleExpr: Parser[Expression] = simpleExpr1 ~ "_".?

  val prefixExpr: Parser[Expression] =
    P( "-" ~/ simpleExpr.map { case exp => OpExpression(Operator.Neg, exp) }
     | "+" ~/ simpleExpr.map { case exp => OpExpression(Operator.Plus, exp) }
     | "~" ~/ simpleExpr.map { case exp => OpExpression(Operator.Tilda, exp) }
     | "!" ~/ simpleExpr.map { case exp => OpExpression(Operator.Bang, exp) }
     | simpleExpr
     )

  val argumentExprs: Parser[Seq[Expression]] = P("(" ~/ exprs.?.map { case Some(e) => e ; case None => Seq.empty } ~/ ")")

  val block = P(???)
}
