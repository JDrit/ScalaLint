package edu.rit.csh.linter.parser

import edu.rit.csh.linter.language.Expressions._
import fastparse.all._
import fastparse.core.Parser

object Expressions {
  import Literals._

  val expr1: Parser[Expression] =
    ( ("if" ~ "(" ~ expr ~ ")" ~ nl.rep ~ expr ~ (semi.? ~ "else" ~ expr).?).map {
        case (cond, thenExpr, elseExpr) => IfExp(cond, thenExpr, elseExpr)
      }
    | ("while" ~ "(" ~ expr ~ ")" ~ nl.rep ~ expr).map { case (cond, exp) => WhileExp(cond, exp) }
    )

  val expr: Parser[Expression] = expr1

  val exprs: Parser[Seq[Expression]] = (expr ~ ("," ~ expr).rep).map { case (e, es) => es :+ e }


  // TODO
  val simpleExpr1: Parser[Expression] =
    ( Literals.literal.map { case lit => ExpressionLiteral(lit) }
    | Types.path.map { DesignatorExpression }
    )

  val simpleExpr: Parser[Expression] = simpleExpr1 ~ "_".?

  val prefixExpr: Parser[OpExpression] =
    ( "-" ~ simpleExpr.map { case exp => OpExpression(Operator.Neg, exp) }
    | "+" ~ simpleExpr.map { case exp => OpExpression(Operator.Plus, exp) }
    | "~" ~ simpleExpr.map { case exp => OpExpression(Operator.Tilda, exp) }
    | "!" ~ simpleExpr.map { case exp => OpExpression(Operator.Bang, exp) }
    )

  val argumentExprs: Parser[Seq[Expression]] = "(" ~ exprs.?.map { case Some(e) => e ; case None => Seq.empty } ~ ")"
}
