package edu.rit.csh.linter.parser

import edu.rit.csh.linter.language.Expressions._
import fastparse.all._
import fastparse.core.Parser

object Expressions {

  val expr: Parser[Expression] = ???

  val exprs: Parser[Seq[Expression]] = (expr ~ ("," ~ expr).rep).map { case (e, es) => es :+ e }


  // TODO
  val simpleExpr1: Parser[Expression] =
    ( Literals.literal.map { case lit => ExpressionLiteral(lit) }
    | Types.path.map { DesignatorExpression }
    )

  val simpleExpr: Parser[Expression] = ???

  val prefixExpr: Parser[OpExpression] =
    ( "-" ~ simpleExpr.map { case exp => OpExpression(Operator.Neg, exp) }
    | "+" ~ simpleExpr.map { case exp => OpExpression(Operator.Plus, exp) }
    | "~" ~ simpleExpr.map { case exp => OpExpression(Operator.Tilda, exp) }
    | "!" ~ simpleExpr.map { case exp => OpExpression(Operator.Bang, exp) }
    )

  val argumentExprs: Parser[Seq[Expression]] = "(" ~ exprs.?.map { case Some(e) => e ; case None => Seq.empty } ~ ")"
}
