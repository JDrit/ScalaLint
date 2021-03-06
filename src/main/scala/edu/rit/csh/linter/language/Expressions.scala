package edu.rit.csh.linter.language

import edu.rit.csh.linter.language.Literals.Literal
import edu.rit.csh.linter.language.Types.Typ

object Expressions {

  abstract class Expression

  case class DefaultExpression(typ: Typ) extends Expression

  case class IfExp(cond: Expression, thenExpr: Expression, elseExpr: Option[Expression]) extends Expression

  case class WhileExp(cond: Expression, exp: Expression) extends Expression

  case class TryExp(tryExp: Expression, catchExp: Option[Expression], finallyExp: Option[Expression])

  case class Binding(id: Symbol, typ: Option[Typ]) extends Expression

  // A designator refers to a named term. It can be a simple name or a selection
  case class DesignatorExpression(str: Symbol) extends Expression

  object Operator extends Enumeration {
    val Neg, Plus, Tilda, Bang = Value
  }

  case class OpExpression(op: Operator.Value, expr: Expression) extends Expression

  case class LiteralExpression[T](literal: Literal[T]) extends Expression

  // A tuple expression (e1,…,en) is an alias for the class instance creation scala.Tuplen(e1,…,en),
  // where n≥2. The empty tuple () is the unique value of type scala.Unit.
  case class TupleExpression(exprs: Expression*) extends Expression
}
