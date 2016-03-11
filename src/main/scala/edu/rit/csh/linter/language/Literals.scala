package edu.rit.csh.linter.language


object Literals {

  abstract class Literal[T](value: T) {
    override def toString(): String = value.toString
  }

  case class IntegerLiteral(val value: Int) extends Literal[Int](value: Int)

  case class FloatingLiteral(val value: Double) extends Literal[Double](value: Double)

  case class BooleanLiteral(val value: Boolean) extends Literal[Boolean](value: Boolean)

  case class CharacterLiteral(val value: Int) extends Literal[Int](value: Int)

  case class StringLiteral(val value: String) extends Literal[String](value: String)

  case class SymbolLiteral(val value: String) extends Literal[String](value: String)

  case class NullLiteral() extends Literal[Null](null: Null)

}
