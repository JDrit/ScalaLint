package edu.rit.csh.linter.language


object Literals {

  abstract class Literal[T](value: T)

  case class IntegerLiteral(val value: Int) extends Literal[Int](value: Int) {
    override def toString(): String = s"$value /* integer literal */"
  }

  case class FloatingLiteral(val value: Double) extends Literal[Double](value: Double) {
    override def toString(): String = s"$value /* floating literal */"
  }

  case class BooleanLiteral(val value: Boolean) extends Literal[Boolean](value: Boolean) {
    override def toString(): String = s"$value /* boolean literal */"
  }

  case class CharacterLiteral(val value: Int) extends Literal[Int](value: Int) {
    override def toString(): String = s":$value /* character literal */"
  }

  case class StringLiteral(val value: String) extends Literal[String](value: String) {
    override def toString(): String = s"$value /* string literal */"
  }

  case class SymbolLiteral(val value: Symbol) extends Literal[Symbol](value: Symbol) {
    override def toString(): String = s"$value /* symbol literal */"
  }

  case class NullLiteral() extends Literal[Null](null: Null) {
    override def toString(): String = "null /* null literal */"
  }

}
