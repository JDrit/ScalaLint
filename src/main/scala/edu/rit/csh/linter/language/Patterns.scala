package edu.rit.csh.linter.language

import edu.rit.csh.linter.language.Literals.Literal
import edu.rit.csh.linter.language.Types.Typ


object Patterns {

  type TypPat = Typ

  abstract class Pattern

  // A typed pattern x:T consists of a pattern variable x and a type pattern T. The type of x
  // is the type pattern T, where each type variable and wildcard is replaced by a fresh,
  // unknown type. This pattern matches any value matched by the type pattern T; it binds
  // the variable name to that value.
  case class TypedPattern(name: String, Typ: TypPat) extends Pattern

  // A pattern binder x@p consists of a pattern variable x and a pattern p. The type of the
  // variable x is the static type T of the pattern p. This pattern matches any value v matched
  // by the pattern p, provided the run-time type of v is also an instance of T, and it binds
  // the variable name to that value.
  case class PatternBinder(name: String, pattern: Pattern) extends Pattern

  // A literal pattern L matches any value that is equal (in terms of ==) to the literal L.
  // The type of L must conform to the expected type of the pattern.
  case class LiteralPattern(literal: Literal) extends Pattern

  
}
