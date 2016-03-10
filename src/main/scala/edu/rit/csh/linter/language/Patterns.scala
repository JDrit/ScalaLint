package edu.rit.csh.linter.language

import edu.rit.csh.linter.language.Literals.Literal
import edu.rit.csh.linter.language.Types.Typ


object Patterns {

  type TypPat = Typ

  abstract class Pattern

  // A variable pattern x is a simple identifier which starts with a lower case letter.
  // It matches any value, and binds the variable name to that value. The type of x is the
  // expected type of the pattern as given from outside. A special case is the wild-card
  // pattern _ which is treated as if it was a fresh variable on each occurrence.
  case class VariablePattern(name: Symbol) extends Pattern

  // A typed pattern x:T consists of a pattern variable x and a type pattern T. The type of x
  // is the type pattern T, where each type variable and wildcard is replaced by a fresh,
  // unknown type. This pattern matches any value matched by the type pattern T; it binds
  // the variable name to that value.
  case class TypedPattern(name: Symbol, Typ: TypPat) extends Pattern

  // A pattern binder x@p consists of a pattern variable x and a pattern p. The type of the
  // variable x is the static type T of the pattern p. This pattern matches any value v matched
  // by the pattern p, provided the run-time type of v is also an instance of T, and it binds
  // the variable name to that value.
  case class BindingPattern(name: Symbol, pattern: Pattern) extends Pattern

  // A literal pattern L matches any value that is equal (in terms of ==) to the literal L.
  // The type of L must conform to the expected type of the pattern.
  case class LiteralPattern[T](literal: Literal[T]) extends Pattern

  // A stable identifier pattern is a stable identifier r. The type of r must conform to the
  // expected type of the pattern. The pattern matches any value v such that r == v
  case class StableIdPattern(id: Symbol) extends Pattern

  // A constructor pattern is of the form c(p1,…,pn) where n≥0. It consists of a stable identifier
  // c, followed by element patterns p1,…,pn. The constructor c is a simple or qualified name
  // which denotes a case class. If the case class is monomorphic, then it must conform to the
  // expected type of the pattern, and the formal parameter types of x's primary constructor are
  // taken as the expected types of the element patterns p1,…,pn. If the case class is polymorphic,
  // then its type parameters are instantiated so that the instantiation of c conforms to the
  // expected type of the pattern. The instantiated formal parameter types of c's primary
  // constructor are then taken as the expected types of the component patterns p1,…,pn. The
  // pattern matches all objects created from constructor invocations c(v1,…,vn) where each
  // element pattern pi matches the corresponding value vi.
  case class ConstructorPattern(id: Symbol, patterns: Pattern*) extends Pattern {

    override def toString(): String = s"${id.toString.substring(1)}(${patterns.mkString(", ")}"
  }

  // A tuple pattern (p1,…,pn) is an alias for the constructor pattern scala.Tuplen(p1,…,pn),
  // where n≥2. The empty tuple () is the unique value of type scala.Unit.
  case class TuplePattern(patterns: Pattern*) extends Pattern

  // TODO look into this later
  // case class ExtractorPattern

  case class SequencePattern(id: Symbol, patterns: Seq[Pattern], wildCard: Option[Symbol]) extends Pattern

  // A pattern alternative p1 | … | pn consists of a number of alternative patterns pi.
  // All alternative patterns are type checked with the expected type of the pattern. They may
  // not bind variables other than wildcards. The alternative pattern matches a value v if at
  // least one its alternatives matches v.
  case class AlternativePattern(patterns: Pattern*) extends Pattern {
    override def toString(): String = patterns.mkString(" | ")
  }
}

