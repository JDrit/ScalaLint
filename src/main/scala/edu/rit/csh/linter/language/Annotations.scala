package edu.rit.csh.linter.language

import edu.rit.csh.linter.language.Expressions.Expression
import edu.rit.csh.linter.language.Types.SimpleType


object Annotations {

  case class Annotation(typ: SimpleType, args: Expression*)
}
