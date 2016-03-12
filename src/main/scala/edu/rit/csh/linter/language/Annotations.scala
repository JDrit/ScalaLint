package edu.rit.csh.linter.language

import edu.rit.csh.linter.language.Expressions.Expression
import edu.rit.csh.linter.language.Types.Typ


object Annotations {

  case class Annotation(typ: Typ, args: Expression*)
}
