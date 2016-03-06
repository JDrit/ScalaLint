package edu.rit.csh.linter.parser

import edu.rit.csh.linter.language.Annotations.Annotation
import fastparse.all._
import fastparse.core.Parser

object Annotations {
  import Types._
  import Expressions._

  val annotation: Parser[Annotation] = ("@" ~ simpleType ~ argumentExprs.rep).map { Annotation.tupled }

}
