package edu.rit.csh.linter

import edu.rit.csh.linter.language.Annotations.Annotation
import edu.rit.csh.linter.language.Types.TypeDesignator
import edu.rit.csh.linter.parser.Annotations
import org.scalatest.FunSuite

class AnnotationsTest extends FunSuite {
  import TestUtils._

  test("simple annotation") {
    parse("@transient", Annotations.annotation, Annotation(TypeDesignator('transient)))
  }

  test("annotation with arguments") {
    parse("@deprecated(\"Use D\", \"1.0\")", Annotations.annotation, Annotation(TypeDesignator('deprecated)))
  }
}
