package edu.rit.csh.linter.language

import edu.rit.csh.linter.language.Annotations.Annotation
import edu.rit.csh.linter.language.Types.Typ

object Declarations {

  object Dependency extends Enumeration {
    val Covariant, Contravariant = Value
  }

  abstract class Declaration

  // A value declaration val x: T introduces x as a name of a value of type T.
  case class ValDcl(ids: Seq[Symbol], typ: Typ)

  case class VariantTypeParam(annotations: Seq[Annotation], variance: Option[Dependency.Value], param: TypeParam) extends Declaration

  type TypeParamClause = Seq[VariantTypeParam]

  // The most general form of a first-order type parameter is @a1…@an ± t >: L <: U. Here, L,
  // and U are lower and upper bounds that constrain possible type arguments for the parameter.
  // It is a compile-time error if L does not conform to U. ± is a variance, i.e. an optional
  // prefix of either +, or -. One or more annotations may precede the type parameter.
  case class TypeParam(name: String,
                       params: Option[TypeParamClause] = None,
                       lowerBound: Option[Typ] = None,
                       upperBound: Option[Typ] = None,
                       contextBound: Option[Typ] = None)
}
