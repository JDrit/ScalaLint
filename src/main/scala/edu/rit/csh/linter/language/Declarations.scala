package edu.rit.csh.linter.language

import edu.rit.csh.linter.language.Annotations.Annotation
import edu.rit.csh.linter.language.Expressions.Expression
import edu.rit.csh.linter.language.Types.Typ

object Declarations {

  object Dependency extends Enumeration {
    val Covariant, Contravariant = Value
  }

  abstract class Declaration

  // A value declaration val x: T introduces x as a name of a value of type T.
  case class ValDcl(ids: Seq[Symbol], typ: Typ)

  case class VarDcl(ids: Seq[Symbol], typ: Typ)


  case class VariantTypeParam(annotations: Seq[Annotation], variance: Option[Dependency.Value],
                              param: TypeParam) extends Declaration

  type TypeParamClause = Seq[VariantTypeParam]

  case class Parameter(annotations: Seq[Annotation], name: Symbol, typ: Option[ParamType],
                       expr: Option[Expression]) extends Declaration

  // The most general form of a first-order type parameter is @a1…@an ± t >: L <: U. Here, L,
  // and U are lower and upper bounds that constrain possible type arguments for the parameter.
  // It is a compile-time error if L does not conform to U. ± is a variance, i.e. an optional
  // prefix of either +, or -. One or more annotations may precede the type parameter.
  case class TypeParam(name: String,
                       params: Option[TypeParamClause] = None,
                       lowerBound: Option[Typ] = None,
                       upperBound: Option[Typ] = None,
                       contextBound: Option[Typ] = None)

  abstract class ParamType(typ: Typ) extends Typ

  case class RegularParamType(typ: Typ) extends ParamType(typ)

  // The type of a value parameter may be prefixed by =>, e.g. x: => T. The type of such a
  // parameter is then the parameterless method type => T. This indicates that the corresponding
  // argument is not evaluated at the point of function application, but instead is evaluated at
  // each use within the function. That is, the argument is evaluated using call-by-name.
  case class ByNameParamType(typ: Typ) extends ParamType(typ)

  // The last value parameter of a parameter section may be suffixed by '*', e.g. (..., x:T*).
  // The type of such a repeated parameter inside the method is then the sequence type scala.Seq[T].
  // Methods with repeated parameters T* take a variable number of arguments of type T. That is,
  // if a method m with type (p1:T1,…,pn:Tn,ps:S*)U is applied to arguments (e1,…,ek) where k≥n,
  // then m is taken in that application to have type (p1:T1,…,pn:Tn,ps:S,…,ps′S)U, with k−n
  // occurrences of type S where any parameter names beyond ps are fresh
  case class RepeatedParamType(typ: Typ) extends ParamType(typ)
}
