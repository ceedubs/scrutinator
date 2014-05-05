package net.ceedubs.scrutinator
package swagger

import scalaz._
import scalaz.Leibniz._
import shapeless._
import shapeless.contrib.scalaz._
import shapeless.ops.hlist.ToList
import shapeless.record._
import org.scalatra.swagger.Parameter

trait FieldListSwaggerConverter[L <: HList] {
  def apply(fields: L): ModelState[Seq[Parameter]]
}

object FieldListSwaggerConverter {

  implicit def toSwaggerParamConverter[F[_], L <: HList, O <: HList](implicit traverser: TraverserAux[L, toSwaggerParam.type, F, O], ev: F[O] === ModelState[O], toList: ToList[O, Parameter]): FieldListSwaggerConverter[L] = new FieldListSwaggerConverter[L] {
    def apply(fields: L) = ev(traverser(fields)).map(toList.apply)
  }

  def toSwaggerParams[L <: HList](fields: L)(implicit converter: FieldListSwaggerConverter[L]): ModelState[Seq[Parameter]] =
    converter(fields)
}

object toSwaggerParam extends Poly1 {
  implicit def swaggerParam[K, P](implicit namedParamConverter: NamedParamConverter[K], swaggerConverter: SwaggerParamConverter[NamedParam[P]]) = at[FieldType[K, P]] { param =>
    val namedParam: NamedParam[P] = namedParamConverter.asNamedParam(param)
    swaggerConverter(namedParam) }
}
