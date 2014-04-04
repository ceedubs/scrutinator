package net.ceedubs.scrutinator
package swagger

import scalaz._
import scalaz.Leibniz._
import shapeless._
import shapeless.record._
import shapeless.contrib.scalaz._
import shapeless.ops.hlist.{ Mapper, ToList }
import org.scalatra.swagger.{ Model, Parameter }

object SwaggerSupport {

  object toSwaggerParam extends Poly1 {
    implicit def swaggerParam[K, P](implicit namedParamConverter: NamedParamConverter[K], swaggerConverter: SwaggerParamConverter[NamedParam[P]]) = at[FieldType[K, P]] { param =>
      val namedParam: NamedParam[P] = namedParamConverter.asNamedParam(param)
      swaggerConverter(namedParam) }
  }

  def toSwaggerParams[F[_], I <: HList, O <: HList](fields: I)(implicit traverser: TraverserAux[I, toSwaggerParam.type, F, O], ev: F[O] === ModelState[O], toList: ToList[O, Parameter]): ModelState[Seq[Parameter]] = {
    ev(traverser(fields)).map(toList.apply)
  }
}
