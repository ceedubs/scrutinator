package net.ceedubs.scrutinator
package swagger

import shapeless._
import shapeless.record._
import shapeless.ops.hlist.{ Mapper, ToList }
import org.scalatra.swagger.Parameter

object SwaggerSupport {

  object toSwaggerParam extends Poly1 {
    implicit def swaggerParam[K, P](implicit namedParamConverter: NamedParamConverter[K], swaggerConverter: SwaggerParamConverter[NamedParam[P]]) = at[FieldType[K, P]] { param =>
      val namedParam: NamedParam[P] = namedParamConverter.asNamedParam(param)
      swaggerConverter(namedParam) }
  }

  def toSwaggerParams[I <: HList, O <: HList](fields: I)(implicit mapper: Mapper.Aux[toSwaggerParam.type, I, O], toList: ToList[O, Parameter]): Seq[Parameter] = {
    toList(mapper(fields))
  }
}
