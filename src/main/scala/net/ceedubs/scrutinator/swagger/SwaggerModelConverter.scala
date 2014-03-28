package net.ceedubs.scrutinator
package swagger

import net.ceedubs.scrutinator.json4s.readers._
import JsonParam._
import shapeless._
import shapeless.record._
import shapeless.ops.hlist.{ Mapper, ToList }
import org.scalatra.swagger.{ AllowableValues, DataType, Model, ModelProperty }

trait SwaggerModelConverter[A] {
  def apply(a: A): Model
}

object SwaggerModelConverter extends JsonBodyModelConverters {
  def apply[A](f: A => Model): SwaggerModelConverter[A] = new SwaggerModelConverter[A] {
    def apply(a: A): Model = f(a)
  }
}

trait SwaggerModelPropertyConverter[A] {
  def apply(a: A): ModelProperty
}

object SwaggerModelPropertyConverter extends JsonFieldModelPropertyConverters {
  def apply[A](f: A => ModelProperty): SwaggerModelPropertyConverter[A] = new SwaggerModelPropertyConverter[A] {
    def apply(a: A): ModelProperty = f(a)
  }
}

object toSwaggerModelProperty extends Poly1 {
  implicit def viaConverter[K, P](implicit namedParamConverter: NamedParamConverter[K], swaggerConverter: SwaggerModelPropertyConverter[NamedParam[P]]) =
    at[FieldType[K, P]] { param =>
      val namedParam: NamedParam[P] = namedParamConverter.asNamedParam(param) 
      NamedParam[ModelProperty](namedParam.name, swaggerConverter(namedParam))
    }
}

trait JsonBodyModelConverters {
  implicit def namedJsonBodyConverter[L <: HList, O <: HList](implicit mapper: Mapper.Aux[toSwaggerModelProperty.type, L, O], toList: ToList[O, NamedParam[ModelProperty]]): SwaggerModelConverter[NamedParam[JsonObjectParam[L]]] =
    SwaggerModelConverter[NamedParam[JsonObjectParam[L]]](namedParam =>
      Model(id = namedParam.name,
        name = namedParam.name,
        qualifiedName = None,
        description = None,
        properties = toList(mapper(namedParam.param.fields)).map(p => (p.name, p.param))))
}

trait JsonFieldModelPropertyConverters {
  implicit def jsonFieldModelPropertyConverter[A](implicit dataTypeConverter: SwaggerDataTypeConverter[A]): SwaggerModelPropertyConverter[NamedParam[JsonFieldParam[A]]] =
    SwaggerModelPropertyConverter[NamedParam[JsonFieldParam[A]]](namedParam =>
      ModelProperty(
        `type` = dataTypeConverter.dataType,
        required = false,
        description = None /* TODO */))

  implicit def requiredJsonFieldModelPropertyConverter[A](implicit converter: SwaggerModelPropertyConverter[NamedParam[JsonFieldParam[A]]]): SwaggerModelPropertyConverter[NamedParam[RequiredParam[JsonFieldParam[A]]]] =
    SwaggerModelPropertyConverter[NamedParam[RequiredParam[JsonFieldParam[A]]]] { namedParam =>
      val nestedNamedParam = NamedParam(namedParam.name, namedParam.param.param)
      converter(nestedNamedParam).copy(required = true)
    }

}
