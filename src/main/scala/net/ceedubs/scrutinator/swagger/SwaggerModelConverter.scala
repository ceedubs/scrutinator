package net.ceedubs.scrutinator
package swagger

import net.ceedubs.scrutinator.json4s.readers._
import JsonParam._
import scalaz._
import scalaz.Leibniz._
import shapeless._
import shapeless.record._
import shapeless.ops.hlist.{ Mapper, ToList }
import shapeless.contrib.scalaz._
import org.scalatra.swagger.{ AllowableValues, DataType, Model, ModelProperty }

trait SwaggerModelConverter[A] {
  def apply(a: A): State[Map[String, Model], Model]
}

object SwaggerModelConverter extends JsonBodyModelConverters {
  def apply[A](f: A => State[Map[String, Model], Model]): SwaggerModelConverter[A] = new SwaggerModelConverter[A] {
    def apply(a: A) = f(a)
  }
}

trait SwaggerModelPropertyConverter[A] {
  def apply(a: A): State[Map[String, Model], ModelProperty]
}

object SwaggerModelPropertyConverter extends JsonFieldModelPropertyConverters {
  def apply[A](f: A => State[Map[String, Model], ModelProperty]): SwaggerModelPropertyConverter[A] = new SwaggerModelPropertyConverter[A] {
    def apply(a: A) = f(a)
  }
}

object toSwaggerModelProperty extends Poly1 {
  implicit def viaConverter[K, P](implicit namedParamConverter: NamedParamConverter[K], swaggerConverter: SwaggerModelPropertyConverter[NamedParam[P]]) =
    at[FieldType[K, P]] { param =>
      val namedParam: NamedParam[P] = namedParamConverter.asNamedParam(param) 
      swaggerConverter(namedParam).map(p => NamedParam[ModelProperty](namedParam.name, p))
    }
}

trait JsonBodyModelConverters {
  implicit def namedJsonBodyConverter[F[_], L <: HList, O <: HList](implicit traverser: TraverserAux[L, toSwaggerModelProperty.type, F, O], toList: ToList[O, NamedParam[ModelProperty]], ev: F[O] === State[Map[String, Model], O]): SwaggerModelConverter[NamedParam[JsonObjectParam[L]]] =
    SwaggerModelConverter[NamedParam[JsonObjectParam[L]]](namedParam =>
      State[Map[String, Model], Model](s =>
        s.get(namedParam.name)
        .map(m => s -> m)
        .getOrElse {
          val (s2, fields) = ev(traverser(namedParam.param.fields)).apply(s)
          val model = Model(
            id = namedParam.name,
            name = namedParam.name,
            qualifiedName = None,
            description = None,
            properties = toList(fields).map(p => (p.name, p.param)))
          (s2 + (namedParam.name -> model), model)
        }))
}

trait JsonFieldModelPropertyConverters {
  implicit def jsonFieldModelPropertyConverter[A](implicit dataTypeConverter: SwaggerDataTypeConverter[A]): SwaggerModelPropertyConverter[NamedParam[JsonFieldParam[A]]] =
    SwaggerModelPropertyConverter[NamedParam[JsonFieldParam[A]]](namedParam =>
      State.state(
        ModelProperty(
          `type` = dataTypeConverter.dataType,
          required = false,
          description = None /* TODO */)))

  implicit def requiredJsonFieldModelPropertyConverter[A](implicit converter: SwaggerModelPropertyConverter[NamedParam[JsonFieldParam[A]]]): SwaggerModelPropertyConverter[NamedParam[RequiredParam[JsonFieldParam[A]]]] =
    SwaggerModelPropertyConverter[NamedParam[RequiredParam[JsonFieldParam[A]]]] { namedParam =>
      val nestedNamedParam = NamedParam(namedParam.name, namedParam.param.param)
      converter(nestedNamedParam).map(_.copy(required = true))
    }

}
