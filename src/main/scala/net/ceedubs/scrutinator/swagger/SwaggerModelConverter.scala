package net.ceedubs.scrutinator
package swagger

import net.ceedubs.scrutinator.json4s.readers._
import scalaz.{ @@ => _, _ }
import scalaz.Leibniz._
import shapeless._
import shapeless.tag
import shapeless.tag._
import shapeless.record._
import shapeless.ops.hlist.{ Mapper, ToList }
import shapeless.contrib.scalaz._
import org.scalatra.swagger.{ AllowableValues, DataType, Model, ModelProperty }

sealed trait SwaggerModelId

object ModelId {
  val tagger: Tagger[SwaggerModelId] = tag[SwaggerModelId]
  def apply(id: String): ModelId = tagger[String](id)
}

trait SwaggerModelConverter[A] {
  def apply(a: A): ModelState[Model]
}

object SwaggerModelConverter extends SwaggerModelConverters {
  def apply[A](f: A => ModelState[Model]): SwaggerModelConverter[A] = new SwaggerModelConverter[A] {
    def apply(a: A) = f(a)
  }
}

trait SwaggerModelPropertyConverter[A] {
  def apply(a: A): ModelState[ModelProperty]
}

object SwaggerModelPropertyConverter extends FieldModelPropertyConverters {
  def apply[A](f: A => ModelState[ModelProperty]): SwaggerModelPropertyConverter[A] = new SwaggerModelPropertyConverter[A] {
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

trait SwaggerModelConverters {
  implicit def swaggerModelConverter[F[_], L <: HList, O <: HList](implicit traverser: TraverserAux[L, toSwaggerModelProperty.type, F, O], toList: ToList[O, NamedParam[ModelProperty]], ev: F[O] === ModelState[O]): SwaggerModelConverter[SwaggerModel[L]] = SwaggerModelConverter[SwaggerModel[L]](swaggerModel =>
      ModelState[Model] { s =>
        val modelId = ModelId(swaggerModel.modelId)
        s.get(modelId)
        .map(m => s -> m)
        .getOrElse {
          val (s2, fields) = ev(traverser(swaggerModel.fields)).apply(s)
          val model = Model(
            id = modelId,
            name = modelId,
            qualifiedName = None,
            description = None,
            properties = toList(fields).map(p => (p.name, p.param)))
          (s2 + (modelId -> model), model)
        }})
}

trait FieldModelPropertyConverters {
  implicit def fieldModelPropertyConverter[A](implicit dataTypeConverter: SwaggerDataTypeConverter[A]): SwaggerModelPropertyConverter[NamedParam[Param[A]]] =
    SwaggerModelPropertyConverter[NamedParam[Param[A]]](namedParam =>
      State.state(
        ModelProperty(
          `type` = dataTypeConverter.dataType,
          required = false,
          description = None /* TODO */)))

  implicit def requiredFieldModelPropertyConverter[A](implicit converter: SwaggerModelPropertyConverter[NamedParam[Param[A]]]): SwaggerModelPropertyConverter[NamedParam[RequiredParam[Param[A]]]] =
    SwaggerModelPropertyConverter[NamedParam[RequiredParam[Param[A]]]] { namedParam =>
      val nestedNamedParam = NamedParam(namedParam.name, namedParam.param.param)
      converter(nestedNamedParam).map(_.copy(required = true))
    }

}

object ModelState {
  def apply[A](f: ModelsCache => (ModelsCache, A)): ModelState[A] = State[ModelsCache, A](f)
}
