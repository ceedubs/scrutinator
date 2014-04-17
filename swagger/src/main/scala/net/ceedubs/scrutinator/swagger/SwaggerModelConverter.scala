package net.ceedubs.scrutinator
package swagger

import scalaz.{ @@ => _, _ }
import scalaz.Leibniz._
import shapeless._
import shapeless.tag
import shapeless.tag._
import shapeless.record._
import shapeless.ops.hlist.{ Mapper, ToList }
import shapeless.contrib.scalaz._
import org.scalatra.swagger.{ AllowableValues, DataType, Model, ModelProperty }

sealed trait ModelIdTag

object ModelId {
  val tagger: Tagger[ModelIdTag] = tag[ModelIdTag]
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

trait ModelWithIdPropertyConverter[A] {
  def apply(a: A): ModelState[ModelProperty]
}

object ModelWithIdPropertyConverter extends FieldModelPropertyConverters {
  def apply[A](f: A => ModelState[ModelProperty]): ModelWithIdPropertyConverter[A] = new ModelWithIdPropertyConverter[A] {
    def apply(a: A) = f(a)
  }
}

object toModelWithIdProperty extends Poly1 {
  implicit def viaConverter[K, P](implicit namedParamConverter: NamedParamConverter[K], swaggerConverter: ModelWithIdPropertyConverter[NamedParam[P]]) =
    at[FieldType[K, P]] { param =>
      val namedParam: NamedParam[P] = namedParamConverter.asNamedParam(param) 
      swaggerConverter(namedParam).map(p => NamedParam[ModelProperty](namedParam.name, p))
    }
}

trait SwaggerModelConverters {
  implicit def modelWithIdConverter[F[_], L <: HList, O <: HList](implicit traverser: TraverserAux[L, toModelWithIdProperty.type, F, O], toList: ToList[O, NamedParam[ModelProperty]], ev: F[O] === ModelState[O]): SwaggerModelConverter[ModelWithId[L]] = SwaggerModelConverter[ModelWithId[L]](modelWithId =>
      ModelState[Model] { s =>
        val modelId = ModelId(modelWithId.id)
        s.get(modelId)
        .map(m => s -> m)
        .getOrElse {
          val (s2, fields) = ev(traverser(modelWithId.model.fields)).apply(s)
          val model = Model(
            id = modelId,
            name = modelId,
            qualifiedName = None,
            description = modelWithId.model.description,
            properties = toList(fields).map(p => (p.name, p.param)))
          (s2 + (modelId -> model), model)
        }})
}

trait FieldModelPropertyConverters {
  implicit def fieldModelPropertyConverter[A](implicit dataTypeConverter: SwaggerDataTypeConverter[A]): ModelWithIdPropertyConverter[NamedParam[Field[A]]] =
    ModelWithIdPropertyConverter[NamedParam[Field[A]]](namedParam =>
      State.state(
        ModelProperty(
          `type` = dataTypeConverter.dataType,
          required = false,
          description = namedParam.param.description)))

  implicit def requiredFieldModelPropertyConverter[P](implicit converter: ModelWithIdPropertyConverter[NamedParam[P]]): ModelWithIdPropertyConverter[NamedParam[RequiredParam[P]]] =
    ModelWithIdPropertyConverter[NamedParam[RequiredParam[P]]] { namedParam =>
      val nestedNamedParam = NamedParam(namedParam.name, namedParam.param.param)
      converter(nestedNamedParam).map(_.copy(required = true))
    }

  implicit def modelModelPropertyConverter[L <: HList](implicit modelConverter: SwaggerModelConverter[ModelWithId[L]]): ModelWithIdPropertyConverter[NamedParam[ModelField[ModelWithId[L]]]] =
    ModelWithIdPropertyConverter[NamedParam[ModelField[ModelWithId[L]]]](namedParam =>
      modelConverter(namedParam.param.model).
      map(model =>
        ModelProperty(
          `type` = DataType(model.id),
          required = false,
          description = namedParam.param.description)))

}

object ModelState {
  def apply[A](f: ModelsCache => (ModelsCache, A)): ModelState[A] = State[ModelsCache, A](f)
}
