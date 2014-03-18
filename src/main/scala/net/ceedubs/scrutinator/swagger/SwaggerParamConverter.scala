package net.ceedubs.scrutinator
package swagger

import org.scalatra.swagger.{ AllowableValues, DataType, Parameter }
import ValueSource._

trait SwaggerDataTypeConverter[A] {
  def dataType: DataType
}

object SwaggerDataTypeConverter {
  def apply[A](d: DataType): SwaggerDataTypeConverter[A] = new SwaggerDataTypeConverter[A] {
    val dataType = d
  }

  implicit val intSwaggerDataConverter: SwaggerDataTypeConverter[Int] = apply[Int](DataType.Int)
  implicit val stringSwaggerDataConverter: SwaggerDataTypeConverter[String] = apply[String](DataType.String)
  implicit val booleanSwaggerDataConverter: SwaggerDataTypeConverter[Boolean] = apply[Boolean](DataType.Boolean)
  implicit val longSwaggerDataConverter: SwaggerDataTypeConverter[Long] = apply[Long](DataType.Long)
  implicit val dateSwaggerDataConverter: SwaggerDataTypeConverter[java.util.Date] = apply[java.util.Date](DataType.Date)

}

trait SwaggerSourceConverter[S <: ValueSource] {
  def sourceType: org.scalatra.swagger.ParamType.ParamType
}

object SwaggerSourceConverter {
  def apply[S <: ValueSource](s: org.scalatra.swagger.ParamType.ParamType): SwaggerSourceConverter[S] = 
    new SwaggerSourceConverter[S] {
      val sourceType = s
    }

  implicit val headerSourceConverter: SwaggerSourceConverter[Headers] =
    SwaggerSourceConverter(org.scalatra.swagger.ParamType.Header)

  implicit val queryStringSourceConverter: SwaggerSourceConverter[QueryString] =
    SwaggerSourceConverter(org.scalatra.swagger.ParamType.Query)
}

trait SwaggerParamConverter[A] {
  def apply(a: A): Parameter
}

object SwaggerParamConverter extends NamedParamConverters with RequiredParamConverters with ParamWithDefaultConverters {
  def apply[A](f: A => Parameter): SwaggerParamConverter[A] = new SwaggerParamConverter[A] {
    def apply(a: A) = f(a)
  }
}

trait NamedParamConverters {
  implicit def namedParamConverter[A, S <: ValueSource](
      implicit sourceConverter: SwaggerSourceConverter[S], dataTypeConverter: SwaggerDataTypeConverter[A]): SwaggerParamConverter[NamedParam[Param[A, S]]] = {
    SwaggerParamConverter[NamedParam[Param[A, S]]] { namedParam =>
      Parameter(
        name = namedParam.name,
        `type` = dataTypeConverter.dataType,
        description = namedParam.param.description,
        notes = None, // TODO
        paramType = sourceConverter.sourceType,
        defaultValue = None,
        allowableValues = AllowableValues.AnyValue, // TODO
        required = false,
        position = 0) // TODO
    }
  }
}

trait RequiredParamConverters {
  implicit def namedRequiredParamConverter[A](implicit converter: SwaggerParamConverter[NamedParam[A]]): SwaggerParamConverter[NamedParam[RequiredParam[A]]] = {
    SwaggerParamConverter[NamedParam[RequiredParam[A]]] { namedRequiredParam =>
      val namedInnerParam = NamedParam[A](namedRequiredParam.name, namedRequiredParam.param.param)
      converter(namedInnerParam).copy(required = true)
    }
  }
}

trait ParamWithDefaultConverters {
  implicit def ParamWithDefaultConverter[A, S <: ValueSource](implicit converter: SwaggerParamConverter[NamedParam[Param[A, S]]], showA: SwaggerShow[A]): SwaggerParamConverter[NamedParam[ParamWithDefault[A, S]]] = {
    SwaggerParamConverter[NamedParam[ParamWithDefault[A, S]]] { namedParamWithDefault =>
      val namedInnerParam = NamedParam[Param[A, S]](namedParamWithDefault.name, namedParamWithDefault.param.param)
      converter(namedInnerParam).copy(
        defaultValue = Some(showA.shows(
          SwaggerSpec(namedParamWithDefault.param.default))))
    }
  }
}
