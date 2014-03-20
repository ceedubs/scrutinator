package net.ceedubs.scrutinator
package scalatra
package readers

import net.ceedubs.scrutinator.readers._
import scalaz._
import org.scalatra.validation.{ FieldName, ValidationError }
import org.scalatra.MultiParams

trait PathReaders {
  import PathReaders._
  import Param._

  implicit def pathNamedParamReader[A](implicit reader: ParamReader[ErrorsOrMaybe, (FieldKey, PathParams), A]): ParamReader[ErrorsOrMaybe, (NamedParam[PathParam[A]], Request), A] = {
    ParamReader[ErrorsOrMaybe, (NamedParam[PathParam[A]], Request), A](Function.tupled { (namedParam, request) =>
      val fieldKey = FieldKey(name = namedParam.name, prettyName = namedParam.param.prettyName) 
      val multiParams: Option[Map[String, Seq[String]]] = request.get(org.scalatra.MultiParamsKey).flatMap(x =>
        if (x.isInstanceOf[MultiParams]) Some(x.asInstanceOf[MultiParams]) else None)
      val params = new org.scalatra.util.MultiMapHeadView[String, String] {
        val multiMap = multiParams.getOrElse(Map.empty)
      }

      val pathParams = PathParams(params)
      reader.reader((fieldKey, pathParams)).flatMap { maybeA =>
        std.option.cata(maybeA)({ a =>
          val errors = namedParam.param.validations.map(_.apply(fieldKey, a)
            .map(e => ValidationError(e, FieldName(fieldKey.name)))).flatten
          std.option.toFailure(std.list.toNel(errors))(Some(a))
        }, Validation.success(None))
      }
    })
  }

  implicit val pathStringFieldReader: ParamReader[ErrorsOrMaybe, (FieldKey, PathParams), String] = {
    val kleisli = Kleisli[ErrorsOrMaybe, (FieldKey, PathParams), String](Function.tupled(
      (fieldKey, pathParams) =>
        Validation.success(pathParams.get(fieldKey.name).filterNot(_.isEmpty))))
    ParamReader.fromKleisli(kleisli)
 
  }

}

object PathReaders extends PathReaders {
  import ValueSource.Path

  type PathParams = Map[String, String] @@ Path

  object PathParams {
    def apply(params: Map[String, String]): PathParams = Tag[Map[String, String], Path](params)
  }
}
