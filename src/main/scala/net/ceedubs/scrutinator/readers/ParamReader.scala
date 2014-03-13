package net.ceedubs.scrutinator
package readers

import scalaz.{ @@ => _, _}
import scalaz.std.option.toSuccess
import org.scalatra.validation.{ FieldName, ValidationError }

trait ParamReader[M[+_], -I, +O] {
 
  def reader: Kleisli[M, I, O]

  def transform[M2[+_], I2, O2](f: Kleisli[M, I, O] => Kleisli[M2, I2, O2]): ParamReader[M2, I2, O2] = ParamReader.fromKleisli(f(reader))

}

object ParamReader extends QueryStringReaders with OptionalParamReaders with RequiredParamReaders {

  def apply[M[+_], I, O](f: I => M[O]): ParamReader[M, I, O] = fromKleisli(Kleisli(f))

  def fromKleisli[M[+_], I, O](k: Kleisli[M, I, O]): ParamReader[M, I, O] = new ParamReader[M, I, O] {

    val reader: Kleisli[M, I, O] = k
  }
}

final case class FieldKey(name: String, prettyName: Option[String]) {
  def displayName: String = prettyName.getOrElse(name)
}
