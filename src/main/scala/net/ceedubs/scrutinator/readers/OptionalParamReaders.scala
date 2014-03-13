package net.ceedubs.scrutinator
package readers

trait OptionalParamReaders {

  implicit def optionalParamReader[I, A](implicit reader: ParamReader[ErrorsOrMaybe, I, A]): ParamReader[ErrorsOr, I, Option[A]] = {
    ParamReader.fromKleisli(reader.reader.mapK[ErrorsOr, Option[A]](identity))
  }

}
