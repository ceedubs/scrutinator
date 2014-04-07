package net.ceedubs.scrutinator
package readers

trait OptionalParamReaders {

  implicit def optionalParamReader[I, A](implicit reader: ParamReader[ValidatedOption, I, A]): ParamReader[Validated, I, Option[A]] = {
    ParamReader.fromKleisli(reader.reader.mapK[Validated, Option[A]](identity))
  }

}
