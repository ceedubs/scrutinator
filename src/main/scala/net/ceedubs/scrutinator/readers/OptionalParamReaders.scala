package net.ceedubs.scrutinator
package readers

trait OptionalParamReaders {

  implicit def optionalParamReader[I, A](implicit reader: ParamReader[ValidatedOption, I, A]): ParamReader[Validated, I, Option[A]] =
    reader.mapK[Validated, Option[A]](identity)

}
