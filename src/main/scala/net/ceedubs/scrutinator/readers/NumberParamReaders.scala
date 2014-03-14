package net.ceedubs.scrutinator
package readers

import scalaz.{ @@ => _, _}

trait NumberParamReaders {
  implicit def intFieldReader[I](implicit reader: ParamReader[ErrorsOrMaybe, (FieldKey, I), String]): ParamReader[ErrorsOrMaybe, (FieldKey, I), Int] = {
    ParamReader.andThenCheck(reader) { s =>
      std.string.parseInt(s).leftMap(_ =>
        (displayName: String) => NonEmptyList(s"$displayName must be a valid integer"))
    }
  }

  implicit def longFieldReader[I](implicit reader: ParamReader[ErrorsOrMaybe, (FieldKey, I), String]): ParamReader[ErrorsOrMaybe, (FieldKey, I), Long] = {
    ParamReader.andThenCheck(reader) { s =>
      std.string.parseLong(s).leftMap(_ =>
        (displayName: String) => NonEmptyList(s"$displayName must be a valid long"))
    }
  }

  implicit def byteFieldReader[I](implicit reader: ParamReader[ErrorsOrMaybe, (FieldKey, I), String]): ParamReader[ErrorsOrMaybe, (FieldKey, I), Byte] = {
    ParamReader.andThenCheck(reader) { s =>
      std.string.parseByte(s).leftMap(_ =>
        (displayName: String) => NonEmptyList(s"$displayName must be a valid byte"))
    }
  }

  implicit def doubleFieldReader[I](implicit reader: ParamReader[ErrorsOrMaybe, (FieldKey, I), String]): ParamReader[ErrorsOrMaybe, (FieldKey, I), Double] = {
    ParamReader.andThenCheck(reader) { s =>
      std.string.parseDouble(s).leftMap(_ =>
        (displayName: String) => NonEmptyList(s"$displayName must be a valid double"))
    }
  }

  implicit def floatFieldReader[I](implicit reader: ParamReader[ErrorsOrMaybe, (FieldKey, I), String]): ParamReader[ErrorsOrMaybe, (FieldKey, I), Float] = {
    ParamReader.andThenCheck(reader) { s =>
      std.string.parseFloat(s).leftMap(_ =>
        (displayName: String) => NonEmptyList(s"$displayName must be a valid float"))
    }
  }

  implicit def shortFieldReader[I](implicit reader: ParamReader[ErrorsOrMaybe, (FieldKey, I), String]): ParamReader[ErrorsOrMaybe, (FieldKey, I), Short] = {
    ParamReader.andThenCheck(reader) { s =>
      std.string.parseShort(s).leftMap(_ =>
        (displayName: String) => NonEmptyList(s"$displayName must be a valid short"))
    }
  }
}
