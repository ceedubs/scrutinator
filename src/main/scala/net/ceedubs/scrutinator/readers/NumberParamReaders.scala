package net.ceedubs.scrutinator
package readers

import scalaz._

trait NumberParamReaders {
  implicit def intFieldReader[I](implicit reader: ParamReader[ValidatedOption, (FieldKey, I), String]): ParamReader[ValidatedOption, (FieldKey, I), Int] = {
    ParamReader.andThenCheck(reader) { (fieldKey, s) =>
      std.string.parseInt(s).leftMap(_ =>
        NonEmptyList(s"${fieldKey.displayName} must be a valid integer"))
    }
  }

  implicit def longFieldReader[I](implicit reader: ParamReader[ValidatedOption, (FieldKey, I), String]): ParamReader[ValidatedOption, (FieldKey, I), Long] = {
    ParamReader.andThenCheck(reader) { (fieldKey, s) =>
      std.string.parseLong(s).leftMap(_ =>
        NonEmptyList(s"${fieldKey.displayName} must be a valid long"))
    }
  }

  implicit def byteFieldReader[I](implicit reader: ParamReader[ValidatedOption, (FieldKey, I), String]): ParamReader[ValidatedOption, (FieldKey, I), Byte] = {
    ParamReader.andThenCheck(reader) { (fieldKey, s) =>
      std.string.parseByte(s).leftMap(_ =>
        NonEmptyList(s"${fieldKey.displayName} must be a valid byte"))
    }
  }

  implicit def doubleFieldReader[I](implicit reader: ParamReader[ValidatedOption, (FieldKey, I), String]): ParamReader[ValidatedOption, (FieldKey, I), Double] = {
    ParamReader.andThenCheck(reader) { (fieldKey, s) =>
      std.string.parseDouble(s).leftMap(_ =>
        NonEmptyList(s"${fieldKey.displayName} must be a valid double"))
    }
  }

  implicit def floatFieldReader[I](implicit reader: ParamReader[ValidatedOption, (FieldKey, I), String]): ParamReader[ValidatedOption, (FieldKey, I), Float] = {
    ParamReader.andThenCheck(reader) { (fieldKey, s) =>
      std.string.parseFloat(s).leftMap(_ =>
        NonEmptyList(s"${fieldKey.displayName} must be a valid float"))
    }
  }

  implicit def shortFieldReader[I](implicit reader: ParamReader[ValidatedOption, (FieldKey, I), String]): ParamReader[ValidatedOption, (FieldKey, I), Short] = {
    ParamReader.andThenCheck(reader) { (fieldKey, s) =>
      std.string.parseShort(s).leftMap(_ =>
        NonEmptyList(s"${fieldKey.displayName} must be a valid short"))
    }
  }
}
