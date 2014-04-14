package net.ceedubs.scrutinator

import scalaz._
import scalatra.readers.PathReaders
import org.scalatra.validation.ValidationError

package object scalatra extends PathReaders {
  type ValidationErrorsOr[+A] = NonEmptyList[ValidationError] \/ A
}
