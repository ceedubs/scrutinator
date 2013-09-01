package org.scalatra.contrib.shapeless

import scalaz.ValidationNel
import org.scalatra.validation.ValidationError

package object binding {
  type ErrorsOr[A] = ValidationNel[ValidationError, A]
}
