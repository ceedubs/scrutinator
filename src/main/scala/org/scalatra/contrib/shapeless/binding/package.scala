package org.scalatra.contrib.shapeless

import scalaz.ValidationNel

package object binding {
  type ErrorMessagesOr[A] = ValidationNel[String, A]
}
