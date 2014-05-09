package net.ceedubs.scrutinator

import scalaz._

sealed trait AllowedValues[A]

final class AnyValue[A] extends AllowedValues[A]

final class AllowedValueList[A] private[scrutinator](val allowed: NonEmptyList[A], val equal: Equal[A]) extends AllowedValues[A]

object AllowedValueList {
  def unapply[A](l: AllowedValueList[A]): Option[(NonEmptyList[A], Equal[A])] = Some((l.allowed, l.equal))
}

final class AllowedRange[A] private[scrutinator](val min: A, val max: A, val order: Order[A]) extends AllowedValues[A] {
  require(order.lessThanOrEqual(min, max), s"min ($min) must not be greater than max ($max)")
}

object AllowedRange {
  def unapply[A](r: AllowedRange[A]): Option[(A, A, Order[A])] = Some((r.min, r.max, r.order))
}

object AllowedValues {
  def any[A]: AllowedValues[A] = new AnyValue

  def anyOf[A](a: A, as: A*)(implicit equal: Equal[A]): AllowedValues[A] =
    new AllowedValueList(NonEmptyList(a, as: _*), equal)

  def range[A](min: A, max: A)(implicit order: Order[A]): AllowedValues[A] = new AllowedRange[A](min, max, order)
}
