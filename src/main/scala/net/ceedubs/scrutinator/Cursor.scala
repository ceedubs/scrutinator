package net.ceedubs.scrutinator

sealed trait Cursor

final case class FieldC(name: String, prettyName: Option[String]) extends Cursor {
  def displayName: String = prettyName.getOrElse(name)
}

/** Zero-based index of a sequence/array */
final case class IndexC(i: Int) extends Cursor
