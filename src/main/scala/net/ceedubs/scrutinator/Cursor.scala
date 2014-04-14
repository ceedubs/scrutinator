package net.ceedubs.scrutinator

sealed trait Cursor {
  def fold[A](c: FieldC => A, i: IndexC => A): A = this match {
    case x: FieldC => c(x)
    case x: IndexC => i(x)
  }
}

final case class FieldC(name: String, prettyName: Option[String]) extends Cursor {
  def displayName: String = prettyName.getOrElse(name)
}

/** Zero-based index of a sequence/array */
final case class IndexC(i: Int) extends Cursor

object CursorHistory {
  def fieldName(c: CursorHistory): Option[String] = c.reverse match {
    case (h :: t) =>
      val first = h.fold(_.name, i => s"[${i.i}]")
      val rest = t.map(_.fold(
        f => s".${f.name}",
        i => s"[${i.i}]"))
      Some((first :: rest).mkString)
    case _ => None
  }
}
