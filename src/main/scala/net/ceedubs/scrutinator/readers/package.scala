package net.ceedubs.scrutinator

package object readers {
  type FieldReader[M[+_], I, O] = ParamReader[M, (FieldC, I), O]
}
