package io.chymyst.earley.unit

import io.chymyst.earley.typeclasses.Monoid.MonoidSyntax
import io.chymyst.earley.typeclasses.{Foldable, Monoid}
import munit.FunSuite

class GraphTest extends FunSuite {
  // Define a language with LitStr, And, Or.
  enum F[A]:
    case LitStr(str: String)
    case And(l: A, r: A)
    case Or(l: A, r: A)

  given Foldable[F] with
    def reduce[T: Monoid, A](fa: F[A])(f: A => T): T = fa match {
      case F.LitStr(str) => Monoid[T].empty
      case F.And(l, r)   => f(l) ++ f(r)
      case F.Or(l, r)    => f(l) ++ f(r)
    }
}
