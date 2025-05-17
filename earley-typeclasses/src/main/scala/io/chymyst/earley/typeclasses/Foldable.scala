package io.chymyst.earley.typeclasses

trait Foldable[F[_]]:
  def reduce[T: Monoid, A](fa: F[A])(f: A => T): T

object Foldable {
  def apply[F[_]](using ev: Foldable[F]): Foldable[F] = ev
}
