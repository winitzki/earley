package io.chymyst.earley

import io.chymyst.earley.typeclasses.{Foldable, Monoid}
import io.chymyst.earley.typeclasses.Monoid.MonoidSyntax

sealed trait Graph[F[_], Id]

object Graph:
  final case class Rule[F[_], Id](id: Id, nodes: () => Graph[F, Id]) extends Graph[F, Id]:
    override def toString: String = s"$id"

    def print: String = s"$id ::== ${nodes()}"

  final case class Wrap[F[_], Id](unwrap: F[Graph[F, Id]]) extends Graph[F, Id]

  def reduce[F[_]: Foldable, Id, T: Monoid](f: Graph[F, Id] => T)(g: Graph[F, Id]): T =
    trackVisited[F, Id, T](g, f, Set(), Monoid[T].empty, descend = true)._1

  implicit def rule[F[_]: Foldable, Id](g: => Graph[F, Id])(using id: Id): Rule[F, Id] =
    Rule(id = id, nodes = () => g)

  private type S[F[_], Id] = Set[Rule[F, Id]] => Set[Rule[F, Id]]

  private given [S] => Monoid[S => S] = new Monoid[S => S] {
    override def empty: S => S = s => s

    override def combine(a: S => S, b: S => S): S => S = a andThen b
  }

  private type ST[A, T] = Set[A] => (T, Set[A])

  private type STF[F[_], Id, T] = ST[Rule[F, Id], T]

  // Writer monad transformer (using T) applied to the State monad (using Set[A] as state type).
  private given [T: Monoid, A] => Monoid[ST[A, T]] = new Monoid[ST[A, T]]:
    override def empty: ST[A, T] = s => (Monoid[T].empty, s)

    override def combine(a: ST[A, T], b: ST[A, T]): ST[A, T] = { s =>
      val (newT1, newS1) = a.apply(s)
      val (newT2, newS2) = b.apply(newS1)
      (newT1 ++ newT2, newS2)
    }

  inline private val useName = true // Later try setting this to `false`.

  inline private def contains[F[_]: Foldable, Id](visited: Set[Rule[F, Id]], rule: Rule[F, Id]): Boolean =
    inline if useName then visited.map(_.id) contains rule.id else visited contains rule

  private def trackVisited[F[_]: Foldable, Id, T: Monoid](
    start: Graph[F, Id],
    f: Graph[F, Id] => T,
    visited: Set[Rule[F, Id]],
    resultSoFar: T,
    descend: Boolean,
  ): (T, Set[Rule[F, Id]]) = {
    start match {
      case rule: Rule[F, Id]    =>
        if !descend
        then (resultSoFar ++ f(rule), visited)
        else if contains(visited, rule)
        then (resultSoFar, visited)
        else trackVisited(rule.nodes(), f, visited + rule, resultSoFar ++ f(rule), descend)
      case wrapped: Wrap[F, Id] =>
        val resultFromOp: STF[F, Id, T] = summon[Foldable[F]].reduce[STF[F, Id, T], Graph[F, Id]](wrapped.unwrap) { node =>
          val foldingOverOp: STF[F, Id, T] = { previousVisited =>
            trackVisited(node, f, previousVisited, f(node), descend)
          }
          foldingOverOp
        }
        val (newResult, newVisited)     = resultFromOp(visited)
        (resultSoFar ++ f(wrapped) ++ newResult, newVisited)
    }
  }
