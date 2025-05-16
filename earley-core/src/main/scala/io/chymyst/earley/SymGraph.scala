package io.chymyst.earley

import io.chymyst.earley.typeclasses.Monoid
import io.chymyst.earley.typeclasses.Monoid.MonoidSyntax
import sourcecode.Name

//* GraphNode is a Rule, a NodeLiteral, or a NodeOp. Other traits may be extended.
sealed trait GraphNode extends GraphFold

final case class Rule(name: String, node: () => GraphNode) extends GraphNode:
  override def toString: String = s"$name"

  def print: String = s"$name ::== ${node()}"

  def reduce[T: Monoid](f: GraphNode => T): T = SymGraph.trackVisited(this, f, Set(), Monoid[T].empty, descend = true)._1

  def reduceSkipThis[T: Monoid](f: GraphNode => T): T = SymGraph.trackVisited(node(), f, Set(), Monoid[T].empty, descend = true)._1

  def reduceOneLevel[T: Monoid](f: GraphNode => T): T = SymGraph.trackVisited(node(), f, Set(), Monoid[T].empty, descend = false)._1

  def rulesUsedRec: Set[Rule] = SymGraph.rulesUsed(this)

  def rulesUsedAtOneLevel: Set[Rule] = reduceOneLevel[Set[Rule]] {
    case rule: Rule => Set(rule)
    case _          => Set()
  }

  def literalsUsedRec: Set[NodeLiteral] = SymGraph.literalsUsed(this)

  def opsUsedRec: Set[NodeOp] = SymGraph.opsUsed(this)

trait NodeLiteral extends GraphNode:
  def reduce[T: Monoid](f: GraphNode => T): T = f(this)

trait NodeOp extends GraphNode

trait GraphFold:
  def reduce[T: Monoid](f: GraphNode => T): T

object SymGraph:
  implicit def rule(x: => GraphNode)(using valName: Name): Rule = Rule(name = valName.value, node = () => x)

  def rulesUsed(start: Rule): Set[Rule] = start.reduce[Set[Rule]] {
    case rule: Rule => Set(rule)
    case _          => Set()
  }

  def literalsUsed(start: Rule): Set[NodeLiteral] = start.reduce[Set[NodeLiteral]] {
    case literal: NodeLiteral => Set(literal)
    case _                    => Set()
  }

  def opsUsed(start: Rule): Set[NodeOp] = start.reduce[Set[NodeOp]] {
    case op: NodeOp => Set(op)
    case _          => Set()
  }

  private type S = Set[Rule] => Set[Rule]

  private given Monoid[S] = new Monoid[S] {
    override def empty: S = s => s

    override def combine(a: S, b: S): S = a andThen b
  }

  private type ST[T] = Set[Rule] => (T, Set[Rule])

  // Writer monad transformer (using T) applied to the State monad (S).
  private given [T: Monoid] => Monoid[ST[T]] = new Monoid[ST[T]]:
    override def empty: ST[T] = s => (Monoid[T].empty, s)

    override def combine(a: ST[T], b: ST[T]): ST[T] = { s =>
      val (newT1, newS1) = a.apply(s)
      val (newT2, newS2) = b.apply(newS1)
      (newT1 ++ newT2, newS2)
    }

  inline private val useName = true // Later try setting this to `false`.

  inline private def contains(visited: Set[Rule], rule: Rule): Boolean =
    inline if useName then visited.map(_.name) contains rule.name else visited contains rule

  private[earley] def trackVisited[T: Monoid](start: GraphNode, f: GraphNode => T, visited: Set[Rule], resultSoFar: T, descend: Boolean): (T, Set[Rule]) = {
    start match {
      case rule: Rule           =>
        if !descend
        then (resultSoFar ++ f(rule), visited)
        else if contains(visited, rule)
        then (resultSoFar, visited)
        else trackVisited(rule.node(), f, visited + rule, resultSoFar ++ f(rule), descend)
      case literal: NodeLiteral => (resultSoFar ++ literal.reduce(f), visited)
      case op: NodeOp           =>
        val resultFromOp: ST[T]     = op.reduce[ST[T]] { node =>
          val foldingOverOp: ST[T] = { previousVisited =>
            trackVisited(node, f, previousVisited, f(node), descend)
          }
          foldingOverOp
        }
        val (newResult, newVisited) = resultFromOp(visited)
        (resultSoFar ++ f(op) ++ newResult, newVisited)
    }
  }
