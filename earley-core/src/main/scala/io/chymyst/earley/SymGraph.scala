package io.chymyst.earley

import io.chymyst.earley.typeclasses.Monoid
import io.chymyst.earley.typeclasses.Monoid.MonoidSyntax
import sourcecode.Name

// GraphNode is a Rule, a TerminalNode, or an OpNode. The traits `TerminalNode` and `OpNode` may be extended.
sealed trait GraphNode:
  def reduce[T: Monoid](f: GraphNode => T): T

final case class Rule(name: String, node: () => GraphNode) extends GraphNode:
  override def toString: String = s"$name"

  def print: String = s"$name ::== ${node()}"

  def reduce[T: Monoid](f: GraphNode => T): T = SymGraph.trackVisited(this, f, Set(), Monoid[T].empty)._1

  def reduceSkipThis[T: Monoid](f: GraphNode => T): T = SymGraph.trackVisited(node(), f, Set(), Monoid[T].empty)._1

  def rulesUsedRec: Set[Rule] = SymGraph.rulesUsed(this)

  def terminalsUsedRec: Set[TerminalNode] = SymGraph.literalsUsed(this)

  def opsUsedRec: Set[OpNode] = SymGraph.opsUsed(this)

trait TerminalNode extends GraphNode:
  def reduce[T: Monoid](f: GraphNode => T): T = f(this)

trait OpNode extends GraphNode

object SymGraph:
  def rule(x: => GraphNode)(using valName: Name): Rule = Rule(name = valName.value, node = () => x)

  def rulesUsed(start: Rule): Set[Rule] = start.reduce[Set[Rule]] {
    case rule: Rule => Set(rule)
    case _          => Set()
  }

  def literalsUsed(start: Rule): Set[TerminalNode] = start.reduce[Set[TerminalNode]] {
    case literal: TerminalNode => Set(literal)
    case _                     => Set()
  }

  def opsUsed(start: Rule): Set[OpNode] = start.reduce[Set[OpNode]] {
    case op: OpNode => Set(op)
    case _          => Set()
  }

  private type S = Set[Rule] => Set[Rule]

  private given Monoid[S] = new Monoid[S] {
    override def empty: S = s => s

    override def combine(a: S, b: S): S = a andThen b
  }

  private type ST[T] = Set[Rule] => (T, Set[Rule])

  // ST[T] is a monoid when T is one.
  private given [T: Monoid] => Monoid[ST[T]] = new Monoid[ST[T]]:
    override def empty: ST[T] = s => (Monoid[T].empty, s)

    override def combine(a: ST[T], b: ST[T]): ST[T] = { s =>
      val (newT1, newS1) = a.apply(s)
      val (newT2, newS2) = b.apply(newS1)
      (newT1 ++ newT2, newS2)
    }

  inline private val identifyRulesByNameOnly = true // Later try setting this to `false`.

  inline private def contains(visited: Set[Rule], rule: Rule): Boolean =
    inline if identifyRulesByNameOnly then visited.map(_.name) contains rule.name else visited contains rule

  private[earley] def trackVisited[T: Monoid](start: GraphNode, f: GraphNode => T, visited: Set[Rule], resultSoFar: T): (T, Set[Rule]) = {
    start match {
      case rule: Rule            =>
        if contains(visited, rule)
        then (resultSoFar, visited)
        else trackVisited(rule.node(), f, visited + rule, resultSoFar ++ f(rule))
      case literal: TerminalNode => (resultSoFar ++ literal.reduce(f), visited)
      case op: OpNode            =>
        val resultFromOp: ST[T]     = op.reduce[ST[T]] { node =>
          val foldingOverOp: ST[T] = { previousVisited =>
            trackVisited(node, f, previousVisited, f(node))
          }
          foldingOverOp
        }
        val (newResult, newVisited) = resultFromOp(visited)
        (resultSoFar ++ f(op) ++ newResult, newVisited)
    }
  }
