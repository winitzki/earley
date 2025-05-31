package io.chymyst.earley

import io.chymyst.earley.typeclasses.Monoid
import io.chymyst.earley.typeclasses.Monoid.MonoidSyntax
import sourcecode.Name

import scala.collection.immutable.HashMap
import scala.language.implicitConversions
import scala.reflect.ClassTag

// Interface to a grammar object.
// This is a low-level representation used by the parser.
// A user-facing grammar API will be converted to this representation.

trait SimpleGrammar:
  def startSymbol: NonTerminalSymbol
  def terminalSymbols: Set[TerminalSymbol]
  def nonTerminalSymbols: Set[NonTerminalSymbol]
  def rulesForSymbol: NonTerminalSymbol => Set[RuleWithRHS]

  override def toString: String =
    (Vector(startSymbol) ++ (nonTerminalSymbols - startSymbol).toVector.sortBy(_.name))
      .flatMap(symbol => rulesForSymbol(symbol).toVector.sortBy(_.toString)).mkString("\n")

// Interface to a right-hand-side of a grammar production rule.

trait RuleWithRHS:
  def size: Int
  def baseSymbol: NonTerminalSymbol
  def symbolAtIndex(index: Int): AnySymbol

final case class RuleWithRHSAsVector(baseSymbol: NonTerminalSymbol, rhs: Vector[AnySymbol]) extends RuleWithRHS:
  def size: Int                            = rhs.length
  def symbolAtIndex(index: Int): AnySymbol = rhs(index) // rhs.lift(index).getOrElse(StartOfText) TODO remove this

  override def toString: String = s"$baseSymbol ::== ${rhs.mkString(" ")}"

  // override def toString: String = s"${baseRule.name}" // Is this necessary?

trait AnySymbol:
  def name: String
  def isTerminal: Boolean                          = false
  def matchesChar(c: Char): Boolean                = false
  def matchesSymbol(s: NonTerminalSymbol): Boolean = false
  override def toString: String                    = name

//final case object StartOfText extends AnySymbol:  TODO remove this
//  val name =  "|-"

final case class TerminalSymbol(name: String, character: Char) extends AnySymbol:
  override def isTerminal: Boolean           = true
  override def matchesChar(c: Char): Boolean = character == c

final case class NonTerminalSymbol(name: String) extends AnySymbol:
  override def matchesSymbol(s: NonTerminalSymbol): Boolean = this.name == s.name

// SimpleGrammarDef supports only the simplest normal form for the Earley parser.
// Each grammar production rule must have the form `A ::== B C D ... E`. Here `A` is a non-terminal and
// each of B, C, D, ..., E must be either a single literal character or another non-terminal.

object SimpleGrammarDef:

  final case class And(symbols: Vector[GraphNode]) extends OpNode:
    override def reduce[T: Monoid](f: GraphNode => T): T = symbols.map(f).reduce(_ ++ _)

    override def toString: String = symbols.mkString(" ")

  final case class Or(symbols: Vector[GraphNode]) extends OpNode:
    override def reduce[T: Monoid](f: GraphNode => T): T = symbols.map(f).reduce(_ ++ _)

    override def toString: String = symbols.mkString(" | ")

  final case class LitChar(c: Char) extends TerminalNode:
    override def toString: String = s"'$c'"

  implicit def lit(c: Char): TerminalNode = LitChar(c)

  extension (rule: Rule) def toNonTerminalSymbol: NonTerminalSymbol = NonTerminalSymbol(rule.name)

  private val terminalNodeToTerminalSymbol: PartialFunction[TerminalNode, TerminalSymbol] = { case l @ LitChar(c) => TerminalSymbol(l.toString, c) }

  private def flattenOpNode(node: GraphNode): Set[Vector[AnySymbol]] = node match {
    case rule: Rule         => Set(Vector(rule.toNonTerminalSymbol))
    case node: TerminalNode => Set(Vector(node).collect(terminalNodeToTerminalSymbol))
    case And(symbols)       =>
      symbols.map(flattenOpNode).reduce { case (a, b) =>
        for {
          x <- a
          y <- b
        } yield x ++ y
      }
    case Or(symbols)        => symbols.map(flattenOpNode).reduce(_ ++ _)
  }

  private def flattenRHS(r: Rule): Set[RuleWithRHS] =
    val baseSymbol                          = r.toNonTerminalSymbol
    val rhsExpanded: Set[Vector[AnySymbol]] = flattenOpNode(r.node())
    rhsExpanded.map(rhs => RuleWithRHSAsVector(baseSymbol, rhs))

  def toSimpleGrammar(rule: Rule): SimpleGrammar = new SimpleGrammar {
    final val startSymbol: NonTerminalSymbol = NonTerminalSymbol(rule.name)

    final val rulesForSymbol: NonTerminalSymbol => Set[RuleWithRHS] =
      type Result = HashMap[NonTerminalSymbol, Set[RuleWithRHS]]
      given Monoid[Result] = new Monoid[Result]:
        override def empty: Result = HashMap()

        override def combine(a: Result, b: Result): Result = a.merged(b) { case ((k1, v1), (k2, v2)) => (k1, v1 ++ v2) }

      val allNonTerminalsWithRHS = rule.reduce[Result] {
        case r: Rule => HashMap(r.toNonTerminalSymbol -> flattenRHS(r))
        case _       => HashMap()
      }

      allNonTerminalsWithRHS.apply

    final val terminalSymbols: Set[TerminalSymbol] = rule.terminalsUsedRec.collect(terminalNodeToTerminalSymbol)

    final val nonTerminalSymbols: Set[NonTerminalSymbol] = rule.rulesUsedRec.map(_.toNonTerminalSymbol)
  }

  extension (s: GraphNode)
    def ~(x: GraphNode): GraphNode  = (s, x) match {
      case (And(a), And(b)) => And(a ++ b)
      case (And(a), b)      => And(a :+ b)
      case (a, And(b))      => And(a +: b)
      case (a, b)           => And(Vector(a, b))
    }
    def ||(x: GraphNode): GraphNode = (s, x) match {
      case (Or(a), Or(b)) => Or(a ++ b)
      case (Or(a), b)     => Or(a :+ b)
      case (a, Or(b))     => Or(a +: b)
      case (a, b)         => Or(Vector(a, b))
    }
