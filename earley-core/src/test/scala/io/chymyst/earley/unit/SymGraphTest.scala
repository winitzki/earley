package io.chymyst.earley.unit

import com.eed3si9n.expecty.Expecty.expect
import io.chymyst.earley.SymGraph.rule
import io.chymyst.earley.typeclasses.Monoid
import io.chymyst.earley.typeclasses.Monoid.MonoidSyntax
import io.chymyst.earley.{GraphNode, TerminalNode, OpNode, Rule}
import munit.FunSuite

object Fixtures1:
  final case class And(l: GraphNode, r: GraphNode) extends OpNode:
    override def reduce[T: Monoid](f: GraphNode => T): T = f(l) ++ f(r)

    override def toString: String = s"$l $r"

  final case class Or(l: GraphNode, r: GraphNode) extends OpNode:
    override def reduce[T: Monoid](f: GraphNode => T): T = f(l) ++ f(r)

    override def toString: String = s"$l | $r"

  final case class LitStr(str: String) extends TerminalNode:
    override def toString: String = s"'$str'"

  extension (n: GraphNode)
    def &(other: GraphNode): And = And(n, other)
    def |(other: GraphNode): Or  = Or(n, other)

class SymGraphTest extends FunSuite:

  import Fixtures1.*

  test("define some rules without cyclic dependencies") {

    lazy val a: Rule = rule(LitStr("x") & b | c)

    lazy val b: Rule = rule(LitStr("y") | e)

    lazy val c: Rule = rule(LitStr("z"))

    lazy val d: Rule = rule(c)

    lazy val e: Rule = c

    val a_ops = a.opsUsedRec.map(_.toString)
    expect(a_ops == Set("'x' b", "'x' b | c", "'y' | c"))
    val b_ops = b.opsUsedRec.map(_.toString)
    expect(b_ops == Set("'y' | c"))
    val c_ops = c.opsUsedRec.map(_.toString)
    expect(c_ops == Set())

    expect(a.rulesUsedRec.map(_.name) == Set("a", "b", "c"))
    expect(b.rulesUsedRec.map(_.name) == Set("b", "c"))
    expect(c.rulesUsedRec.map(_.name) == Set("c"))
    expect(d.rulesUsedRec.map(_.name) == Set("c", "d"))
    expect(e.rulesUsedRec.map(_.name) == Set("c"))

    expect(a.terminalsUsedRec.map(_.toString) == Set("'x'", "'y'", "'z'"))
    expect(b.terminalsUsedRec.map(_.toString) == Set("'y'", "'z'"))
    expect(c.terminalsUsedRec.map(_.toString) == Set("'z'"))
    expect(d.terminalsUsedRec.map(_.toString) == Set("'z'"))

    expect(a.node().toString == "'x' b | c")
    expect(b.node().toString == "'y' | c")

  }

  test("define some rules with cyclic dependencies") {
    lazy val a: Rule = rule(LitStr("x") & b | c)
    lazy val b: Rule = rule(LitStr("y") | a | d)
    lazy val c: Rule = rule(LitStr("z") & d & c & a)
    lazy val d: Rule = rule(LitStr("u") & d | LitStr("v"))

    expect(a.rulesUsedRec.map(_.name) == Set("a", "b", "c", "d"))
    expect(b.rulesUsedRec.map(_.name) == Set("a", "b", "c", "d"))
    expect(c.rulesUsedRec.map(_.name) == Set("a", "b", "c", "d"))
    expect(d.rulesUsedRec.map(_.name) == Set("d"))

    val opsUsed = Set("'x' b | c", "'x' b", "'y' | a | d", "'y' | a", "'z' d", "'z' d c", "'z' d c a", "'u' d", "'u' d | 'v'")
    expect(a.opsUsedRec.map(_.toString) == opsUsed)
    expect(b.opsUsedRec.map(_.toString) == opsUsed)
    expect(c.opsUsedRec.map(_.toString) == opsUsed)
    expect(d.opsUsedRec.map(_.toString) == Set("'u' d", "'u' d | 'v'"))

    val litUsed = Set("'x'", "'y'", "'z'", "'u'", "'v'")
    expect(a.terminalsUsedRec.map(_.toString) == litUsed)
    expect(b.terminalsUsedRec.map(_.toString) == litUsed)
    expect(c.terminalsUsedRec.map(_.toString) == litUsed)
    expect(d.terminalsUsedRec.map(_.toString) == Set("'u'", "'v'"))

  }
