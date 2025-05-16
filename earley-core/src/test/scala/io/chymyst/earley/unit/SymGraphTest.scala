package io.chymyst.earley.unit

import com.eed3si9n.expecty.Expecty.expect
import io.chymyst.earley.SymGraph.rule
import io.chymyst.earley.typeclasses.Monoid
import io.chymyst.earley.typeclasses.Monoid.MonoidSyntax
import io.chymyst.earley.{GraphNode, NodeLiteral, NodeOp, Rule}
import munit.FunSuite

object Fixtures1:
  final case class And(l: GraphNode, r: GraphNode) extends NodeOp:
    override def reduce[T: Monoid](f: GraphNode => T): T = f(l) ++ f(r)

  final case class Or(l: GraphNode, r: GraphNode) extends NodeOp:
    override def reduce[T: Monoid](f: GraphNode => T): T = f(l) ++ f(r)

  final case class LitStr(str: String) extends NodeLiteral

  extension (n: GraphNode)
    def &(other: GraphNode): And = And(n, other)
    def |(other: GraphNode): Or = Or(n, other)

class SymGraphTest extends FunSuite:

  import Fixtures1.*

  test("define some rules without cyclic dependencies") {

    def a: Rule = LitStr("x") & b | c

    def b: Rule = LitStr("y") | e

    def c: Rule = LitStr("z")

    def d: Rule = rule(c)

    def e: Rule = c

    expect(a.rulesUsed.map(_.name) == Set("a", "b", "c"))
    expect(b.rulesUsed.map(_.name) == Set("b", "c"))
    expect(c.rulesUsed.map(_.name) == Set("c"))
    expect(d.rulesUsed.map(_.name) == Set("c", "d"))
    expect(e.rulesUsed.map(_.name) == Set("c"))

    expect(a.literalsUsed.map(_.toString) == Set("LitStr(x)", "LitStr(y)", "LitStr(z)"))
    expect(b.literalsUsed.map(_.toString) == Set("LitStr(y)", "LitStr(z)"))
    expect(c.literalsUsed.map(_.toString) == Set("LitStr(z)"))
    expect(d.literalsUsed.map(_.toString) == Set("LitStr(z)"))

    expect(a.node().toString == "Or(And(LitStr(x),Rule(b)),Rule(c))")
    expect(b.node().toString == "Or(LitStr(y),Rule(c))")

    val c_ops = c.opsUsed.map(_.toString)
    expect(c_ops == Set())
    val b_ops = b.opsUsed.map(_.toString)
    expect(b_ops == Set("Or(LitStr(y),Rule(c))"))
    val a_ops = a.opsUsed.map(_.toString)
    expect(a_ops == Set("And(LitStr(x),Rule(b))", "Or(And(LitStr(x),Rule(b)),Rule(c))"))

  }

  test("define some rules with cyclic dependencies") {
    def a: Rule = LitStr("x") & b | c

    def b: Rule = LitStr("y") & a | d

    def c: Rule = LitStr("z") & d | a

    def d: Rule = LitStr("w") & c | b
  }
