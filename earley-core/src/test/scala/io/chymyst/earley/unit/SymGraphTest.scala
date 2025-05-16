package io.chymyst.earley.unit

import com.eed3si9n.expecty.Expecty.expect
import io.chymyst.earley.typeclasses.{Applicative, Monoid}
import io.chymyst.earley.typeclasses.Applicative.ApplicativeOps
import io.chymyst.earley.typeclasses.Monoid.MonoidSyntax
import io.chymyst.earley.{GraphFold, GraphNode, NodeLiteral, NodeOp, Rule}
import munit.FunSuite
import io.chymyst.earley.SymGraph.rule

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

    def b: Rule = a & LitStr("y") | d

    def c: Rule = LitStr("z")

    def d: Rule = c

    val a_used = a.rulesUsed.map(_.name)

    expect(a_used == Set("a", "b", "c"))

  }

  test("define some rules with cyclic dependencies") {
    def a: Rule = LitStr("x") & b | c

    def b: Rule = LitStr("y") & a | d

    def c: Rule = LitStr("z") & d | a

    def d: Rule = LitStr("w") & c | b
  }
