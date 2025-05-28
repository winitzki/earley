package io.chymyst.earley

import sourcecode.Name

// User-facing API to create grammars.

// Grammars can be created:
// 1) by a direct definition in code,
// 2) by parsing a grammar definition given as a string in some BNF-like syntax.

// A grammar must have a single top-level start symbol.
// The goal of parsing is to obtaining a parse tree starting with that symbol.

sealed trait GrammarNode

object GrammarNode {
  final case class NonTerminal(name: String, node: () => GrammarNode) extends GrammarNode {
    override def toString: String = s"$name"

    def print: String = s"$name ::== ${node()}"
  }

  case class Terminal(matches: Char => Boolean) extends GrammarNode

  case class And(nodes: GrammarNode*) extends GrammarNode

  case class Or(nodes: GrammarNode*) extends GrammarNode

  case class Many(node: GrammarNode) extends GrammarNode

  implicit def rule(x: => GrammarNode)(using valName: Name): NonTerminal = NonTerminal(name = valName.value, node = () => x)
}
