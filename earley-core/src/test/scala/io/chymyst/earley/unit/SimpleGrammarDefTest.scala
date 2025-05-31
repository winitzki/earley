package io.chymyst.earley.unit

import io.chymyst.earley.{NonTerminalSymbol, Rule, SimpleGrammarDef, TerminalSymbol}
import munit.FunSuite
import io.chymyst.earley.SymGraph.rule
import com.eed3si9n.expecty.Expecty.expect

object FixturesForSimpleGrammar:

  trait FixtureForSimpleGrammar:
    def example_input: String
    def start_symbol: Rule
    final def char_input: Array[Char] = example_input.toCharArray

  lazy val grammar_figure_7_4 = new FixtureForSimpleGrammar {

    import io.chymyst.earley.SimpleGrammarDef.*

    lazy val S: Rule = rule('a' ~ S ~ 'b' || S ~ 'a' ~ 'b' || 'a' ~ 'a' ~ 'a')

    override def example_input: String = "aaaab"

    override def start_symbol: Rule = S
  }

  lazy val grammar_figure_7_8 = new FixtureForSimpleGrammar {
    import io.chymyst.earley.SimpleGrammarDef.*

    lazy val S: Rule = rule(E)

    lazy val E: Rule = rule(E ~ Q ~ F || F)

    lazy val Q: Rule = rule('+' || '-')

    lazy val F: Rule = rule('a')

    override def example_input: String = "a-a+a"

    override def start_symbol: Rule = S

  }

  lazy val grammar_figure_7_17 = new FixtureForSimpleGrammar {

    import io.chymyst.earley.SimpleGrammarDef.*

    override def example_input: String = "aa/a"

    override def start_symbol: Rule = S

    lazy val S: Rule = rule(E)

    lazy val E: Rule = rule(E ~ Q ~ F || F)

    lazy val Q: Rule = rule('*' || '/' || Empty)

    lazy val F: Rule = rule('a')
  }

class SimpleGrammarDefTest extends FunSuite {

  test("convert the grammar from Grune-Jacobs Figure 7.4") {
    val start_symbol = FixturesForSimpleGrammar.grammar_figure_7_4.start_symbol
    expect(start_symbol.print == "S ::== 'a' S 'b' | S 'a' 'b' | 'a' 'a' 'a'")
    val grammar      = SimpleGrammarDef.toSimpleGrammar(start_symbol)
    expect(grammar.toString == """S ::== 'a' 'a' 'a'
                                 |S ::== 'a' S 'b'
                                 |S ::== S 'a' 'b'
                                 |""".stripMargin)
  }

  test("convert the grammar from Grune-Jacobs Figure 7.8") {
    val start_symbol = FixturesForSimpleGrammar.grammar_figure_7_8.start_symbol
    expect(start_symbol.print == "S ::== E")
    val grammar      = SimpleGrammarDef.toSimpleGrammar(start_symbol)
    expect(grammar.toString == """S ::== E
                                 |E ::== E Q F
                                 |E ::== F
                                 |F ::== 'a'
                                 |Q ::== '+'
                                 |Q ::== '-'
                                 |""".stripMargin)
  }

  test("convert the grammar from Grune-Jacobs Figure 7.17") {
    val start_symbol = FixturesForSimpleGrammar.grammar_figure_7_17.start_symbol
    expect(start_symbol.print == "S ::== E")
    val grammar = SimpleGrammarDef.toSimpleGrammar(start_symbol)
    expect(grammar.startSymbol == NonTerminalSymbol("S"))
    expect(grammar.nonTerminalSymbols == Set(NonTerminalSymbol("S"), NonTerminalSymbol("E"), NonTerminalSymbol("Q"), NonTerminalSymbol("F")))
    expect(grammar.terminalSymbols == Set(TerminalSymbol("'*'",'*'),TerminalSymbol("'/'",'/'),TerminalSymbol("'a'",'a')))
    println(grammar.toString)
    println("---")
    expect(grammar.toString ==
      """S ::== E
        |E ::== E Q F
        |E ::== F
        |F ::== 'a'
        |Q ::== '*'
        |Q ::== '/'
        |Q ::== ε
        |""".stripMargin)
  }
}
