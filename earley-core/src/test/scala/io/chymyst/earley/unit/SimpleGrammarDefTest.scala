package io.chymyst.earley.unit

import io.chymyst.earley.{Rule, SimpleGrammarDef}
import munit.FunSuite
import io.chymyst.earley.SymGraph.rule
import com.eed3si9n.expecty.Expecty.expect

object FixturesForSimpleGrammar:
  lazy val grammar_figure_7_4 = {
    import io.chymyst.earley.SimpleGrammarDef.*

    lazy val S: Rule = rule('a' ~ S ~ 'b' || S ~ 'a' ~ 'b' || 'a' ~ 'a' ~ 'a')

    val example_input = "aaaab".toCharArray

    (S, example_input)
  }

  lazy val grammar_figure_7_8 = {
    import io.chymyst.earley.SimpleGrammarDef.*

    lazy val S: Rule = rule(E)

    lazy val E: Rule = rule(E ~ Q ~ F || F)

    lazy val Q: Rule = rule('+' || '-')

    lazy val F: Rule = rule('a')

    val example_input = "a-a+a".toCharArray

    (S, example_input)
  }

class SimpleGrammarDefTest extends FunSuite {

  test("convert the grammar from Grune-Jacobs Figure 7.4") {
    val (start_symbol, input) = FixturesForSimpleGrammar.grammar_figure_7_4
    expect(start_symbol.print == "S ::== 'a' S 'b' | S 'a' 'b' | 'a' 'a' 'a'")
    val grammar               = SimpleGrammarDef.toSimpleGrammar(start_symbol)
    expect(grammar.toString == """S ::== 'a' 'a' 'a'
                                 |S ::== 'a' S 'b'
                                 |S ::== S 'a' 'b'""".stripMargin)
  }

  test("convert the grammar from Grune-Jacobs Figure 7.8") {
    val (start_symbol, input) = FixturesForSimpleGrammar.grammar_figure_7_8
    expect(start_symbol.print == "S ::== E")
    val grammar               = SimpleGrammarDef.toSimpleGrammar(start_symbol)
    expect(grammar.toString == """S ::== E
                                 |E ::== E Q F
                                 |E ::== F
                                 |F ::== 'a'
                                 |Q ::== '+'
                                 |Q ::== '-'""".stripMargin)
  }
}
