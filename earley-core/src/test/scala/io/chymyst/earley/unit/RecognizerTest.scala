package io.chymyst.earley.unit

import com.eed3si9n.expecty.Expecty.expect
import io.chymyst.earley.Recognizer.EarleyItem
import io.chymyst.earley.unit.FixturesForSimpleGrammar.*
import io.chymyst.earley.{Recognizer, Rule, SimpleGrammarDef}
import munit.FunSuite

def prettyprintChart(chart: (Vector[Set[EarleyItem]], Vector[Set[EarleyItem]])): String =
  (chart._1 `zip` chart._2).zipWithIndex
    .map { case ((completed, predicted), i) =>
      s"$i:\n" +
        completed.toVector.map(_.toString).sorted.mkString("\n") +
        "\n---\n" +
        predicted.toVector.map(_.toString).sorted.mkString("\n") +
        "\n"
    }.mkString("\n")

def testGrammar(fixture: FixtureForSimpleGrammar, prefixLength: Int): String =
  prettyprintChart(Recognizer.earleyRecognizer(SimpleGrammarDef.toSimpleGrammar(fixture.start_symbol), fixture.example_input.toCharArray.take(prefixLength)))

class RecognizerTest extends FunSuite {

  test("recognize the sample from Grune-Jacobs Figure 7.4") {

    expect(testGrammar(grammar_figure_7_4, 5) == """0:
                                                   |
                                                   |---
                                                   |S → ● 'a' 'a' 'a' @0
                                                   |S → ● 'a' S 'b' @0
                                                   |S → ● S 'a' 'b' @0
                                                   |
                                                   |1:
                                                   |
                                                   |---
                                                   |S → 'a' ● 'a' 'a' @0
                                                   |S → 'a' ● S 'b' @0
                                                   |S → ● 'a' 'a' 'a' @1
                                                   |S → ● 'a' S 'b' @1
                                                   |S → ● S 'a' 'b' @1
                                                   |
                                                   |2:
                                                   |
                                                   |---
                                                   |S → 'a' 'a' ● 'a' @0
                                                   |S → 'a' ● 'a' 'a' @1
                                                   |S → 'a' ● S 'b' @1
                                                   |S → ● 'a' 'a' 'a' @2
                                                   |S → ● 'a' S 'b' @2
                                                   |S → ● S 'a' 'b' @2
                                                   |
                                                   |3:
                                                   |S → 'a' 'a' 'a' ● @0
                                                   |---
                                                   |S → 'a' 'a' ● 'a' @1
                                                   |S → 'a' ● 'a' 'a' @2
                                                   |S → 'a' ● S 'b' @2
                                                   |S → S ● 'a' 'b' @0
                                                   |S → ● 'a' 'a' 'a' @3
                                                   |S → ● 'a' S 'b' @3
                                                   |S → ● S 'a' 'b' @3
                                                   |
                                                   |4:
                                                   |S → 'a' 'a' 'a' ● @1
                                                   |---
                                                   |S → 'a' 'a' ● 'a' @2
                                                   |S → 'a' S ● 'b' @0
                                                   |S → 'a' ● 'a' 'a' @3
                                                   |S → 'a' ● S 'b' @3
                                                   |S → S 'a' ● 'b' @0
                                                   |S → S ● 'a' 'b' @1
                                                   |S → ● 'a' 'a' 'a' @4
                                                   |S → ● 'a' S 'b' @4
                                                   |S → ● S 'a' 'b' @4
                                                   |
                                                   |5:
                                                   |S → 'a' S 'b' ● @0
                                                   |S → S 'a' 'b' ● @0
                                                   |---
                                                   |S → S ● 'a' 'b' @0
                                                   |""".stripMargin)
  }

  test("recognize the sample from Grune-Jacobs Figure 7.8") {
    expect(testGrammar(grammar_figure_7_8, 5) == """0:
                                                   |
                                                   |---
                                                   |S → ● E @0
                                                   |""".stripMargin)
  }

}
