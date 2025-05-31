package io.chymyst.earley.unit

import com.eed3si9n.expecty.Expecty.expect
import io.chymyst.earley.Recognizer.EarleyItem
import io.chymyst.earley.unit.FixturesForSimpleGrammar.*
import io.chymyst.earley.{Recognizer, Rule, SimpleGrammarDef}
import munit.FunSuite

def prettyprintChart(chart: (Vector[Set[EarleyItem]], Vector[Set[EarleyItem]])): String =
  (chart._1 `zip` chart._2).zipWithIndex
    .map { case ((completed, predicted), i) =>
      s"@$i:\n--- completed:\n" +
        completed.toVector.map(_.toString).sorted.mkString("\n") +
        "\n--- predicted:\n" +
        predicted.toVector.map(_.toString).sorted.mkString("\n") +
        "\n"
    }.mkString("\n")

def testGrammar(fixture: FixtureForSimpleGrammar, prefixLength: Int, runLastPredictor: Boolean): String =
  prettyprintChart(
    Recognizer.earleyRecognizer(SimpleGrammarDef.toSimpleGrammar(fixture.start_symbol), fixture.example_input.toCharArray.take(prefixLength), runLastPredictor)
  )

class RecognizerTest extends FunSuite {

  test("recognize the sample from Grune-Jacobs Figure 7.4") {

    expect(testGrammar(grammar_figure_7_4, 5, false) == """@0:
                                                   |--- completed:
                                                   |
                                                   |--- predicted:
                                                   |S   →   ●     S    'a'   'b'    @0
                                                   |S   →   ●    'a'    S    'b'    @0
                                                   |S   →   ●    'a'   'a'   'a'    @0
                                                   |
                                                   |@1:
                                                   |--- completed:
                                                   |
                                                   |--- predicted:
                                                   |S   →   ●     S    'a'   'b'    @1
                                                   |S   →   ●    'a'    S    'b'    @1
                                                   |S   →   ●    'a'   'a'   'a'    @1
                                                   |S   →  'a'    ●     S    'b'    @0
                                                   |S   →  'a'    ●    'a'   'a'    @0
                                                   |
                                                   |@2:
                                                   |--- completed:
                                                   |
                                                   |--- predicted:
                                                   |S   →   ●     S    'a'   'b'    @2
                                                   |S   →   ●    'a'    S    'b'    @2
                                                   |S   →   ●    'a'   'a'   'a'    @2
                                                   |S   →  'a'    ●     S    'b'    @1
                                                   |S   →  'a'    ●    'a'   'a'    @1
                                                   |S   →  'a'   'a'    ●    'a'    @0
                                                   |
                                                   |@3:
                                                   |--- completed:
                                                   |S   →  'a'   'a'   'a'    ●     @0
                                                   |--- predicted:
                                                   |S   →   S     ●    'a'   'b'    @0
                                                   |S   →   ●     S    'a'   'b'    @3
                                                   |S   →   ●    'a'    S    'b'    @3
                                                   |S   →   ●    'a'   'a'   'a'    @3
                                                   |S   →  'a'    ●     S    'b'    @2
                                                   |S   →  'a'    ●    'a'   'a'    @2
                                                   |S   →  'a'   'a'    ●    'a'    @1
                                                   |
                                                   |@4:
                                                   |--- completed:
                                                   |S   →  'a'   'a'   'a'    ●     @1
                                                   |--- predicted:
                                                   |S   →   S     ●    'a'   'b'    @1
                                                   |S   →   S    'a'    ●    'b'    @0
                                                   |S   →   ●     S    'a'   'b'    @4
                                                   |S   →   ●    'a'    S    'b'    @4
                                                   |S   →   ●    'a'   'a'   'a'    @4
                                                   |S   →  'a'    S     ●    'b'    @0
                                                   |S   →  'a'    ●     S    'b'    @3
                                                   |S   →  'a'    ●    'a'   'a'    @3
                                                   |S   →  'a'   'a'    ●    'a'    @2
                                                   |
                                                   |@5:
                                                   |--- completed:
                                                   |S   →   S    'a'   'b'    ●     @0
                                                   |S   →  'a'    S    'b'    ●     @0
                                                   |--- predicted:
                                                   |S   →   S     ●    'a'   'b'    @0
                                                   |""".stripMargin)
  }

  test("reproduce the chart from Grune-Jacobs Figure 7.11") {
    expect(testGrammar(grammar_figure_7_8, 5, false) == """@0:
                                                   |--- completed:
                                                   |
                                                   |--- predicted:
                                                   |E   →   ●     E     Q     F     @0
                                                   |E   →   ●     F     @0
                                                   |F   →   ●    'a'    @0
                                                   |S   →   ●     E     @0
                                                   |
                                                   |@1:
                                                   |--- completed:
                                                   |E   →   F     ●     @0
                                                   |F   →  'a'    ●     @0
                                                   |S   →   E     ●     @0
                                                   |--- predicted:
                                                   |E   →   E     ●     Q     F     @0
                                                   |Q   →   ●    '+'    @1
                                                   |Q   →   ●    '-'    @1
                                                   |
                                                   |@2:
                                                   |--- completed:
                                                   |Q   →  '-'    ●     @1
                                                   |--- predicted:
                                                   |E   →   E     Q     ●     F     @0
                                                   |F   →   ●    'a'    @2
                                                   |
                                                   |@3:
                                                   |--- completed:
                                                   |E   →   E     Q     F     ●     @0
                                                   |F   →  'a'    ●     @2
                                                   |S   →   E     ●     @0
                                                   |--- predicted:
                                                   |E   →   E     ●     Q     F     @0
                                                   |Q   →   ●    '+'    @3
                                                   |Q   →   ●    '-'    @3
                                                   |
                                                   |@4:
                                                   |--- completed:
                                                   |Q   →  '+'    ●     @3
                                                   |--- predicted:
                                                   |E   →   E     Q     ●     F     @0
                                                   |F   →   ●    'a'    @4
                                                   |
                                                   |@5:
                                                   |--- completed:
                                                   |E   →   E     Q     F     ●     @0
                                                   |F   →  'a'    ●     @4
                                                   |S   →   E     ●     @0
                                                   |--- predicted:
                                                   |E   →   E     ●     Q     F     @0
                                                   |""".stripMargin)
  }

  test("reproduce parse chart for grammar in Figure 7.17") {
    expect(testGrammar(grammar_figure_7_17, 4, true) == """""")
  }
}
