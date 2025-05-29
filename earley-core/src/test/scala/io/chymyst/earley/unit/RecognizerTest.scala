package io.chymyst.earley.unit

import com.eed3si9n.expecty.Expecty.expect
import io.chymyst.earley.Recognizer.EarleyItem
import io.chymyst.earley.unit.FixturesForSimpleGrammar.*
import io.chymyst.earley.{Recognizer, Rule, SimpleGrammarDef}
import munit.FunSuite

def prettyprintChart(chart: (Vector[Set[EarleyItem]], Vector[Set[EarleyItem]])): String =
  (chart._1 `zip` chart._2).zipWithIndex
    .map { case ((completed, predicted), i) =>
      s"@$i:\n" +
        completed.toVector.map(_.toString).sorted.mkString("\n") +
        "\n---\n" +
        predicted.toVector.map(_.toString).sorted.mkString("\n") +
        "\n"
    }.mkString("\n")

def testGrammar(fixture: FixtureForSimpleGrammar, prefixLength: Int): String =
  prettyprintChart(Recognizer.earleyRecognizer(SimpleGrammarDef.toSimpleGrammar(fixture.start_symbol), fixture.example_input.toCharArray.take(prefixLength)))

class RecognizerTest extends FunSuite {

  test("recognize the sample from Grune-Jacobs Figure 7.4") {

    expect(testGrammar(grammar_figure_7_4, 5) == """@0:
                                                   |
                                                   |---
                                                   |S   →   ●     S    'a'   'b'    @0
                                                   |S   →   ●    'a'    S    'b'    @0
                                                   |S   →   ●    'a'   'a'   'a'    @0
                                                   |
                                                   |@1:
                                                   |
                                                   |---
                                                   |S   →   ●     S    'a'   'b'    @1
                                                   |S   →   ●    'a'    S    'b'    @1
                                                   |S   →   ●    'a'   'a'   'a'    @1
                                                   |S   →  'a'    ●     S    'b'    @0
                                                   |S   →  'a'    ●    'a'   'a'    @0
                                                   |
                                                   |@2:
                                                   |
                                                   |---
                                                   |S   →   ●     S    'a'   'b'    @2
                                                   |S   →   ●    'a'    S    'b'    @2
                                                   |S   →   ●    'a'   'a'   'a'    @2
                                                   |S   →  'a'    ●     S    'b'    @1
                                                   |S   →  'a'    ●    'a'   'a'    @1
                                                   |S   →  'a'   'a'    ●    'a'    @0
                                                   |
                                                   |@3:
                                                   |S   →  'a'   'a'   'a'    ●     @0
                                                   |---
                                                   |S   →   S     ●    'a'   'b'    @0
                                                   |S   →   ●     S    'a'   'b'    @3
                                                   |S   →   ●    'a'    S    'b'    @3
                                                   |S   →   ●    'a'   'a'   'a'    @3
                                                   |S   →  'a'    ●     S    'b'    @2
                                                   |S   →  'a'    ●    'a'   'a'    @2
                                                   |S   →  'a'   'a'    ●    'a'    @1
                                                   |
                                                   |@4:
                                                   |S   →  'a'   'a'   'a'    ●     @1
                                                   |---
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
                                                   |S   →   S    'a'   'b'    ●     @0
                                                   |S   →  'a'    S    'b'    ●     @0
                                                   |---
                                                   |S   →   S     ●    'a'   'b'    @0
                                                   |""".stripMargin)
  }

  test("recognize the sample from Grune-Jacobs Figure 7.8") {
    expect(testGrammar(grammar_figure_7_8, 5) == """@0:
                                                   |
                                                   |---
                                                   |E   →   ●     E     Q     F     @0
                                                   |E   →   ●     F     @0
                                                   |F   →   ●    'a'    @0
                                                   |S   →   ●     E     @0
                                                   |
                                                   |@1:
                                                   |E   →   F     ●     @0
                                                   |F   →  'a'    ●     @0
                                                   |S   →   E     ●     @0
                                                   |---
                                                   |E   →   E     ●     Q     F     @0
                                                   |Q   →   ●    '+'    @1
                                                   |Q   →   ●    '-'    @1
                                                   |
                                                   |@2:
                                                   |Q   →  '-'    ●     @1
                                                   |---
                                                   |E   →   E     Q     ●     F     @0
                                                   |F   →   ●    'a'    @2
                                                   |
                                                   |@3:
                                                   |E   →   E     Q     F     ●     @0
                                                   |F   →  'a'    ●     @2
                                                   |S   →   E     ●     @0
                                                   |---
                                                   |E   →   E     ●     Q     F     @0
                                                   |Q   →   ●    '+'    @3
                                                   |Q   →   ●    '-'    @3
                                                   |
                                                   |@4:
                                                   |Q   →  '+'    ●     @3
                                                   |---
                                                   |E   →   E     Q     ●     F     @0
                                                   |F   →   ●    'a'    @4
                                                   |
                                                   |@5:
                                                   |E   →   E     Q     F     ●     @0
                                                   |F   →  'a'    ●     @4
                                                   |S   →   E     ●     @0
                                                   |---
                                                   |E   →   E     ●     Q     F     @0
                                                   |Q   →   ●    '+'    @5
                                                   |Q   →   ●    '-'    @5
                                                   |""".stripMargin)
  }

}
