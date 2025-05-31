package io.chymyst.earley

import scala.annotation.tailrec

object Recognizer {
  private def formatCentered(providedWidth: Int)(s: String): String =
    val contentWidth = s.length
    val padding: Int = math.max(0, providedWidth - contentWidth + 1) / 2
    " " * padding + s + " " * (providedWidth - padding - contentWidth)

  final case class EarleyItem(rule: RuleWithRHS, dotPosition: Int, initialIndex: Int) {
    def symbolAtDot: AnySymbol = rule.symbolAtIndex(dotPosition)
    def advance: EarleyItem    = this.copy(dotPosition = this.dotPosition + 1)
    def isComplete: Boolean    = (rule.size == dotPosition)

    override def toString: String = s"${rule.baseSymbol}   → ${
        val symbolsPrinted: Seq[String] = (0 until rule.size).map(i => rule.symbolAtIndex(i).toString)
        (symbolsPrinted.take(dotPosition) ++ Seq("●") ++ symbolsPrinted.drop(dotPosition)).map(formatCentered(5)).mkString(" ") + s"   @$initialIndex"
      }"
  }

  // Repeat until previousA stops changing:
  // previousA and previousB are two given sets. We will be appending new sets to previousA and to previousB. However, some elements may be already present.
  // Call getNewToAppend to find new sets to append. If there are actually any new sets to append, then we append and repeat the procedure.
  // We are done if there is nothing new to append to previousA (and so we will assume that there is also nothing new to append to previousB).
  @tailrec
  def transitiveClosure[A, B](previousA: Set[A], previousB: Set[B], toAppendToA: Set[A], getNewToAppend: Set[A] => (Set[A], Set[B])): (Set[A], Set[B]) = {
    val difference = toAppendToA -- previousA
    if difference.isEmpty then (previousA, previousB)
    else {
      val (newSetADelta, newSetBDelta) = getNewToAppend(difference)
      transitiveClosure(previousA ++ difference, previousB ++ newSetBDelta, newSetADelta, getNewToAppend)
    }
  }

  /*
  TO DO:

  - Make the recognizer step more granular. Reuse parts that perform scanning.

  - Make the initial chart correct without hacks. Use the granular parts from the previous refactoring.

  - Use opaque types to distinguish predicted and completed item sets.
   */

  // Return: (completed, predicted).
  def earleyRecognizerStep(
    grammar: SimpleGrammar,
    input: Array[Char],
    currentIndex: Int,
    previousChart: Vector[Set[EarleyItem]],
    needToRunPredictor: Boolean,
  ): (Set[EarleyItem], Set[EarleyItem]) = {
    val currentChar                                  = input(currentIndex)
    // Find items from the previous chart that match the current character.
    // Advance all those items and copy them to the new chart.
    val itemsThatRecognizedThisChar: Set[EarleyItem] =
      previousChart.lastOption.map(_.filter(_.symbolAtDot `matchesChar` currentChar).map(_.advance)).toSet.flatten

    // ## Completer

    // Find those items that have been completed as a result of being advanced past a terminal symbol, due to matching a single character.
    val (completedItems, activeItems) = itemsThatRecognizedThisChar.partition(_.isComplete)

    // For each of the completed items, find the parent items, advance those items, and add them to the current sets.
    // Find those that have been completed and add to the "completed" set. Non-completed items need to be added to the "active" set.
    // Repeat this until the "completed" set becomes stable ("transitive closure").
    val getNewCompletedAndActiveItems: Set[EarleyItem] => (Set[EarleyItem], Set[EarleyItem]) =
      _.flatMap(item => previousChart(item.initialIndex).filter(_.symbolAtDot `matchesSymbol` item.rule.baseSymbol).map(_.advance)).partition(_.isComplete)

    val (newCompletedItems, newActiveItems) = transitiveClosure(Set(), activeItems, completedItems, getNewCompletedAndActiveItems)

    // ## Predictor

    // We have consumed the character at the current index. Now we will produce items for the next character's index.

    // The second set is always going to be empty here.
    val newPredictedItems =
      if needToRunPredictor then transitiveClosure(Set(), Set(), newActiveItems, getNewPredictedItems(grammar, currentIndex))._1 else newActiveItems

    (newCompletedItems, newPredictedItems)
  }

  // Find all non-terminal symbols at dot; find rules for them; insert new Earley items with those rules, at the next index.
  private def getNewPredictedItems(grammar: SimpleGrammar, currentIndex: Int): Set[EarleyItem] => (Set[EarleyItem], Set[EarleyItem]) = { predicted =>
    val newPredicted = predicted
      .map(_.symbolAtDot).collect { case symbol: NonTerminalSymbol => grammar.rulesForSymbol(symbol) }.flatten.map(rhs => EarleyItem(rhs, 0, currentIndex + 1))
    (newPredicted, Set())
  }

  // The result is a tuple (completed, predicted).
  def earleyRecognizer(grammar: SimpleGrammar, input: Array[Char], runLastPredictor: Boolean): (Vector[Set[EarleyItem]], Vector[Set[EarleyItem]]) = {
    val fakeInitialItems              = Set(EarleyItem(RuleWithRHSAsVector(grammar.startSymbol, Vector(grammar.startSymbol)), 0, -1))
    val initialChart: Set[EarleyItem] = transitiveClosure(Set(), Set(), fakeInitialItems, getNewPredictedItems(grammar, -1))._1.filter(_.initialIndex >= 0)
    input.indices.foldLeft((Vector(Set()), Vector(initialChart))) { case ((prevCompleted, prevPredicted), i) =>
      val needToRunPredictor =
        runLastPredictor || i + 1 < input.length // We may not need to run the predictor if we are at the end of input. Otherwise, we always do.
      val (newCompleted, newPredicted) = earleyRecognizerStep(grammar, input, currentIndex = i, previousChart = prevPredicted, needToRunPredictor)
      (prevCompleted.appended(newCompleted), prevPredicted.appended(newPredicted))
    }
  }

}
