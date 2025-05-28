package io.chymyst.earley

import scala.annotation.tailrec

object Recognizer {
  final case class EarleyItem(rule: RuleWithRHS, dotPosition: Int, initialIndex: Int) {
    def symbolAtDot: AnySymbol = rule.symbolAtIndex(dotPosition)
    def advance: EarleyItem    = this.copy(dotPosition = this.dotPosition + 1)
    def isComplete: Boolean    = (rule.size == dotPosition)
  }

  // Repeat until previousA stops changing:
  // previousA and previousB are two given sets. We will be appending new sets to previousA and to previousB. However, some elements may be already present.
  // Call getNewToAppend to find new sets to append. If there are actually any new sets to append, then we append and repeat the procedure.
  // We are done if there is nothing new to append to previousA (and so we will assume that there is also nothing new to append to previousB).
  @tailrec
  def transitiveClosure[A, B](previousA: Set[A], previousB: Set[B], toAppendToA: Set[A], getNewToAppend: Set[A] => (Set[A], Set[B])): (Set[A], Set[B]) = {
    if (toAppendToA -- previousA).isEmpty then (previousA, previousB)
    else {
      val (newSetADelta, newSetBDelta) = getNewToAppend(toAppendToA)
      transitiveClosure(previousA ++ toAppendToA, previousB ++ newSetBDelta, newSetADelta, getNewToAppend)
    }
  }

  def earleyRecognizerStep(grammar: SimpleGrammar, currentChar: Char, currentIndex: Int, previousChart: Array[Set[EarleyItem]]): Set[EarleyItem] = {
    // Find items from the previous chart that match the current character.
    // Advance all those items and copy them to the new chart.
    val itemsThatRecognizedThisChar: Set[EarleyItem] = previousChart.last.filter(_.symbolAtDot `matchesChar` currentChar).map(_.advance)

    // ## Completer

    // Find those items that have been completed as a result of being advanced.
    val (completedItems, activeItems)                                                        = itemsThatRecognizedThisChar.partition(_.isComplete)
    // For each of the completed items, find the parent items, advance those items, and add them to the current sets.
    // Find those that have been completed and add to the "completed" set. Non-completed items need to be added to the "active" set.
    // Repeat this until the "completed" set becomes stable ("transitive closure").
    val getNewCompletedAndActiveItems: Set[EarleyItem] => (Set[EarleyItem], Set[EarleyItem]) =
      _.flatMap(item => previousChart(item.initialIndex).filter(_.symbolAtDot `matchesSymbol` item.rule.baseSymbol)).partition(_.isComplete)

    val (newCompletedItems, newActiveItems) = transitiveClosure(Set(), activeItems, completedItems, getNewCompletedAndActiveItems)

    // ## Predictor

    // We have consumed the character at the current index. Now we will produce items for the next character's index.

    // Find all non-terminal symbols at dot; find rules for them; insert new Earley items with those rules, at the next index.
    val getNewPredictedItems: Set[EarleyItem] => (Set[EarleyItem], Set[EarleyItem]) = { predicted =>
      val newPredicted = predicted
        .map(_.symbolAtDot).collect { case symbol: NonTerminalSymbol => grammar.rulesForSymbol(symbol) }.flatten.map(rhs =>
          EarleyItem(rhs, 0, currentIndex + 1)
        )
      (newPredicted, Set())
    }

    // The second set is always going to be empty here.
    val (newPredictedItems, _) = transitiveClosure(Set(), Set(), newActiveItems, getNewPredictedItems)

    newPredictedItems
  }

  def earleyRecognizer(grammar: SimpleGrammar, input: Array[Char]): Array[Set[EarleyItem]] =
    input.indices.foldLeft(Array[Set[EarleyItem]]()) { (prev, i) =>
      prev.appended(earleyRecognizerStep(grammar, currentChar = input(i), currentIndex = i, previousChart = prev))
    }

}
