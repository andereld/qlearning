package org.eldhuset.it3708.qlearning

import org.eldhuset.it3708.flatland.{PoisonCell, FoodCell, Cell}

case class Statistics(
   foodConsumed: Int = 0,
   totalFood: Int = 0,
   poisonConsumed: Int = 0,
   stepsPerformed: Int = 0) {

  override def toString: String = Seq(
    s"Run statistics:",
    s"\tFood cells consumed: $foodConsumed / $totalFood",
    s"\tPoison cells consumed: $poisonConsumed",
    s"\tNumber of steps: $stepsPerformed"
  ) mkString "\n"

  def updated(consumedCell: Cell): Statistics = {
    val steps = stepsPerformed + 1

    consumedCell match {
      case FoodCell(_) =>
        this.copy(stepsPerformed = steps, foodConsumed = foodConsumed + 1)
      case PoisonCell() =>
        this.copy(stepsPerformed = steps, poisonConsumed = poisonConsumed + 1)
      case _ =>
        this.copy(stepsPerformed = steps)
    }
  }
}
