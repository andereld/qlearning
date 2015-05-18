package org.eldhuset.it3708.flatland

sealed trait Cell extends Serializable {
  def reward(scenario: Flatland): Double = this match {
    case EmptyCell()    => 0
    case StartingCell() => if (scenario.foodCount == 0) 3 else 0
    case PoisonCell()   => -2
    case FoodCell(_)    => 1
  }
}

case class EmptyCell() extends Cell
case class StartingCell() extends Cell
case class PoisonCell() extends Cell
case class FoodCell(foodCount: Int) extends Cell
