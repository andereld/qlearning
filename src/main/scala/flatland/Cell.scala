package org.eldhuset.it3708.flatland

sealed trait Cell extends Serializable {
  def reward(scenario: Flatland): Double = this match {
    case EmptyCell()    => -1
    case StartingCell() => if (scenario.foodCount == 0) 100 else -1
    case PoisonCell()   => -1000
    case FoodCell(_)    => 100
  }
}

case class EmptyCell() extends Cell
case class StartingCell() extends Cell
case class PoisonCell() extends Cell
case class FoodCell(foodCount: Int) extends Cell
