package org.eldhuset.it3708.flatland

sealed trait Cell

case class EmptyCell() extends Cell
case class StartingCell() extends Cell
case class PoisonCell() extends Cell
case class FoodCell(foodCount: Int) extends Cell
