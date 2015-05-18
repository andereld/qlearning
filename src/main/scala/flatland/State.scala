package org.eldhuset.it3708.flatland

case class State(position: Coordinates, consumedFoodCells: Set[Coordinates]) {
  def updated(newPosition: Coordinates, consumedCell: Cell): State =
    consumedCell match {
      case FoodCell(_) => State(newPosition, consumedFoodCells + newPosition)
      case _           => State(newPosition, consumedFoodCells)
    }

  def finished(scenario: Flatland): Boolean =
    position == scenario.start && scenario.foodCount == 0
}

object State {
  def initialState(scenario: Flatland): State =
    State(position = scenario.start, consumedFoodCells = Set[Coordinates]())
}
