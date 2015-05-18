package org.eldhuset.it3708.flatland

import scala.util.Random

sealed trait Action {
  def coordinates(state: State, scenario: Flatland): Coordinates

  def apply(state: State, scenario: Flatland): (Cell, Flatland) =
    consumeCell(coordinates(state, scenario), scenario)

  def consumeCell(p: Coordinates, scenario: Flatland): (Cell, Flatland) = {
    val consumedCell = scenario(p.row, p.column)
    val newScenario = scenario.updated(p, EmptyCell())

    (consumedCell, newScenario)
  }
}

case class Up() extends Action {
  def coordinates(state: State, scenario: Flatland): Coordinates = {
    val (row, column) = (state.position.row, state.position.column)
    val newRow = if (row > 0) row - 1 else scenario.height - 1

    Coordinates(row = newRow, column = column)
  }
}

case class Right() extends Action {
  def coordinates(state: State, scenario: Flatland): Coordinates = {
    val (row, column) = (state.position.row, state.position.column)
    val newColumn = if (column < scenario.width - 1) column + 1 else 0

    Coordinates(row = row, column = newColumn)
  }
}

case class Down() extends Action {
  def coordinates(state: State, scenario: Flatland): Coordinates = {
    val (row, column) = (state.position.row, state.position.column)
    val newRow = if (row < scenario.height - 1) row + 1 else 0

    Coordinates(row = newRow, column = column)
  }
}

case class Left() extends Action {
  def coordinates(state: State, scenario: Flatland): Coordinates = {
    val (row, column) = (state.position.row, state.position.column)
    val newColumn = if (column > 0) column - 1 else scenario.width - 1

    Coordinates(row = row, column = newColumn)
  }
}

object Action {
  val possibleActions = Vector(Up(), Right(), Down(), Left())

  def randomAction: Action =
    possibleActions(Random.nextInt(possibleActions.length))
}
