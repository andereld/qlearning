package org.eldhuset.it3708.qlearning

import org.eldhuset.it3708.flatland._

case class ScenarioRepresentation(scenario: Flatland, state: State, q: Q) {
  def bestActionForCoordinates(coordinates: Coordinates): Option[Action] = {
    val assumedState = state.copy(position = coordinates)

    q get assumedState match {
      case Some(actions) if actions.nonEmpty => Some((actions maxBy (_._2))._1)
      case Some(actions) if actions.isEmpty  => None
      case None                              => None
    }
  }

  override def toString: String = {
    val cells = for {
      row <- 0 until scenario.height
      column <- 0 until scenario.width
      coordinates = Coordinates(row = row, column = column)
      str = stringForCell(scenario(row, column), coordinates)
    } yield str

    val rows = cells.grouped(scenario.width) map (_ mkString " ")

    rows mkString "\n"
  }

  def stringForCell(cell: Cell, coordinates: Coordinates): String = {
    def stringForAction(action: Option[Action]): String = action match {
      case Some(action: Up)    => "^"
      case Some(action: Right) => ">"
      case Some(action: Down)  => "v"
      case Some(action: Left)  => "<"
      case None                => " "
    }

    val contents = cell match {
      case PoisonCell() =>
        s"${Console.RED}( P )${Console.RESET}"
      case FoodCell(_) =>
        s"${Console.YELLOW}( F )${Console.RESET}"
      case StartingCell() =>
        val action = bestActionForCoordinates(coordinates)
        val actionString = stringForAction(action)
        s"${Console.GREEN}( $actionString )${Console.RESET}"
      case EmptyCell() =>
        val action = bestActionForCoordinates(coordinates)
        val actionString = stringForAction(action)
        s"( $actionString )"
    }

    if (coordinates == state.position)
      s"${Console.REVERSED}$contents${Console.RESET}"
    else
      contents
  }
}

