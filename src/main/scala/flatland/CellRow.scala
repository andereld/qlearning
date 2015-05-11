package org.eldhuset.it3708.flatland

class CellRow(cells: Seq[Cell]) {
  def apply(column: Int): Cell = cells(column)

  override def toString: String = {
    cells map {
      case EmptyCell()      => emptyCell
      case StartingCell()   => startingCell
      case PoisonCell()     => poisonCell
      case FoodCell(n: Int) => foodCell(n)
    } mkString " "
  }

  private def emptyCell = "(    )"
  private def startingCell = s"${Console.BLUE}(    )${Console.RESET}"
  private def poisonCell =  s"${Console.RED}(    )${Console.RESET}"
  private def foodCell(n: Int) = f"${Console.GREEN}( $n%2d )${Console.RESET}"
}
