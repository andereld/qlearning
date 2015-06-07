package org.eldhuset.it3708.flatland

case class CellRow(cells: Seq[Cell]) {
  def apply(column: Int): Cell = cells(column)

  def updated(column: Int, value: Cell): CellRow =
    new CellRow(cells = cells.updated(column, value))

  def foodCount: Int = cells.foldLeft(0) { (acc, cell) =>
    cell match {
      case FoodCell(n: Int) => acc + n
      case _                => acc
    }
  }
}
