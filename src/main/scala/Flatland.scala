package org.eldhuset.qlearning

import java.io.File
import scala.io.Source

sealed trait Cell

case class EmptyCell() extends Cell
case class StartingCell() extends Cell
case class PoisonCell() extends Cell
case class FoodCell(foodCount: Int) extends Cell

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

class Flatland(
      width: Int,
      height: Int,
      start: (Int, Int),
      foodCount: Int,
      rows: Seq[CellRow]) {
  def apply(row: Int, column: Int): Cell = rows(row)(column)

  override def toString: String = rows map (_.toString) mkString "\n"
}

object Flatland {
  private def stringToIntList(str: String): List[Int] =
    (str.trim split raw"\s+" map (_.toInt)).toList

  private def intToCell(i: Int): Cell = i match {
    case -2         => StartingCell()
    case -1         => PoisonCell()
    case 0          => EmptyCell()
    case n if n > 0 => FoodCell(foodCount = n)
    case _          =>
      throw new Exception("Invalid integer value in scenario file.")
  }

  def fromFile(file: File): Flatland = {
    val source = Source.fromFile(file)
    val specification :: rows = source.getLines().toList

    val width :: height :: startX :: startY :: foodCount :: _ =
      stringToIntList(specification)

    val cellRows = for {
      row <- rows
      cells = stringToIntList(row) map intToCell
    } yield new CellRow(cells = cells)

    return new Flatland(
      width = width,
      height = height,
      start = (startX, startY),
      foodCount = foodCount,
      rows = cellRows)
  }
}
