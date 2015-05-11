package org.eldhuset.it3708.flatland

import org.eldhuset.it3708.qlearning

import java.io.File
import scala.io.Source

class Flatland(
      width: Int,
      height: Int,
      start: (Int, Int),
      foodCount: Int,
      rows: Seq[CellRow]) extends qlearning.Scenario[Flatland] {
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
