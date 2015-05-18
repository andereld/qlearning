package org.eldhuset.it3708.flatland

import java.io.File
import scala.io.Source

case class Flatland(
      width: Int,
      height: Int,
      start: Coordinates,
      initialFoodCount: Int,
      rows: Seq[CellRow]) {

  def apply(row: Int, column: Int): Cell = rows(row)(column)

  def updated(row: Int, column: Int, value: Cell): Flatland = {
    val newRow = rows(row).updated(column, value)
    Flatland(
      width = width,
      height = height,
      start = start,
      initialFoodCount = initialFoodCount,
      rows = rows.updated(row, newRow))
  }

  def updated(position: Coordinates, value: Cell): Flatland =
    updated(row = position.row, column = position.column, value = value)

  def foodCount: Int =
    rows.foldLeft(0) ((acc: Int, row: CellRow) => acc + row.foodCount)

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
    } yield CellRow(cells = cells)

    return Flatland(
      width = width,
      height = height,
      start = Coordinates(row = startY, column = startX),
      initialFoodCount = foodCount,
      rows = cellRows)
  }
}
