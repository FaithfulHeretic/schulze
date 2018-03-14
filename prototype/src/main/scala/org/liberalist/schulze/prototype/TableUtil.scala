package org.liberalist.schulze.prototype

object TableUtil {

  sealed trait Justify

  case class LeftJustify(x: Any) extends Justify

  case class RightJustify(x: Any) extends Justify

  def createTable(originalRows: Seq[Seq[Any]]): Seq[String] = {
    val paddedRows = makeAllRowsTheSameSize(originalRows, "")
    val columns = paddedRows.transpose
    val columnWidths = columns.map(maxWidthForColumn)
    val top = makeTop(columnWidths)
    val middle = makeMiddle(columnWidths)
    val bottom = makeBottom(columnWidths)
    val formattedRows = formatRows(columnWidths, paddedRows)
    Seq(top) ++ interleave(formattedRows, middle) ++ Seq(bottom)
  }

  private def makeAllRowsTheSameSize(rows: Seq[Seq[Any]], value: Any): Seq[Seq[Any]] = {
    val rowSizes = rows.map(_.size)
    val targetSize = if (rowSizes.isEmpty) 0 else rowSizes.max

    def makeRowSameSize(row: Seq[Any]): Seq[Any] = {
      val extraCells = makeExtraCells(targetSize - row.size, value)
      row ++ extraCells
    }

    val sameSizeRows = rows.map(makeRowSameSize)
    sameSizeRows
  }

  private def makeExtraCells(howMany: Int, contents: Any): Seq[Any] = {
    (1 to howMany).map(_ => contents)
  }

  private val emptyRow = Stream.continually("")

  private def makeTop(columnWidths: Seq[Int]): String = {
    makeRow(columnWidths, emptyRow, "═", "╔", "╤", "╗")
  }

  private def makeMiddle(columnWidths: Seq[Int]): String = {
    makeRow(columnWidths, emptyRow, "─", "╟", "┼", "╢")
  }

  private def makeBottom(columnWidths: Seq[Int]): String = {
    makeRow(columnWidths, emptyRow, "═", "╚", "╧", "╝")
  }

  private def makeRow(columnWidths: Seq[Int], data: Seq[Any], padding: String, left: String, center: String, right: String): String = {
    val formattedCells = for {
      (width, cell) <- columnWidths zip data
    } yield {
      formatCell(cell, width, padding)
    }
    formattedCells.mkString(left, center, right)
  }

  private def formatRows(columnWidths: Seq[Int], rows: Seq[Seq[Any]]): Seq[String] = {
    val formatRow = makeRow(columnWidths, _: Seq[Any], " ", "║", "│", "║")
    rows.map(formatRow)
  }

  private def formatCell(cell: Any, width: Int, padding: String): String = {
    cell match {
      case LeftJustify(x) => JustifyUtil.leftJustify(cellToString(x), width, padding)
      case RightJustify(x) => JustifyUtil.rightJustify(cellToString(x), width, padding)
      case null => JustifyUtil.rightJustify(cellToString(cell), width, padding)
      case _: String => JustifyUtil.leftJustify(cellToString(cell), width, padding)
      case _ => JustifyUtil.rightJustify(cellToString(cell), width, padding)
    }
  }

  private def interleave[T](data: Seq[T], separator: T): Seq[T] = {
    def combine(soFar: List[T], next: T): List[T] = {
      next :: separator :: soFar
    }

    if (data.isEmpty) {
      Seq()
    } else {
      data.tail.foldLeft(List(data.head))(combine).reverse
    }
  }

  private def maxWidthForColumn(column: Seq[Any]): Int = {
    column.map(cellWidth).max
  }

  private def cellWidth(cell: Any): Int = {
    cellToString(cell).length
  }

  private def cellToString(cell: Any): String = {
    cell match {
      case null => "null"
      case LeftJustify(x) => cellToString(x)
      case RightJustify(x) => cellToString(x)
      case x => x.toString
    }
  }
}