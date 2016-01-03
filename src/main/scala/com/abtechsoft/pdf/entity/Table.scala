package com.abtechsoft.pdf.entity

import java.util
import scala.collection.JavaConverters._

/**
 * Created by supriya on 2/1/16.
 */
case class Table(pageIdx: Int = 0, rows: util.ArrayList[TableRow] = new util.ArrayList(), columnsCount: Int = 0) {

  def toHtml: String = {
    val retVal: StringBuilder = new StringBuilder
    retVal.append("<!DOCTYPE html>" + "<html>" + "<head>" + "<meta charset='utf-8'>").append("</head>").append("<body>")
    retVal.append("<table border='1'>")
    for (row <- rows.asScala) {
      retVal.append("<tr>")
      var cellIdx: Int = 0
      var columnIdx: Int = 0
      while (columnIdx < columnsCount) {
        if (cellIdx < row.cells.size()) {
          val cell: TableCell = row.cells.get(cellIdx)
          if (cell.idx == columnIdx) {
            retVal.append("<td>").append(cell.content).append("</td>")
            cellIdx += 1
            columnIdx += 1
          }
          else if (columnIdx < cellIdx) {
            retVal.append("<td>").append("</td>")
            columnIdx += 1
          }
          else {
            throw new RuntimeException("Invalid state")
          }
        }
        else {
        }
      }
      retVal.append("</tr>")
    }
    retVal.append("</table>").append("</body>").append("</html>")
    retVal.toString
  }

  override def toString: String = {
    val retVal: StringBuilder = new StringBuilder
    for (row <- rows.asScala) {
      if (retVal.length > 0) retVal.append("\n")
      retVal.append(row.toString)
    }
    retVal.toString
  }
}



