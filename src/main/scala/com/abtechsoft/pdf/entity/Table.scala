package com.abtechsoft.pdf.entity

/**
  * Created by supriya on 2/1/16.
  */
case class Table
(
  pageIdx: Int = 0,
  rows: List[TableRow] = List(),
  columnsCount: Int = 0
) {

  def this(idx: Int, columnsCount: Int) {
    this()
    idx
    columnsCount
  }

  def getPageIdx: Int = {
    return pageIdx
  }

  def getRows: List[TableRow] = {
    return rows
  }

  def toHtml: String = {
    val retVal: StringBuilder = new StringBuilder
    retVal.append("<!DOCTYPE html>" + "<html>" + "<head>" + "<meta charset='utf-8'>").append("</head>").append("<body>")
    retVal.append("<table border='1'>")
    for (row <- rows) {
      retVal.append("<tr>")
      var cellIdx: Int = 0
      var columnIdx: Int = 0
      while (columnIdx < columnsCount) {
        if (cellIdx < row.getCells.size) {
          val cell: TableCell = row.getCells.get(cellIdx)
          if (cell.getIdx == columnIdx) {
            retVal.append("<td>").append(cell.getContent).append("</td>")
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
          break //todo: break is not supported
        }
      }
      retVal.append("</tr>")
    }
    retVal.append("</table>").append("</body>").append("</html>")
    return retVal.toString
  }

  override def toString: String = {
    val retVal: StringBuilder = new StringBuilder
    for (row <- rows) {
      if (retVal.length > 0) {
        retVal.append("\n")
      }
      retVal.append(row.toString)
    }
    return retVal.toString
  }
}



