package com.abtechsoft.pdf.entity

import java.util
import scala.collection.JavaConverters._

/**
 * Created by supriya on 2/1/16.
 */
case class TableRow(idx: Int = 0, cells: util.ArrayList[TableCell] = new util.ArrayList()) {

  override def toString: String = {
    val retVal: StringBuilder = new StringBuilder
    var lastCellIdx: Int = -1
    for (cell <- cells.asScala.toList) {
      for (idx <- lastCellIdx until cell.idx - 1) retVal.append(";")

      if (retVal.length > 0) retVal.append(";")
      retVal.append(cell.content)
      lastCellIdx = cell.idx
    }

    retVal.toString
  }

}
