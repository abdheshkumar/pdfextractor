package com.abtechsoft.pdf.entity

/**
  * Created by supriya on 2/1/16.
  */
case class TableRow
(
  idx: Int = 0,
  cells: List[TableCell] = List()
) {

  def this(idx: Int) {
    this()
    idx
    this.cells
  }

  /**
    *
    * @return
    */
  override def toString: String = {
    val retVal: StringBuilder = new StringBuilder
    var lastCellIdx: Int = -1
    for (cell <- cells) {
      {
        for (idx <- lastCellIdx until cell.getIdx - 1) {
          retVal.append(";")
        }
      }
      if (retVal.length > 0) {
        retVal.append(";")
      }
      retVal.append(cell.getContent)
      lastCellIdx = cell.getIdx
    }
    retVal.toString
  }

}
