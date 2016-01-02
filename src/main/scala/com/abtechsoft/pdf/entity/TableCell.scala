package com.abtechsoft.pdf.entity

/**
  * Created by supriya on 2/1/16.
  */
case class TableCell
(
  content: String,
  idx: Int = 0) {

  def this(idx: Int, content: String) {
    this()
    idx
    content
  }

  def getContent: String = {
    return content
  }

  def getIdx: Int = {
    return idx
  }
}

