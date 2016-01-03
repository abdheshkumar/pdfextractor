package com.abtechsoft.pdf

import scala.collection.JavaConverters._

/**
 * Created by abdhesh on 1/3/16.
 */
object Tester {
  def main(args: Array[String]) {
    val extractor = new PDFTableExtractor();
    val tables = extractor.setSource("src/sample-1.pdf")
      //.addPage(0)//to determine which pages will be extracted table content. If not set, all pages will be extracted
      //.addPage(1)
      //.exceptLine(0) //the first line in each page
      .exceptLine(Array(0, 1)) //the second line in each page
      //.exceptLine(-1) //the last line in each page
      .extract()
    tables.asScala.toList.foreach {
      f =>
        println("::::::::::::" + f.toString)

    }
  }

}
