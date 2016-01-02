package com.abtechsoft

import java.io.IOException

import org.apache.pdfbox.util.{PDFTextStripper, TextPosition}
import scala.util.control.Exception._
import scala.collection.JavaConverters._
import scala.collection.mutable.StringBuilder

/** * Created by abdhesh on 1/1/16. */

object PdfHandler extends PDFFileLoader {
  lazy val pdfFileName = "src/AnandSingh.pdf"
  val pdfStripper = new PrintTextLocations()

  def main(args: Array[String]) = {
    val pages = getPageContents
    pages.foreach {
      page =>
        println(s"P=$page\n")
    }
  }

  def getPageContents = {
    ultimately(pdfDocument.close()) {
      val pages = 1 to pdfDocument.getNumberOfPages
      pages.toList.flatMap {
        page =>
          pdfStripper.setStartPage(page)
          pdfStripper.setEndPage(page)
          pdfStripper.setSortByPosition(true)
          Some(pdfStripper.getText(pdfDocument))
      }
    }
  }
}

class PrintTextLocations extends PDFTextStripper {

  override def processTextPosition(text: TextPosition) = {
    super.processTextPosition(text)
  }

  @throws(classOf[IOException])
  override def writeString(text: String, textPositions: java.util.List[TextPosition]) = {
    def innerWriter(textPs: List[TextPosition], builder: StringBuilder, prevBaseFont: Option[String]): String = {
      textPs match {
        case Nil => builder.toString()
        case headTPosition :: tail =>
          val baseFont = Option(headTPosition.getFont.getBaseFont)
          val isPrevBaseFont = baseFont.isDefined && baseFont != prevBaseFont
          val pBaseF = if (isPrevBaseFont) {
            baseFont.foreach(f => builder.append(s"[FontFamily=$f,FontSize=${headTPosition.getFontSizeInPt.toInt}}]"))
            baseFont
          } else prevBaseFont
          builder.append(headTPosition.getCharacter)
          innerWriter(tail, builder, pBaseF)
      }
    }
    val builder = innerWriter(textPositions.asScala.toList, new StringBuilder, None)
    super.writeString(builder)
  }
}