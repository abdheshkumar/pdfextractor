package com.abtechsoft

import java.nio.file.Files

import com.typesafe.scalalogging.LazyLogging
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.util.PDFTextStripper

import scala.util.{Failure, Success, Try}

/** * Created by abdhesh on 1/1/16. */

trait PDFFileLoader extends LazyLogging {

  val pdfFileName: String
  val pdfStripper: PDFTextStripper

  def pdfStream =
    PDFLocator.locateFile(pdfFileName) match {
      case Some(path) => Files.newInputStream(path)
      case None =>
        logger.warn("Looking for %s in classpath", pdfFileName)
        getClass.getResourceAsStream(s"/$pdfFileName")
    }

  lazy val pdfDocument =
    Try(pdfStream)
      .map {
      stream => PDDocument.load(stream)
    } match {
      case Success(pdfDoc) => pdfDoc
      case Failure(ex) => throw new RuntimeException(s"Failed to load $pdfFileName", ex)
    }
}
