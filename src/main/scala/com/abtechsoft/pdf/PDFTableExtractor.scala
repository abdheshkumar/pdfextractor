package com.abtechsoft.pdf

import java.io.{File, FileInputStream, InputStream}
import java.util.{ArrayList, Collection, Comparator, List}

import com.google.common.collect.{HashMultimap, LinkedListMultimap, Range}
import org.apache.pdfbox.pdmodel.{PDDocument, PDPage}
import org.apache.pdfbox.util.{PDFTextStripper, TextPosition}
import org.slf4j.LoggerFactory
//remove if not needed
import scala.collection.JavaConversions._
import com.abtechsoft.pdf.entity._

object PDFTableExtractor {

  private class TextPositionExtractor private (private val page: PDPage) extends PDFTextStripper {

    val textPositions = new ArrayList[TextPosition]()

    super.setSortByPosition(true)

    protected override def processTextPosition(textPosition: TextPosition) {
      textPositions.add(textPosition)
    }

    def extract(): List[TextPosition] = {
      this.processStream(page, page.findResources(), page.getContents.getStream)
      this.textPositions.sort(new Comparator[TextPosition]() {

        override def compare(o1: TextPosition, o2: TextPosition): Int = {
          var retVal = 0
          if (o1.getY < o2.getY) {
            retVal = -1
          } else if (o1.getY > o2.getY) {
            retVal = 1
          }
          return retVal
        }
      })
      this.textPositions
    }
  }
}

class PDFTableExtractor {

  val logger = LoggerFactory.getLogger(classOf[PDFTableExtractor])

  val extractedPages = new ArrayList[Integer]()

  val exceptedPages = new ArrayList[Integer]()

  val pageNExceptedLinesMap: HashMultimap[Integer, Integer] = HashMultimap.create()

  var inputStream: InputStream = _

  var document: PDDocument = _

  def setSource(inputStream: InputStream): PDFTableExtractor = {
    this.inputStream = inputStream
    this
  }

  def setSource(file: File): PDFTableExtractor = {
    this.setSource(new FileInputStream(file))
  }

  def setSource(filePath: String): PDFTableExtractor = this.setSource(new File(filePath))

  def addPage(pageIdx: Int): PDFTableExtractor = {
    extractedPages.add(pageIdx)
    this
  }

  def exceptPage(pageIdx: Int): PDFTableExtractor = {
    exceptedPages.add(pageIdx)
    this
  }

  def exceptLine(pageIdx: Int, lineIdxs: Array[Int]): PDFTableExtractor = {
    for (lineIdx <- lineIdxs) {
      pageNExceptedLinesMap.put(pageIdx, lineIdx)
    }
    this
  }

  def exceptLine(lineIdxs: Array[Int]): PDFTableExtractor = {
    this.exceptLine(-1, lineIdxs)
    this
  }

  def extract(): List[Table] = {
    val retVal = new ArrayList[Table]()
    val pageIdNLineRangesMap: LinkedListMultimap[Integer, Range[Integer]] = LinkedListMultimap.create()
    val pageIdNTextsMap: LinkedListMultimap[Integer, TextPosition] = LinkedListMultimap.create()
    this.document = PDDocument.load(inputStream)
    for (pageId <- 0 until document.getNumberOfPages) {
      val b = !exceptedPages.contains(pageId) &&
        (extractedPages.isEmpty || extractedPages.contains(pageId))
      if (b) {
        val pdPage = document.getDocumentCatalog.getAllPages.get(pageId).asInstanceOf[PDPage]
        val texts = extractTextPositions(pdPage)
        val lineRanges = getLineRanges(pageId, texts)
        val textsByLineRanges = getTextsByLineRanges(lineRanges, texts)
        pageIdNLineRangesMap.putAll(pageId, lineRanges)
        pageIdNTextsMap.putAll(pageId, textsByLineRanges)
      }
    }
    val columnRanges = getColumnRanges(pageIdNTextsMap.values)
    for (pageId <- pageIdNTextsMap.keySet) {
      val table = buildTable(pageId, pageIdNTextsMap.get(pageId).asInstanceOf[List[_]], pageIdNLineRangesMap.get(pageId).asInstanceOf[List[_]],
        columnRanges)
      retVal.add(table)
      logger.debug("Found " + table.getRows.size + " row(s) and " + columnRanges.size +
        " column(s) of a table in page " +
        pageId)
    }
    retVal
  }

  private def buildTable(pageIdx: Int,
                         tableContent: List[TextPosition],
                         rowTrapRanges: List[Range[Integer]],
                         columnTrapRanges: List[Range[Integer]]): Table = {
    val retVal = new Table(pageIdx, columnTrapRanges.size)
    var idx = 0
    var rowIdx = 0
    val rowContent = new ArrayList[TextPosition]()
    while (idx < tableContent.size) {
      val textPosition = tableContent.get(idx)
      val rowTrapRange = rowTrapRanges.get(rowIdx)
      val textRange = Range.closed(textPosition.getY.toInt, (textPosition.getY + textPosition.getHeight).toInt)
      if (rowTrapRange.encloses(textRange)) {
        rowContent.add(textPosition)
        idx += 1
      } else {
        val row = buildRow(rowIdx, rowContent, columnTrapRanges)
        retVal.getRows.add(row)
        rowContent.clear()
        rowIdx += 1
      }
    }
    if (!rowContent.isEmpty && rowIdx < rowTrapRanges.size) {
      val row = buildRow(rowIdx, rowContent, columnTrapRanges)
      retVal.getRows.add(row)
    }
    retVal
  }

  private def buildRow(rowIdx: Int, rowContent: List[TextPosition], columnTrapRanges: List[Range[Integer]]): TableRow = {
    val retVal = new TableRow(rowIdx)
    rowContent.sort(new Comparator[TextPosition]() {

      override def compare(o1: TextPosition, o2: TextPosition): Int = {
        var retVal = 0
        if (o1.getX < o2.getX) {
          retVal = -1
        } else if (o1.getX > o2.getX) {
          retVal = 1
        }
        return retVal
      }
    })
    var idx = 0
    var columnIdx = 0
    val cellContent = new ArrayList[TextPosition]()
    while (idx < rowContent.size) {
      val textPosition = rowContent.get(idx)
      val columnTrapRange = columnTrapRanges.get(columnIdx)
      val textRange = Range.closed(textPosition.getX.toInt, (textPosition.getX + textPosition.getWidth).toInt)
      if (columnTrapRange.encloses(textRange)) {
        cellContent.add(textPosition)
        idx += 1
      } else {
        val cell = buildCell(columnIdx, cellContent)
        retVal.getCells.add(cell)
        cellContent.clear()
        columnIdx += 1
      }
    }
    if (!cellContent.isEmpty && columnIdx < columnTrapRanges.size) {
      val cell = buildCell(columnIdx, cellContent)
      retVal.getCells.add(cell)
    }
    retVal
  }

  private def buildCell(columnIdx: Int, cellContent: List[TextPosition]): TableCell = {
    cellContent.sort(new Comparator[TextPosition]() {

      override def compare(o1: TextPosition, o2: TextPosition): Int = {
        var retVal = 0
        if (o1.getX < o2.getX) {
          retVal = -1
        } else if (o1.getX > o2.getX) {
          retVal = 1
        }
        return retVal
      }
    })
    val cellContentBuilder = new StringBuilder()
    for (textPosition <- cellContent) {
      cellContentBuilder.append(textPosition.getCharacter)
    }
    val cellContentString = cellContentBuilder.toString
    TableCell(columnIdx, cellContentString)
  }

  private def extractTextPositions(pdPage: PDPage): List[TextPosition] = {
    val extractor = new TextPositionExtractor(pdPage)
    extractor.extract()
  }

  private def isExceptedLine(pageIdx: Int, lineIdx: Int): Boolean = {
    val retVal = this.pageNExceptedLinesMap.containsEntry(pageIdx, lineIdx) ||
      this.pageNExceptedLinesMap.containsEntry(-1, lineIdx)
    retVal
  }

  private def getTextsByLineRanges(lineRanges: List[Range[Integer]], textPositions: List[TextPosition]): List[TextPosition] = {
    val retVal = new ArrayList[TextPosition]()
    var idx = 0
    var lineIdx = 0
    while (idx < textPositions.size && lineIdx < lineRanges.size) {
      val textPosition = textPositions.get(idx)
      val textRange = Range.closed(textPosition.getY.toInt, (textPosition.getY + textPosition.getHeight).toInt)
      val lineRange = lineRanges.get(lineIdx)
      if (lineRange.encloses(textRange)) {
        retVal.add(textPosition)
        idx += 1
      } else if (lineRange.upperEndpoint() < textRange.lowerEndpoint()) {
        lineIdx += 1
      } else {
        idx += 1
      }
    }
    retVal
  }

  private def getColumnRanges(texts: Collection[TextPosition]): List[Range[Integer]] = {
    val rangesBuilder = new TrapRangeBuilder()
    for (text <- texts) {
      val range = Range.closed(text.getX.toInt, (text.getX + text.getWidth).toInt)
      rangesBuilder.addRange(range)
    }
    rangesBuilder.build()
  }

  private def getLineRanges(pageId: Int, pageContent: List[TextPosition]): List[Range[Integer]] = {
    val lineTrapRangeBuilder = new TrapRangeBuilder()
    for (textPosition <- pageContent) {
      val lineRange = Range.closed(textPosition.getY.toInt, (textPosition.getY + textPosition.getHeight).toInt)
      lineTrapRangeBuilder.addRange(lineRange)
    }
    val lineTrapRanges = lineTrapRangeBuilder.build()
    val retVal = removeExceptedLines(pageId, lineTrapRanges)
    retVal
  }

  private def removeExceptedLines(pageIdx: Int, lineTrapRanges: List[Range[Integer]]): List[Range[Integer]] = {
    val retVal = new ArrayList[Range[Integer]]()
    for (lineIdx <- 0 until lineTrapRanges.size) {
      val isExceptedLine = isExceptedLine(pageIdx, lineIdx) ||
        isExceptedLine(pageIdx, lineIdx - lineTrapRanges.size)
      if (!isExceptedLine) {
        retVal.add(lineTrapRanges.get(lineIdx))
      }
    }
    retVal
  }
}
