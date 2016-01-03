package com.abtechsoft.pdf

import java.io.{File, FileInputStream, InputStream}
import java.util.{ArrayList, List}

import com.abtechsoft.pdf.entity._
import com.google.common.collect.{HashMultimap, LinkedListMultimap, Range}
import org.apache.pdfbox.pdmodel.{PDDocument, PDPage}
import org.apache.pdfbox.util.{PDFTextStripper, TextPosition}

import scala.collection.JavaConversions._


class TextPositionExtractor(val page: PDPage) extends PDFTextStripper {

  val textPositions = new ArrayList[TextPosition]()

  super.setSortByPosition(true)

  override def processTextPosition(textPosition: TextPosition) = textPositions.add(textPosition)

  def extract(): List[TextPosition] = {
    this.processStream(page, page.findResources(), page.getContents.getStream)
    this.textPositions.sortBy(_.getY)
  }
}

class PDFTableExtractor {

  val extractedPages = new ArrayList[Int]()

  val exceptedPages = new ArrayList[Int]()

  val pageNExceptedLinesMap: HashMultimap[Int, Int] = HashMultimap.create()

  var inputStream: InputStream = _

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
    val document = PDDocument.load(inputStream)
    for (pageId <- 0 until document.getNumberOfPages) {
      val b = !exceptedPages.contains(pageId) && (extractedPages.isEmpty || extractedPages.contains(pageId))

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
      val table = buildTable(pageId, pageIdNTextsMap.get(pageId), pageIdNLineRangesMap.get(pageId), columnRanges)
      retVal.add(table)
    }
    retVal
  }

  def buildTable(pageIdx: Int,
                 tableContent: List[TextPosition],
                 rowTrapRanges: List[Range[Integer]],
                 columnTrapRanges: List[Range[Integer]]): Table = {
    val retVal = new Table(pageIdx, columnsCount = columnTrapRanges.size)
    var idx = 0
    var rowIdx = 0
    val rowContent = new ArrayList[TextPosition]()
    while (idx < tableContent.size) {
      val textPosition = tableContent.get(idx)
      val rowTrapRange = rowTrapRanges.get(rowIdx)
      val textRange = Range.closed(new Integer(textPosition.getY.toInt), new Integer((textPosition.getY + textPosition.getHeight).toInt))
      if (rowTrapRange.encloses(textRange)) {
        rowContent.add(textPosition)
        idx += 1
      } else {
        val row = buildRow(rowIdx, rowContent, columnTrapRanges)
        retVal.rows.add(row)
        rowContent.clear()
        rowIdx += 1
      }
    }
    if (!rowContent.isEmpty && rowIdx < rowTrapRanges.size) {
      val row = buildRow(rowIdx, rowContent, columnTrapRanges)
      retVal.rows.add(row)
    }
    retVal
  }

  def buildRow(rowIdx: Int, rowContent: List[TextPosition], columnTrapRanges: List[Range[Integer]]): TableRow = {
    val retVal = new TableRow(rowIdx)
    val inRowContent = rowContent.sortBy(_.getX)

    var idx = 0
    var columnIdx = 0
    val cellContent = new ArrayList[TextPosition]()
    while (idx < inRowContent.size) {
      val textPosition = inRowContent.get(idx)
      val columnTrapRange = columnTrapRanges.get(columnIdx)
      val textRange = Range.closed(new Integer(textPosition.getX.toInt), new Integer((textPosition.getX + textPosition.getWidth).toInt))
      if (columnTrapRange.encloses(textRange)) {
        cellContent.add(textPosition)
        idx += 1
      } else {
        val cell = buildCell(columnIdx, cellContent)
        retVal.cells.add(cell)
        cellContent.clear()
        columnIdx += 1
      }
    }
    if (!cellContent.isEmpty && columnIdx < columnTrapRanges.size) {
      val cell = buildCell(columnIdx, cellContent)
      retVal.cells.add(cell)
    }
    retVal
  }

  def buildCell(columnIdx: Int, cellContent: List[TextPosition]): TableCell = {
    val cellContentBuilder = new StringBuilder()
    for (textPosition <- cellContent.sortBy(_.getX)) {
      cellContentBuilder.append(textPosition.getCharacter)
    }
    val cellContentString = cellContentBuilder.toString
    TableCell(cellContentString, columnIdx)
  }

  def extractTextPositions(pdPage: PDPage): List[TextPosition] = {
    val extractor = new TextPositionExtractor(pdPage)
    extractor.extract()
  }

  def isExceptedLine(pageIdx: Int, lineIdx: Int): Boolean = {
    val retVal = this.pageNExceptedLinesMap.containsEntry(pageIdx, lineIdx) ||
      this.pageNExceptedLinesMap.containsEntry(-1, lineIdx)
    retVal
  }

  def getTextsByLineRanges(lineRanges: List[Range[Integer]], textPositions: List[TextPosition]): List[TextPosition] = {
    val retVal = new ArrayList[TextPosition]()
    var idx = 0
    var lineIdx = 0
    while (idx < textPositions.size && lineIdx < lineRanges.size) {
      val textPosition = textPositions.get(idx)
      val textRange = Range.closed(new Integer(textPosition.getY.toInt), new Integer((textPosition.getY + textPosition.getHeight).toInt))
      val lineRange = lineRanges.get(lineIdx)
      if (lineRange.encloses(textRange)) {
        retVal.add(textPosition)
        idx += 1
      } else if (lineRange.upperEndpoint() < textRange.lowerEndpoint()) lineIdx += 1
      else idx += 1
    }
    retVal
  }

  def getColumnRanges(texts: List[TextPosition]): List[Range[Integer]] = {
    val rangesBuilder = new TrapRangeBuilder()
    for (text <- texts) {
      val range = Range.closed(new Integer(text.getX.toInt), new Integer((text.getX + text.getWidth).toInt))
      rangesBuilder.addRange(range)
    }
    rangesBuilder.build
  }

  def getLineRanges(pageId: Int, pageContent: List[TextPosition]): List[Range[Integer]] = {
    val lineTrapRangeBuilder = new TrapRangeBuilder()
    for (textPosition <- pageContent) {
      val lineRange = Range.closed(new Integer(textPosition.getY.toInt), new Integer((textPosition.getY + textPosition.getHeight).toInt))
      lineTrapRangeBuilder.addRange(lineRange)
    }
    val lineTrapRanges = lineTrapRangeBuilder.build
    removeExceptedLines(pageId, lineTrapRanges)
  }

  def removeExceptedLines(pageIdx: Int, lineTrapRanges: List[Range[Integer]]): List[Range[Integer]] = {
    val retVal = new ArrayList[Range[Integer]]()
    for (lineIdx <- 0 until lineTrapRanges.size) {
      val isExcLine = isExceptedLine(pageIdx, lineIdx) || isExceptedLine(pageIdx, lineIdx - lineTrapRanges.size)
      if (!isExcLine) retVal.add(lineTrapRanges.get(lineIdx))
    }
    retVal
  }
}
