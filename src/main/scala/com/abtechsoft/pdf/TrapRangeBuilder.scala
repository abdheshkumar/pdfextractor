package com.abtechsoft.pdf

import java.util

import com.google.common.collect.Range
import scala.collection.JavaConverters._

/**
 * Created by supriya on 2/1/16.
 */


class TrapRangeBuilder {
  private[this] val ranges = new util.ArrayList[Range[Integer]]()

  def addRange(range: Range[Integer]): TrapRangeBuilder = {
    ranges.add(range)
    this
  }

  /** * The result will be ordered by lowerEndpoint ASC */
  def build = {
    val retVal = new util.ArrayList[Range[Integer]]
    val inRanges = ranges.asScala.toList.sortBy(_.lowerEndpoint)
    for (range <- inRanges) {
      if (retVal.isEmpty) retVal.add(range)
      else {
        val lastRange = retVal.get(retVal.size - 1)
        if (lastRange.isConnected(range)) {
          val newLastRange = lastRange.span(range)
          retVal.set(retVal.size - 1, newLastRange)
        }
        else retVal.add(range)
      }
    }
    retVal
  }
}

