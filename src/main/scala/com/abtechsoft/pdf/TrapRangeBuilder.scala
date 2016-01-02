package com.abtechsoft.pdf

import java.util.Comparator

import org.slf4j.{Logger, LoggerFactory}

/**
  * Created by supriya on 2/1/16.
  */


class TrapRangeBuilder {
  private final val logger: Logger = LoggerFactory.getLogger(classOf[TrapRangeBuilder])
  private final val ranges: util.List[Range[Integer]] = new util.ArrayList[Range[Integer]]

  def addRange(range: Range[Integer]): TrapRangeBuilder = {
    ranges.add(range)
    this
  }

  /**
    * The result will be ordered by lowerEndpoint ASC
    *
    * @return
    */
  def build: util.List[Range[Integer]] = {
    val retVal: util.List[Range[Integer]] = new util.ArrayList[Range[Integer]]
    ranges.sort(new Comparator[Range[_ <: Comparable[_]]]() {
      def compare(o1: Range[_ <: Comparable[_]], o2: Range[_ <: Comparable[_]]): Int = {
        o1.lowerEndpoint.compareTo(o2.lowerEndpoint)
      }
    })
    for (range <- ranges) {
      if (retVal.isEmpty) {
        retVal.add(range)
      }
      else {
        val lastRange: Range[Integer] = retVal.get(retVal.size - 1)
        if (lastRange.isConnected(range)) {
          val newLastRange: Range[_ <: Comparable[_]] = lastRange.span(range)
          retVal.set(retVal.size - 1, newLastRange)
        }
        else {
          retVal.add(range)
        }
      }
    }
    logger.debug("Found " + retVal.size + " trap-range(s)")
    retVal
  }
}

