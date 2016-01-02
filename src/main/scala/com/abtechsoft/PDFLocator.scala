package com.abtechsoft

import java.nio.file.{Files, Paths}

import com.typesafe.scalalogging.LazyLogging


/** * Created by abdhesh on 1/1/16. */

object PDFLocator extends LazyLogging {

  def locateFile(filename: String) = {
    val checks = Stream(
      Paths.get(filename),
      Paths.get(System.getProperty("user.home"), filename)
    )

    val result = checks.find { path =>
      logger.info("Looking for %s in %s", filename, path.toAbsolutePath.getParent)
      Files.exists(path)
    }

    result match {
      case Some(p) => logger.info("Found config file: %s", p.toAbsolutePath)
      case None => logger.warn("Unable to locate file: %s", filename)
    }

    result
  }
}
