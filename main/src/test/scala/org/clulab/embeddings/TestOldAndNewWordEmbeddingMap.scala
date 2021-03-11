package org.clulab.embeddings

import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.InputStreamer
import org.clulab.utils.SeqOdometer
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.File

class TestOldAndNewWordEmbeddingMap extends FlatSpec with Matchers {

  if (false) {
    // Copy the text resource to a local file.
    val wordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate(fileName, compact = true)
    wordEmbeddingMap.save(fileName + InputStreamer.binExtension)
  }

  val fileName = "../glove.840B.300d.10f"
  val resourceName = "/org/clulab/glove/glove.840B.300d.10f"

  val useFileElseResource = Array(true, false)
  val useTxtElseBin = Array(true, false)
  val useExplicitElseCompact = Array(true, false)
  val useOldElseNew = Array(true, false)

  val odometer = new SeqOdometer[Boolean](Array(useFileElseResource, useTxtElseBin, useExplicitElseCompact, useOldElseNew))

  odometer.foreach { case Seq(useFileElseResource, useTxtElseBin, useExplicitElseCompact, useOldElseNew) =>
    val available = (useFileElseResource, useTxtElseBin, useExplicitElseCompact, useOldElseNew) match {
      case (_,     false, _, true) => false // We don't have the bin versions for the old.
      case (false, false, _, _   ) => false // The don't have the bin version as a resource.
      case _ => true
    }

    if (available) {
      val description = s"(useFileElseResource = $useFileElseResource, useTxtElseBin = $useTxtElseBin, useExplicitElseCompact = $useExplicitElseCompact, useOldElseNew = $useOldElseNew)"
      val name = {
        val baseName = if (useFileElseResource) fileName else resourceName
        baseName + (if (useTxtElseBin) InputStreamer.txtExtension else InputStreamer.binExtension)
      }
      println(s"Starting test with $description.")
      val start = System.currentTimeMillis()

      if (useOldElseNew) {
        val resource = !useFileElseResource
        val cached = !useTxtElseBin

        if (useExplicitElseCompact)
          new OldWordEmbeddingMap(name)
        else
          OldCompactWordEmbeddingMap(name, resource, cached)
      }
      else {
        val binary = !useTxtElseBin
        val inputStreamer = new InputStreamer()
        val inputStream =
          if (useFileElseResource) inputStreamer.getFileAsStream(name)
          else inputStreamer.getResourceAsStream(name)

        inputStream.autoClose { inputStream =>
          if (useExplicitElseCompact)
            ExplicitWordEmbeddingMap(inputStream, binary)
          else
            CompactWordEmbeddingMap(inputStream, binary)
        }
      }
      val stop = System.currentTimeMillis()
      val elapsed = stop - start
      println(s"Ending test after $elapsed ms with $description.")
    }
  }
}
