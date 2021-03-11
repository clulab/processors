package org.clulab.embeddings

import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.InputStreamer
import org.clulab.utils.SeqOdometer
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TestOldAndNewWordEmbeddingMap extends FlatSpec with Matchers {
  val compactExt = ".compact"
  val explicitExt = ".explicit"
  val oldExt = ".old"
  val newExt = ".new"
  val fileName = "../glove.840B.300d.10f"
  val resourceName = "/org/clulab/glove/glove.840B.300d.10f"

  if (false) {
    val wordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate(fileName, compact = true)
    wordEmbeddingMap.save(fileName + InputStreamer.binExtension + compactExt + newExt)
  }

  if (false) {
    val wordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate(fileName, compact = false)
    wordEmbeddingMap.save(fileName + InputStreamer.binExtension + explicitExt + newExt)
  }

  if (false) {
    val wordEmbeddingMap = OldCompactWordEmbeddingMap(fileName + InputStreamer.txtExtension, resource = false, cached = false)
    wordEmbeddingMap.save(fileName + InputStreamer.binExtension + compactExt + oldExt)
  }

  if (false) {
    val wordEmbeddingMap = new OldWordEmbeddingMap(fileName + InputStreamer.txtExtension)
    wordEmbeddingMap.saveMatrix(fileName + InputStreamer.binExtension + explicitExt + oldExt)
  }

  val    useFileElseResource: Array[Boolean] = Array(true, false)
  val          useTxtElseBin: Array[Boolean] = Array(true, false)
  val useExplicitElseCompact: Array[Boolean] = Array(true, false)
  val          useOldElseNew: Array[Boolean] = Array(true, false)

  val odometer = new SeqOdometer[Boolean](Array(useFileElseResource, useTxtElseBin, useExplicitElseCompact, useOldElseNew))

  odometer.foreach { case Seq(useFileElseResource, useTxtElseBin, useExplicitElseCompact, useOldElseNew) =>
    val available = (useFileElseResource, useTxtElseBin, useExplicitElseCompact, useOldElseNew) match {
      case (false, false, _,    _   ) => false // The don't have the bin version as a resource.
      case (true,  false, true, true) => false // file, binary, explicit, old is not there.
      case _ => true
    }

    if (available) {
      val description = s"(useFileElseResource = $useFileElseResource, useTxtElseBin = $useTxtElseBin, useExplicitElseCompact = $useExplicitElseCompact, useOldElseNew = $useOldElseNew)"
      val name = {
        val baseName = if (useFileElseResource) fileName else resourceName
        val baseExtName = baseName + (if (useTxtElseBin) InputStreamer.txtExtension else InputStreamer.binExtension)
        val name = baseExtName + (
          if (!useTxtElseBin)
              (if (useExplicitElseCompact) explicitExt else compactExt) +
              (if (useOldElseNew) oldExt else newExt)
          else ""
        )

        name
      }
      println(s"Starting test of $name with $description.")
      val start = System.currentTimeMillis()

      if (useOldElseNew) {
        val resource = !useFileElseResource
        val cached = !useTxtElseBin

        if (useExplicitElseCompact) {
          if (useFileElseResource) {
            if (useTxtElseBin)
              new OldWordEmbeddingMap(name)
            else
              OldWordEmbeddingMap.fromBinary(name) // This is not right.
          }
          else {
            val inputStreamer = new InputStreamer()
            val inputStream = inputStreamer.getResourceAsStream(name)
            inputStream.autoClose { inputStream =>
              new OldWordEmbeddingMap(inputStream, None, false)
            }
          }
        }
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
      println(s"Ending test after $elapsed ms of $name with $description.")
    }
  }
}
