package org.clulab.embeddings

import org.clulab.utils.ClassLoaderObjectInputStream
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.InputStreamer
import org.clulab.utils.SeqOdometer
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.BufferedOutputStream
import java.io.FileOutputStream
import java.io.ObjectOutputStream

class TestOldAndNewWordEmbeddingMap extends FlatSpec with Matchers {
  val unused = false
  val fileName = "../glove.840B.300d.10f"
  val resourceName = "/org/clulab/glove/glove.840B.300d.10f"

  case class WordEmbeddingConfig(useFileElseResource: Boolean, useTxtElseBin: Boolean,
      useExplicitElseCompact: Boolean, useOldElseNew: Boolean) {
    val compactExt = ".compact"
    val explicitExt = ".explicit"
    val oldExt = ".old"
    val newExt = ".new"

    val useFile = useFileElseResource
    val useResource = !useFileElseResource

    val useTxt = useTxtElseBin
    val useBin = !useTxtElseBin

    val useExplicit = useExplicitElseCompact
    val useCompact = !useExplicitElseCompact

    val useOld = useOldElseNew
    val useNew = !useOldElseNew

    val locationName = {
      val head =
        (if (useFileElseResource) fileName
        else resourceName) +
        (if (useTxtElseBin) InputStreamer.txtExtension
        else InputStreamer.binExtension)
      val tail =
        if (useResource) ""
        else
          (if (useExplicitElseCompact) explicitExt
          else compactExt) +
          (if (useOldElseNew) oldExt
          else newExt)

      head + tail
    }

    val available = (useFileElseResource, useTxtElseBin, useExplicitElseCompact, useOldElseNew) match {
      case (false, false, _,    _   ) => false // The don't have the bin version as a resource.
      case (true,  false, true, true) => true  // file, binary, explicit, old is not there.
      case _ => false
    }

    val description = {
      val top = s"(useFileElseResource = $useFileElseResource, useTxtElseBin = $useTxtElseBin, useExplicitElseCompact = $useExplicitElseCompact, useOldElseNew = $useOldElseNew)"
      val bot = s"| $useFileElseResource | $useTxtElseBin | $useExplicitElseCompact | $useOldElseNew |"

      s"$top\n$bot"
    }
  }

  class WordEmbeddingConfigIterator extends Iterator[WordEmbeddingConfig] {
    val    useFileElseResource: Array[Boolean] = Array(true, false)
    val          useTxtElseBin: Array[Boolean] = Array(true, false)
    val useExplicitElseCompact: Array[Boolean] = Array(true, false)
    val          useOldElseNew: Array[Boolean] = Array(true, false)
    val odometer = new SeqOdometer[Boolean](Array(useFileElseResource, useTxtElseBin, useExplicitElseCompact, useOldElseNew))

    override def hasNext: Boolean = odometer.hasNext

    override def next(): WordEmbeddingConfig = {
      val Seq(useFileElseResource, useTxtElseBin, useExplicitElseCompact, useOldElseNew) = odometer.next()

      WordEmbeddingConfig(useFileElseResource, useTxtElseBin, useExplicitElseCompact, useOldElseNew)
    }
  }

  def mkFileBin(wordEmbeddingConfig: WordEmbeddingConfig): Unit = {
    // This assumes that the original resource has already been copied to fileName.txt.
    wordEmbeddingConfig match {
      // useFileElseResource, useTxtElseBin, useExplicitElseCompact, useOldElseNew
      case WordEmbeddingConfig(_, _, true, true) =>
        val wordEmbeddingMap = new OldWordEmbeddingMap(fileName + InputStreamer.txtExtension)
        new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(wordEmbeddingConfig.locationName))).autoClose { objectOutputStream =>
          objectOutputStream.writeObject(wordEmbeddingMap)
        }
        // This just does output in text again, so favor the above version.
        // wordEmbeddingMap.saveMatrix(wordEmbeddingConfig.locationName)
      case WordEmbeddingConfig(_, _, false, true) =>
        val wordEmbeddingMap = OldCompactWordEmbeddingMap(fileName + InputStreamer.txtExtension, resource = false, cached = false)
        wordEmbeddingMap.save(wordEmbeddingConfig.locationName)
      case WordEmbeddingConfig(_, _, _, false) =>
        val wordEmbeddingMap = WordEmbeddingMapPool.getOrElseCreate(fileName, wordEmbeddingConfig.useCompact)
        wordEmbeddingMap.save(wordEmbeddingConfig.locationName)
    }
  }

//  mkFileBin(WordEmbeddingConfig(useFileElseResource = true, useTxtElseBin = false, useExplicitElseCompact = true,  useOldElseNew = true))
//  mkFileBin(WordEmbeddingConfig(useFileElseResource = true, useTxtElseBin = false, useExplicitElseCompact = false, useOldElseNew = true))
//  mkFileBin(WordEmbeddingConfig(useFileElseResource = true, useTxtElseBin = false, useExplicitElseCompact = true,  useOldElseNew = false))
//  mkFileBin(WordEmbeddingConfig(useFileElseResource = true, useTxtElseBin = false, useExplicitElseCompact = false, useOldElseNew = false))

  new WordEmbeddingConfigIterator().foreach { wordEmbeddingConfig =>
    val available = wordEmbeddingConfig.available
    val description = wordEmbeddingConfig.description
    val locationName = wordEmbeddingConfig.locationName

    val resource = wordEmbeddingConfig.useResource
    val cached = wordEmbeddingConfig.useBin

    if (available) {
      println(s"Starting test of $locationName with $description.")
      val start = System.currentTimeMillis()
      val wordEmbeddingMap =
        if (wordEmbeddingConfig.useOld)
          wordEmbeddingConfig match {
            // useFileElseResource, useTxtElseBin, useExplicitElseCompact, useOldElseNew
            case WordEmbeddingConfig(true /* file */ , true /* txt */ , true /* explicit */ , _) =>
              new OldWordEmbeddingMap(locationName)
            case WordEmbeddingConfig(true /* file */ , false /* bin */ , true /* explicit */ , _) =>
              val inputStreamer = new InputStreamer()
              val inputStream = inputStreamer.getFileAsStream(locationName)
              inputStream.autoClose { inputStream =>
                val objectInputStream = new ClassLoaderObjectInputStream(this.getClass.getClassLoader, inputStream)
                objectInputStream.readObject().asInstanceOf[OldWordEmbeddingMap]
              }
            // OldWordEmbeddingMap.fromBinary(locationName) // This is not right.
            case WordEmbeddingConfig(false /* resource */ , true /* txt */ , true /* explicit */ , _) =>
              val inputStreamer = new InputStreamer()
              val inputStream = inputStreamer.getResourceAsStream(locationName)
              inputStream.autoClose { inputStream =>
                new OldWordEmbeddingMap(inputStream, None, false)
              }
            case WordEmbeddingConfig(false /* resource */ , false /* bin */ , true /* explicit */ , _) =>
              ??? // This option is not available

            case WordEmbeddingConfig(_, _, false /* compact */ , _) =>
              OldCompactWordEmbeddingMap(locationName, resource, cached)
          }
        else {
          val inputStreamer = new InputStreamer()
          val inputStream =
            if (wordEmbeddingConfig.useFileElseResource) inputStreamer.getFileAsStream(locationName)
            else inputStreamer.getResourceAsStream(locationName)

          inputStream.autoClose { inputStream =>
            if (wordEmbeddingConfig.useExplicitElseCompact)
              ExplicitWordEmbeddingMap(inputStream, wordEmbeddingConfig.useBin)
            else
              CompactWordEmbeddingMap(inputStream, wordEmbeddingConfig.useBin)
          }
        }
      val stop = System.currentTimeMillis()
      val elapsed = stop - start

      println(s"Ending test after $elapsed ms of $locationName with $description.")
    }
  }
}
