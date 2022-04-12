package org.clulab.embeddings

import org.clulab.dynet.ConstEmbeddingsGlove
import org.clulab.dynet.Utils
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

  Utils.initializeDyNet()

  case class WordEmbeddingConfig(useFileElseResource: Boolean, useTxtElseBin: Boolean,
      useExplicitElseCompact: Boolean, useOldElseNew: Boolean) {
    val compactExt = ".compact"
    val explicitExt = ".explicit"
    val oldExt = ".old"
    val newExt = ".new"

    val useFile: Boolean = useFileElseResource
    val useResource: Boolean = !useFileElseResource

    val useTxt: Boolean = useTxtElseBin
    val useBin: Boolean = !useTxtElseBin // Sometimes bin is java serialization and sometimes it is kryo.

    val useExplicit: Boolean = useExplicitElseCompact
    val useCompact: Boolean = !useExplicitElseCompact

    val useOld: Boolean = useOldElseNew
    val useNew: Boolean = !useOldElseNew

    val locationName: String = {
      val head =
        (if (useFileElseResource) fileName
        else resourceName) +
        (if (useTxtElseBin) InputStreamer.txtExtension
        else InputStreamer.binExtension)
      val tail =
        if (useResource || useTxtElseBin) ""
        else
          (if (useExplicitElseCompact) explicitExt
          else compactExt) +
          (if (useOldElseNew) oldExt
          else newExt)

      head + tail
    }

    val available: Boolean = (useFileElseResource, useTxtElseBin, useExplicitElseCompact, useOldElseNew) match {
      case (false, false, _,    _   ) => false // The don't have the bin version as a resource.
      // case (true, false, true, false) => true // Try to optimize this one.
      case _ => true // usually true
    }

    val description =
        s"(useFileElseResource = $useFileElseResource, useTxtElseBin = $useTxtElseBin, useExplicitElseCompact = $useExplicitElseCompact, useOldElseNew = $useOldElseNew)"

    def summary(operation: String, wordEmbeddingMap: WordEmbeddingMap, elapsed: Long): String = {
      val header  = s"| operation | time (ms) | class | location | useFileElseResource | useTxtElseBin | useExplicitElseCompact | useOldElseNew |"
      val divider = s"| --------- | --------- | ----- | -------- | ------------------- | ------------- | ---------------------- | ------------- |"
      val content = s"| $operation | $elapsed | ${wordEmbeddingMap.getClass.getSimpleName} | $locationName | $useFileElseResource | $useTxtElseBin | $useExplicitElseCompact | $useOldElseNew |"

      s"$header\n$divider\n$content"
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

  def mkFileBins(): Unit = {
//     mkFileBin(WordEmbeddingConfig(useFileElseResource = true, useTxtElseBin = false, useExplicitElseCompact = true,  useOldElseNew = true))
//     mkFileBin(WordEmbeddingConfig(useFileElseResource = true, useTxtElseBin = false, useExplicitElseCompact = false, useOldElseNew = true))
//     mkFileBin(WordEmbeddingConfig(useFileElseResource = true, useTxtElseBin = false, useExplicitElseCompact = true,  useOldElseNew = false))
     mkFileBin(WordEmbeddingConfig(useFileElseResource = true, useTxtElseBin = false, useExplicitElseCompact = false, useOldElseNew = false))
  }

  def loadWordEmbeddingMap(wordEmbeddingConfig: WordEmbeddingConfig): WordEmbeddingMap = {
    val description = wordEmbeddingConfig.description
    val locationName = wordEmbeddingConfig.locationName

    val resource = wordEmbeddingConfig.useResource
    val cached = wordEmbeddingConfig.useBin

    println(s"Starting load test at $locationName with $description.")
    val start = System.currentTimeMillis()
    val wordEmbeddingMap: WordEmbeddingMap =
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

    println(s"Ending load test of ${wordEmbeddingMap.getClass.getSimpleName} after $elapsed ms at $locationName with $description.")
    println(wordEmbeddingConfig.summary("load", wordEmbeddingMap, elapsed))
    wordEmbeddingMap
  }

  def runWordEmbeddingMap(wordEmbeddingMap: WordEmbeddingMap, wordEmbeddingConfig: WordEmbeddingConfig, words: Iterable[String]): Unit = {
    val description = wordEmbeddingConfig.description
    val locationName = wordEmbeddingConfig.locationName

    println(s"Starting run test of ${wordEmbeddingMap.getClass.getSimpleName} at $locationName with $description.")
    val start = System.currentTimeMillis()

    ConstEmbeddingsGlove.mkConstLookupParams(words.toSet, wordEmbeddingMap)

    val stop = System.currentTimeMillis()
    val elapsed = stop - start

    println(s"Ending run test of ${wordEmbeddingMap.getClass.getSimpleName} after $elapsed ms at $locationName with $description.")
    println(wordEmbeddingConfig.summary("run", wordEmbeddingMap, elapsed))
  }

  def run(): Unit = {
    val words = {
      val compactWordEmbeddingMap = CompactWordEmbeddingMap(fileName + ".bin.compact.new", resource = false, cached = true)
      compactWordEmbeddingMap.keys
    }
    new WordEmbeddingConfigIterator().foreach { wordEmbeddingConfig =>
      val available = wordEmbeddingConfig.available
      if (available) {
        val wordEmbeddingMap = loadWordEmbeddingMap(wordEmbeddingConfig)
        runWordEmbeddingMap(wordEmbeddingMap, wordEmbeddingConfig, words)
      }
    }
  }

  behavior of "WordEmbeddingMap"

  it should "run" in {
//    mkFileBins()
//    run()
  }
}
