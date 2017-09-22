package org.clulab.processors.clu.syntax

import java.io.{FileOutputStream, FileWriter, InputStreamReader}
import java.net.{JarURLConnection, URL}
import java.util.jar.{JarEntry, JarFile}
import java.util.zip.ZipOutputStream

import org.maltparser.core.lw.helper.Utils
import org.maltparser.core.options.OptionManager
import org.slf4j.{Logger, LoggerFactory}

import scala.Predef.StringFormat

/**
  * This class replicates the org.maltparser.concurrent.ConcurrentMaltParserService class, with additional functionality to solve the jar-in-a-jar problem.
  * This code may need to be updated whenever there is a version change in maltparser
  */
class ConcurrentMaltParserService

object ConcurrentMaltParserService {
  val logger: Logger = LoggerFactory.getLogger(classOf[ConcurrentMaltParserService])

  private var optionContainerCounter = 0

  def getNextOptionContainerCounter:Int = {
    this.synchronized {
      val v = optionContainerCounter
      optionContainerCounter += 1
      v
    }
  }

  private def loadOptions() {
    this.synchronized {
      if (! OptionManager.instance.hasOptions) {
        OptionManager.instance.loadOptionDescriptionFile()
        OptionManager.instance.generateMaps()
      }
    }
  }

  def initializeParserModel(mcoURL: URL): ConcurrentMaltParserModel = {
    loadOptions()
    val optionContainer = getNextOptionContainerCounter
    val parserModelName = Utils.getInternalParserModelName(mcoURL)
    OptionManager.instance.parseCommandLine("-m parse", optionContainer)

    var url = mcoURL
    if(mcoURL.toString.startsWith("jar:")) {
      // we are already in a jar (we are in code external to processors)
      val path = mcoURL.toString
      assert(path.startsWith("jar:") && path.endsWith(".mco"))

      extractJar(mcoURL)

      println(path); System.exit(1)
      val jarEnd = path.lastIndexOf("!/")
      url = new URL(path.substring(4, jarEnd))
    }

    logger.debug(s"Using model URL: $url for parsing model: $parserModelName")
    val stream = getInputStreamReaderFromConfigFileEntry(url, parserModelName, "savedoptions.sop", "UTF-8")
    OptionManager.instance.loadOptions(optionContainer, stream)

    ConcurrentMaltParserModel(optionContainer, url)
  }

  def extractJar(mcoURL:URL): Unit = {



    val ir = mcoURL.openConnection().asInstanceOf[JarURLConnection].getInputStream

    val fw = new ZipOutputStream(new FileOutputStream("tmp.mco"))
    val buf = new Array[Byte](1000)
    var done = false
    while(! done) {
      val num = ir.read(buf, 0, 1000)
      if(num == -1) {
        done = true
      } else {
        fw.write(buf, 0, num)
      }
    }
    fw.close()
    ir.close()
  }

  def getInputStreamReaderFromConfigFileEntry(
    mcoURL: URL,
    mcoName: String,
    fileName: String,
    charSet: String): InputStreamReader = {
    val mcoJarFile = getConfigJarfile(mcoURL)
    val entry = getConfigFileEntry(mcoJarFile, mcoName, fileName)
    val stream = mcoJarFile.getInputStream(entry)
    new InputStreamReader(stream, charSet)
  }

  def getConfigJarfile(mcoURL: URL): JarFile = {
    val conn = new URL("jar:" + mcoURL.toString + "!/").openConnection().asInstanceOf[JarURLConnection]
    conn.getJarFile
  }

  def getConfigFileEntry(mcoJarFile: JarFile, mcoName: String, fileName: String): JarEntry = {
    var entry = mcoJarFile.getJarEntry(mcoName + '/' + fileName)
    // if(entry == null) entry = mcoJarFile.getJarEntry(mcoName + '\\' + fileName)
    entry
  }
}
