package org.clulab.processors.clu.syntax

import java.io.InputStreamReader
import java.net.{JarURLConnection, URL}
import java.util.jar.{JarEntry, JarFile}

import org.maltparser.core.lw.helper.Utils
import org.maltparser.core.options.OptionManager
import org.slf4j.{Logger, LoggerFactory}

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

    var addJarPrefix = true
    var url = mcoURL
    if(! mcoURL.toString.startsWith("file:")) {
      addJarPrefix = false
      val path = mcoURL.toString
      assert(path.startsWith("jar:") && path.endsWith(".mco"))
      url = new URL(path.substring(0, path.length - 4) + "/")
    }

    logger.debug(s"Actual model URL used: $url")
    val stream = getInputStreamReaderFromConfigFileEntry(url, parserModelName, "savedoptions.sop", "UTF-8", addJarPrefix)
    OptionManager.instance.loadOptions(optionContainer, stream)

    ConcurrentMaltParserModel(optionContainer, url)
  }

  def getInputStreamReaderFromConfigFileEntry(
    mcoURL: URL,
    mcoName: String,
    fileName: String,
    charSet: String,
    addJarPrefix:Boolean): InputStreamReader = {
    val mcoJarFile = getConfigJarfile(mcoURL, addJarPrefix)
    val entry = getConfigFileEntry(mcoJarFile, mcoName, fileName)
    logger.debug(s"getConfigFileEntry: ${entry.toString}")
    val stream = mcoJarFile.getInputStream(entry)
    new InputStreamReader(stream, charSet)
  }

  def getConfigJarfile(mcoURL: URL, addJarPrefix:Boolean): JarFile = {
    val conn =
      if(addJarPrefix) new URL("jar:" + mcoURL.toString() + "!/").openConnection().asInstanceOf[JarURLConnection]
      else mcoURL.openConnection().asInstanceOf[JarURLConnection]
    conn.getJarFile
  }

  def getConfigFileEntry(mcoJarFile: JarFile, mcoName: String, fileName: String): JarEntry = {
    logger.debug(s"Looking for file ${mcoName + '/' + fileName}")
    var entry = mcoJarFile.getJarEntry(mcoName + '/' + fileName)
    if(entry == null) entry = mcoJarFile.getJarEntry(mcoName + '\\' + fileName)
    entry
  }
}
