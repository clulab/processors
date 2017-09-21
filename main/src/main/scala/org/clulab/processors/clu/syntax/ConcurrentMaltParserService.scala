package org.clulab.processors.clu.syntax

import java.io.InputStreamReader
import java.net.{JarURLConnection, URL}
import java.util.jar.{JarEntry, JarFile}

import org.maltparser.core.lw.helper.Utils
import org.maltparser.core.options.OptionManager

/**
  * This class replicates the org.maltparser.concurrent.ConcurrentMaltParserService class, with additional functionality to solve the jar-in-a-jar problem.
  * This code may need to be updated whenever there is a version change in maltparser
  */
class ConcurrentMaltParserService

object ConcurrentMaltParserService {
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

    if(mcoURL.toString.startsWith("file:")) {
      // TODO
    }

    val stream = Utils.getInputStreamReaderFromConfigFileEntry(mcoURL, parserModelName, "savedoptions.sop", "UTF-8")
    OptionManager.instance.loadOptions(optionContainer, stream)

    null
    //new ConcurrentMaltParserModel(optionContainer, mcoURL)
  }

  def getInputStreamFromConfigFileEntry(mcoURL: URL, mcoName: String, fileName: String, charSet: String): InputStreamReader = {
    val mcoJarFile = getConfigJarfile(mcoURL, true)
    val entry = getConfigFileEntry(mcoJarFile, mcoName, fileName)
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
    var entry = mcoJarFile.getJarEntry(mcoName + '/' + fileName)
    if(entry == null) entry = mcoJarFile.getJarEntry(mcoName + '\\' + fileName)
    entry
  }
}
