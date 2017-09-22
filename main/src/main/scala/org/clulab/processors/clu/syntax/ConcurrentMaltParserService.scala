package org.clulab.processors.clu.syntax

import java.io.{FileOutputStream, FileWriter, InputStreamReader}
import java.net.{JarURLConnection, URL}
import java.util.jar.{JarEntry, JarFile}
import java.util.zip.ZipOutputStream

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

    var url = mcoURL
    if(mcoURL.toString.startsWith("jar:")) {
      // we are already in a jar (we are in code external to processors)
      val path = mcoURL.toString
      assert(path.startsWith("jar:") && path.endsWith(".mco"))
      val jarEnd = path.lastIndexOf("!/")
      val jarFileName = path.substring(9, jarEnd)
      val entryName = path.substring(jarEnd + 2)
      println("JAR file: " + jarFileName)
      println("Entry name: " + entryName)

      extractEntry(jarFileName, entryName, parserModelName + ".mco")
      // extractJar(mcoURL)

      println(path); System.exit(1)
      url = new URL(path.substring(4, jarEnd))
    }

    logger.debug(s"Using model URL: $url for parsing model: $parserModelName")
    val stream = getInputStreamReaderFromConfigFileEntry(url, parserModelName, "savedoptions.sop", "UTF-8")
    OptionManager.instance.loadOptions(optionContainer, stream)

    ConcurrentMaltParserModel(optionContainer, url)
  }

  def extractEntry(jarFileName:String, entryName:String, outFileName:String): Unit = {
    val jar = new java.util.jar.JarFile(jarFileName)
    val entry = jar.getEntry(entryName)
    if(entry != null) println("FOUND ENTRY!")
    val is = jar.getInputStream(entry)
    val fos = new FileOutputStream(outFileName)
    val buffer = new Array[Byte](16000)
    var done = false
    while(! done) {
      val num = is.read(buffer, 0, 16000)
      if(num > 0) {
        fos.write(buffer, 0, num)
      } else {
        done = true
      }
    }
    fos.close()
    is.close()
  }

  /*
  def extractJar(mcoURL:URL): Unit = {

    val path = mcoURL.toString
    assert(path.startsWith("jar:file:"))
    var jarEnd = path.lastIndexOf("!/")
    val jarFileName = path.substring(9, jarEnd)
    val entryName = path.substring(jarEnd + 2)

    println("JAR file: " + jarFileName)
    println("Entry name: " + entryName)

    //
    java.util.jar.JarFile jar = new java.util.jar.JarFile(jarFile);
    java.util.Enumeration enumEntries = jar.entries();
    while (enumEntries.hasMoreElements()) {
      java.util.jar.JarEntry file = (java.util.jar.JarEntry) enumEntries.nextElement();
      java.io.File f = new java.io.File(destDir + java.io.File.separator + file.getName());
      if (file.isDirectory()) { // if its a directory, create it
        f.mkdir();
        continue;
      }
      java.io.InputStream is = jar.getInputStream(file); // get the input stream
      java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
      while (is.available() > 0) {  // write contents of 'is' to 'fos'
        fos.write(is.read());
      }
      fos.close();
      is.close();
    }
    jar.close();
    //

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
  */

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
