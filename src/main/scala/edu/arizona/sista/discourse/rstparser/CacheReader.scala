package edu.arizona.sista.discourse.rstparser

import java.io._
import edu.arizona.sista.processors.{Processor, Document}
import org.slf4j.LoggerFactory
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import edu.arizona.sista.utils.ClassLoaderObjectInputStream

/**
 * Caches the output of Reader.readDir, so we don't parse everything everytime
 * User: mihais
 * Date: 5/25/14
 */
object CacheReader {
  val logger = LoggerFactory.getLogger(classOf[CacheReader])

  lazy val CORENLP_PROCESSOR = new CoreNLPProcessor(withDiscourse = false)
  lazy val FASTNLP_PROCESSOR = new FastNLPProcessor(useMalt = false, withDiscourse = false)

  def getProcessor(dependencySyntax:Boolean):Processor =
    dependencySyntax match {
      case true => FASTNLP_PROCESSOR
      case _ => CORENLP_PROCESSOR
    }

  def main(args:Array[String]) {
    val dir = args(0)
    val reader = new Reader
    val output = reader.readDir(dir, CORENLP_PROCESSOR)
    val path = new File(dir).getAbsolutePath + ".rcache"
    val os = new ObjectOutputStream(new FileOutputStream(path))
    os.writeObject(output)
    os.close()
    println("Cache saved in file: " + path)
  }

  private def loadCache(path:String):List[(DiscourseTree, Document)] = {
    logger.debug("Attempting to load cached documents from: " + path)
    val is = new ClassLoaderObjectInputStream(DiscourseTree.getClass.getClassLoader, new FileInputStream(path))
    val output = is.readObject().asInstanceOf[List[(DiscourseTree, Document)]]
    is.close()
    output
  }

  def load(dir:String, proc:Processor):List[(DiscourseTree, Document)] = {
    var trees:List[(DiscourseTree, Document)] = null
    try {
      val path = new File(dir).getAbsolutePath + ".rcache"
      trees = CacheReader.loadCache(path)
      logger.info("Data loaded from cache: " + path)
    } catch {
      case e:Exception => {
        logger.debug("WARNING: Could not load documents from cache. Error was:")
        e.printStackTrace()
        logger.debug("Parsing documents online...")
        val reader = new Reader
        trees = reader.readDir(dir, proc)
        logger.debug(s"Found ${reader.tokenizationMistakes} tokenization mistakes for ${reader.totalTokens} tokens.")
      }
    }
    trees
  }
}

class CacheReader
