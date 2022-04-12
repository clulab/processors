package org.clulab.odin.impl

import java.io.{BufferedInputStream, InputStream}
import scala.io.Source

/**
 * Manage resources for Odin
 * @param embeddings: handles a word embeddings resource for distributional similarity comparisons; standard word vector format?
 * Author: Gus Hahn-Powell.
 * Last Modified: Fix compiler issue: import scala.io.Source.
 */
class OdinResourceManager(val embeddings: Option[EmbeddingsResource])

object OdinResourceManager {

  def apply: OdinResourceManager = new OdinResourceManager(None)

  def apply(resources: Map[String, String]): OdinResourceManager = {
    val constructorMap: Map[String, Option[OdinResource]] = buildResources(resources)

    // get the embeddings entry
    val embeddingsOption: Option[OdinResource] = constructorMap("embeddings")
    // cast as EmbeddingsResources, if present
    val embeddings: Option[EmbeddingsResource] =
      if (embeddingsOption.nonEmpty) Some(embeddingsOption.get.asInstanceOf[EmbeddingsResource])
      else None
    new OdinResourceManager(embeddings)
  }

  def getInputStream(p: String): BufferedInputStream = {
    //println(s"Path to resources is $p")
    val url = RuleReader.mkURL(p)
    val streamFromResources: InputStream = url.openStream()
    new BufferedInputStream(streamFromResources)
  }

  // YOU NEED TO CLOSE ME!!!
  def getSource(path: String): Source = {
    val url = RuleReader.mkURL(path)
    Source.fromURL(url)
  }

  def buildResources(resourcesMap: Map[String, String]): Map[String, Option[OdinResource]] = {
    val pairs = resourcesMap map {
      case (embeddings, p) if embeddings.toLowerCase startsWith "embeddings" =>
        //val source = getSource(p)
        val is = getInputStream(p)

        // Make sure the file exists
        if (is == null) {
           throw new OdinCompileException(s"invalid path given for 'embeddings': $p")
        }
        //val pair = ("embeddings", Some(new EmbeddingsResource(source)))
        val pair = ("embeddings", Some(new EmbeddingsResource(is)))
        //source.close()
        pair
      }
      pairs.toMap.withDefaultValue(None)
    }
}


