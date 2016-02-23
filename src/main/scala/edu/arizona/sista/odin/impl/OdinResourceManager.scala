package edu.arizona.sista.odin.impl

import java.io.InputStream

// other resources could go here
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

  // TODO: is this the proper way to get the real path?
  def getInputStream(p: String): InputStream = {
    println(s"Path to resources is $p")
    getClass.getClassLoader.getResourceAsStream(p)
  }

  // YOU NEED TO CLOSE ME!!!
  def getSource(path: String): io.Source = {
    val url = getClass.getClassLoader.getResource(path)
    if (url == null) io.Source.fromFile(path) else io.Source.fromURL(url)
  }

  def buildResources(resourcesMap: Map[String, String]): Map[String, Option[OdinResource]] = {
    val pairs = resourcesMap map {
      case (embeddings, p) if embeddings.toLowerCase startsWith "embeddings" =>
        val source = getSource(p)

//         // Make sure the file exists
//         if (is == null) {
//           throw new OdinCompileException(s"invalid path given for 'embeddings': $p")
//         }
        val pair = ("embeddings", Some(new EmbeddingsResource(source)))
        //source.close()
        pair
      }
      pairs.toMap.withDefaultValue(None)
    }
}


