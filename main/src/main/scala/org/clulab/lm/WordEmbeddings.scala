package org.clulab.lm

import edu.cmu.dynet.{Dim, Expression, FloatVector, LookupParameter, ParameterCollection}
import org.clulab.embeddings.word2vec.CompactWord2Vec

class WordEmbeddings {
  val parameters = new ParameterCollection()
  val (lookupParameters, w2i) = mkLookupParams()

  def mkLookupParams(): (LookupParameter, Map[String, Int]) = {
    val wordVectors = CompactWord2Vec("glove300dByFreq10.txt", resource = false, cached = false)
    val w2i = wordVectors.keys.toList.sorted.zipWithIndex.toMap

    val wordLookupParameters = parameters.addLookupParameters(w2i.size, Dim(RnnLMTrain.WORD_EMBED_SIZE))

    for(word <- wordVectors.keys) {
      wordLookupParameters.initialize(w2i(word), new FloatVector(wordVectors.get(word).get))
    }
    println(s"Completed loading word embeddings, for ${w2i.size} words.")

    (wordLookupParameters, w2i)
  }

  def get(word:String): Expression = {
    if(w2i.contains(word)) {
      Expression.constLookup(lookupParameters, w2i(word))
    } else {
      assert(w2i.contains(""))
      Expression.constLookup(lookupParameters, w2i("")) // empty string for UNK
    }
  }
}
