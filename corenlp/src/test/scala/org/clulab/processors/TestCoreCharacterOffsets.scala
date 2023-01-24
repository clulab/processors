package org.clulab.processors

import edu.stanford.nlp.ling.CoreAnnotations.{SentencesAnnotation, TextAnnotation, TokensAnnotation}
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.util.CoreMap
import org.clulab.TestUtils
import org.clulab.processors.corenlp.CoreNLPDocument
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.utils.Test

import scala.collection.mutable
import scala.jdk.CollectionConverters._

class TestCoreCharacterOffsets extends Test {
  val proc = new ShallowNLPProcessor()

  "the tokenizer" should "generate correct character offsets in a single sentence" in {
    val doc = proc.mkDocument("John Doe went to China on January 15th, 2001.", keepText = true)

    val docAnnotation = doc.asInstanceOf[CoreNLPDocument].annotation.get
    val sents = docAnnotation.get(classOf[SentencesAnnotation]).asScala
    val text = docAnnotation.get(classOf[TextAnnotation])
    for(s <- sents) {
      val tokens = s.get(classOf[TokensAnnotation]).asScala
      for(token <- tokens) {
        val word = token.word()
        val wordFromOffsets = text.substring(token.beginPosition(), token.endPosition())
        if(! isPostProcessed(word, wordFromOffsets)) {
          withClue(s"beginPosition: ${token.beginPosition()} endPosition: ${token.endPosition()}") {
            word should equal(wordFromOffsets)
          }
        }
      }
    }
  }

  it should "generate correct character offsets in multiple sentences" in {
    val doc = proc.mkDocumentFromSentences(List("John Doe went to China.", "There, he visited Beijing."), keepText = true)

    val docAnnotation = doc.asInstanceOf[CoreNLPDocument].annotation.get
    val sents = docAnnotation.get(classOf[SentencesAnnotation]).asScala
    val text = docAnnotation.get(classOf[TextAnnotation])
    for(s <- sents) {
      val tokens = s.get(classOf[TokensAnnotation]).asScala
      for(token <- tokens) {
        val word = token.word()
        val wordFromOffsets = text.substring(token.beginPosition(), token.endPosition())
        if(! isPostProcessed(word, wordFromOffsets)) {
          word should equal(wordFromOffsets)
        }
      }
    }
  }

  it should "generate correct character offsets in a file" in {
    var text = TestUtils.readFile("org/clulab/processors/raw_text.txt")
    val doc = proc.mkDocument(text, keepText = true)

    val docAnnotation = doc.asInstanceOf[CoreNLPDocument].annotation.get
    val sents = docAnnotation.get(classOf[SentencesAnnotation]).asScala
    text = docAnnotation.get(classOf[TextAnnotation])
    for(s <- sents) {
      val tokens = s.get(classOf[TokensAnnotation]).asScala
      for(token <- tokens) {
        val word = token.word()
        val wordFromOffsets = text.substring(token.beginPosition(), token.endPosition())

        if(! isPostProcessed(word, wordFromOffsets)) {
          word should equal(wordFromOffsets)
          //if(! word.equals(wordFromOffsets)) println(s"Expected $word, found $wordFromOffsets")
        }
      }
    }
  }

  val postProcessedWords = Set("-LRB-", "-RRB-", "``", "''")
  // The sample text has some contractions, all with 've.
  val postProcessedWordsFromOffsets = Set("n't", "'ll", "'ve", "'re")

  /**
    * CoreLabel.word() stores the postprocessed .word from Sentence, where the tokens below do not match the text anymore
    */
  private def isPostProcessed(word: String, wordFromOffsets: String): Boolean =
      postProcessedWords(word) || postProcessedWordsFromOffsets(wordFromOffsets)
}