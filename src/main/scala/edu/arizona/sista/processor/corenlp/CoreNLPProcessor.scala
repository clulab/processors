package edu.arizona.sista.processor.corenlp

import edu.arizona.sista.processor._
import edu.stanford.nlp.pipeline.{StanfordCoreNLP,Annotation}
import java.util.Properties
import collection.mutable.{ListBuffer, ArrayBuffer}
import edu.stanford.nlp.ling.CoreAnnotations._
import scala.collection.JavaConversions._
import edu.stanford.nlp.util.CoreMap
import edu.stanford.nlp.ling.CoreLabel
import java.util
import scala.Some
import edu.arizona.sista.utils.{MutableNumber, Tree, DirectedGraph}
import collection.mutable
import edu.stanford.nlp.dcoref.CorefChain
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefChainAnnotation
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation
import edu.stanford.nlp.trees.SemanticHeadFinder
import edu.stanford.nlp.semgraph.SemanticGraph
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations

/**
 * API for Stanford's CoreNLP tools
 * User: mihais
 * Date: 3/1/13
 */
class CoreNLPProcessor(val internStrings:Boolean = true) extends Processor {
  lazy val tokenizerWithoutSentenceSplitting = mkTokenizerWithoutSentenceSplitting
  lazy val tokenizerWithSentenceSplitting = mkTokenizerWithSentenceSplitting
  lazy val posTagger = mkPosTagger
  lazy val lemmatizer = mkLemmatizer
  lazy val ner = mkNer
  lazy val parser = mkParser
  lazy val coref = mkCoref
  lazy val headFinder = new SemanticHeadFinder()

  def mkTokenizerWithoutSentenceSplitting: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "tokenize")
    new StanfordCoreNLP(props)
  }

  def mkTokenizerWithSentenceSplitting: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "tokenize, ssplit")
    new StanfordCoreNLP(props)
  }

  def mkPosTagger: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "pos")
    new StanfordCoreNLP(props, false)
  }

  def mkLemmatizer: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "lemma")
    new StanfordCoreNLP(props, false)
  }

  def mkNer: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "ner")
    new StanfordCoreNLP(props, false)
  }

  def mkParser: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "parse")
    new StanfordCoreNLP(props, false)
  }

  def mkCoref: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "dcoref")
    new StanfordCoreNLP(props, false)
  }

  def mkDocument(text:String): Document = {
    val annotation = new Annotation(text)
    tokenizerWithSentenceSplitting.annotate(annotation)
    val sas = annotation.get(classOf[SentencesAnnotation])
    val sentences = new Array[Sentence](sas.size())
    var offset = 0
    for (sa <- sas) {
      sentences(offset) = mkSentence(sa)
      offset += 1
    }
    new CoreNLPDocument(sentences, Some(annotation))
  }

  def mkSentence(annotation:CoreMap): Sentence = {
    val tas = annotation.get(classOf[TokensAnnotation])

    val wordBuffer = new ArrayBuffer[String]
    val startOffsetBuffer = new ArrayBuffer[Int]
    val endOffsetBuffer = new ArrayBuffer[Int]

    for (ta <- tas) {
      wordBuffer.add(in(ta.word))
      startOffsetBuffer += ta.beginPosition()
      endOffsetBuffer += ta.endPosition()
    }

    new Sentence(
      wordBuffer.toArray,
      startOffsetBuffer.toArray,
      endOffsetBuffer.toArray)
  }

  def in(s:String):String = {
    if (internStrings) Processor.in.intern(s)
    else s
  }

  def arrayOrNone[T: ClassManifest](b:ArrayBuffer[T]): Option[Array[T]] = {
    if (b.size > 0) new Some[Array[T]](b.toArray)
    else None
  }

  def mkDocumentFromSentences(sentences:Iterable[String],
                              charactersBetweenSentences:Int = 1): Document = {
    val docAnnotation = new Annotation(sentences.mkString(mkSep(charactersBetweenSentences)))
    val sentencesAnnotation = new util.ArrayList[CoreMap]()
    docAnnotation.set(classOf[SentencesAnnotation], sentencesAnnotation.asInstanceOf[java.util.List[CoreMap]])
    val docSents = new Array[Sentence](sentences.size)

    var sentOffset = 0
    var charOffset = 0
    var tokenOffset = 0
    for(sentence <- sentences) {
      val tmpAnnotation = new Annotation(sentence)
      tokenizerWithoutSentenceSplitting.annotate(tmpAnnotation)
      val crtTokens:java.util.List[CoreLabel] =
        tmpAnnotation.get(classOf[TokensAnnotation])

      // construct a proper sentence, with token and character offsets adjusted to make the entire document consistent
      val crtSent = new Annotation(sentence)
      crtSent.set(classOf[TokensAnnotation], crtTokens)
      crtSent.set(classOf[TokenBeginAnnotation], new Integer(tokenOffset))
      tokenOffset += crtTokens.size()
      crtSent.set(classOf[TokenEndAnnotation], new Integer(tokenOffset))
      var i = 0
      while(i < crtTokens.size()) {
        val crtTok = crtTokens.get(i)
        crtTok.setBeginPosition(crtTok.beginPosition() + charOffset)
        crtTok.setEndPosition(crtTok.endPosition() + charOffset)
        i += 1
      }

      sentencesAnnotation.add(crtSent)
      docSents(sentOffset) = mkSentence(crtSent)

      charOffset += sentence.length + charactersBetweenSentences
      sentOffset += 1
    }

    new CoreNLPDocument(docSents, Some(docAnnotation))
  }

  private def mkSep(size:Int):String = {
    val os = new mutable.StringBuilder
    for (i <- 0 until size) os.append(" ")
    os.toString()
  }

  def mkDocumentFromTokens(sentences:Iterable[Iterable[String]],
                           charactersBetweenSentences:Int = 1,
                           charactersBetweenTokens:Int = 1): Document = {
    val sb = new ListBuffer[String]
    for (s <- sentences)
      sb += s.mkString(mkSep(charactersBetweenTokens))
    val sentenceTexts = sb.toArray
    val docAnnotation = new Annotation(sentenceTexts.mkString(mkSep(charactersBetweenSentences)))
    val sentencesAnnotation = new util.ArrayList[CoreMap]()
    docAnnotation.set(classOf[SentencesAnnotation], sentencesAnnotation.asInstanceOf[java.util.List[CoreMap]])
    val docSents = new Array[Sentence](sentences.size)

    var sentOffset = 0
    var charOffset = 0
    var tokenOffset = 0
    for(sentence <- sentences) {
      val crtTokens:util.List[CoreLabel] = new util.ArrayList[CoreLabel]()
      var tokOffset = 0
      for (w <- sentence) {
        val crtTok = new CoreLabel()
        crtTok.setWord(w)
        crtTok.setBeginPosition(charOffset)
        charOffset += w.length
        crtTok.setEndPosition(charOffset)
        crtTokens.add(crtTok)
        tokOffset += 1
        charOffset += charactersBetweenTokens
      }

      val crtSent = new Annotation(sentenceTexts(sentOffset))
      crtSent.set(classOf[TokensAnnotation], crtTokens)
      crtSent.set(classOf[TokenBeginAnnotation], new Integer(tokenOffset))
      tokenOffset += crtTokens.size()
      crtSent.set(classOf[TokenEndAnnotation], new Integer(tokenOffset))

      sentencesAnnotation.add(crtSent)
      docSents(sentOffset) = mkSentence(crtSent)
      sentOffset += 1
    }

    new CoreNLPDocument(docSents, Some(docAnnotation))
  }

  def basicSanityCheck(doc:Document): Option[Annotation] = {
    if (doc.sentences == null)
      throw new RuntimeException("ERROR: Document.sentences == null!")
    if (doc.sentences.size == 0) return None
    if (doc.sentences(0).words == null)
      throw new RuntimeException("ERROR: Sentence.words == null!")
    val annotation = doc.asInstanceOf[CoreNLPDocument].annotation.getOrElse(
      throw new RuntimeException("ERROR: annotator called after Document.clear()!"))
    Some(annotation)
  }

  def tagPartsOfSpeech(doc:Document) {
    val annotation = basicSanityCheck(doc)
    if (annotation.isEmpty) return

    posTagger.annotate(annotation.get)

    // convert CoreNLP Annotations to our data structures
    val sas = annotation.get.get(classOf[SentencesAnnotation])
    var offset = 0
    for (sa <- sas) {
      val tb = new ArrayBuffer[String]
      val tas = sa.get(classOf[TokensAnnotation])
      for (ta <- tas) {
        tb += in(ta.tag())
      }
      doc.sentences(offset).tags = Some(tb.toArray)
      offset += 1
    }
  }

  def lemmatize(doc:Document) {
    val annotation = basicSanityCheck(doc)
    if (annotation.isEmpty) return
    if (doc.sentences.head.tags == None)
      throw new RuntimeException("ERROR: you have to run the POS tagger before lemmatization!")

    lemmatizer.annotate(annotation.get)

    val sas = annotation.get.get(classOf[SentencesAnnotation])
    var offset = 0
    for (sa <- sas) {
      val tb = new ArrayBuffer[String]
      val tas = sa.get(classOf[TokensAnnotation])
      for (ta <- tas) {
        tb += in(ta.lemma())
      }
      doc.sentences(offset).lemmas = Some(tb.toArray)
      offset += 1
    }
  }

  def recognizeNamedEntities(doc:Document) {
    val annotation = basicSanityCheck(doc)
    if (annotation.isEmpty) return
    if (doc.sentences.head.tags == None)
      throw new RuntimeException("ERROR: you have to run the POS tagger before NER!")
    if (doc.sentences.head.lemmas == None)
      throw new RuntimeException("ERROR: you have to run the lemmatizer before NER!")

    ner.annotate(annotation.get)

    // convert CoreNLP Annotations to our data structures
    val sas = annotation.get.get(classOf[SentencesAnnotation])
    var offset = 0
    for (sa <- sas) {
      val tb = new ArrayBuffer[String]
      val nb = new ArrayBuffer[String]
      val tas = sa.get(classOf[TokensAnnotation])
      for (ta <- tas) {
        tb += in(ta.ner())
        val n = ta.get(classOf[NormalizedNamedEntityTagAnnotation])
        if (n != null) nb += in(n)
        else nb += in("O")
      }
      doc.sentences(offset).entities = Some(tb.toArray)
      doc.sentences(offset).norms = Some(nb.toArray)
      offset += 1
    }
  }

  def parse(doc:Document) {
    val annotation = basicSanityCheck(doc)
    if (annotation.isEmpty) return

    parser.annotate(annotation.get)

    val sas = annotation.get.get(classOf[SentencesAnnotation])
    var offset = 0
    for (sa <- sas) {
      //
      // save the constituent tree, including head word information
      //
      val stanfordTree = sa.get(classOf[TreeAnnotation])
      if (stanfordTree != null) {
        val position = new MutableNumber[Int](0)
        doc.sentences(offset).syntacticTree = Some(toTree(stanfordTree, position))
      }

      //
      // save syntactic dependencies
      //
      val edgeBuffer = new ListBuffer[(Int, Int, String)]
      val da = sa.get(classOf[SemanticGraphCoreAnnotations.CollapsedCCProcessedDependenciesAnnotation])
      val edges = da.getEdgeSet
      for (edge <- edges) {
        val head:Int = edge.getGovernor.get(classOf[IndexAnnotation])
        val modifier:Int = edge.getDependent.get(classOf[IndexAnnotation])
        var label = edge.getRelation.getShortName
        val spec = edge.getRelation.getSpecific
        if (spec != null) label = label + "_" + spec
        edgeBuffer.add((head - 1, modifier - 1, in(label)))
      }

      val roots = new mutable.HashSet[Int]
      for (iw <- da.getRoots) {
        roots.add(iw.get(classOf[IndexAnnotation]) - 1)
      }

      val dg = new DirectedGraph[String](edgeBuffer.toList, roots.toSet)
      //println(dg)
      doc.sentences(offset).dependencies = Some(dg)
      offset += 1
    }
  }

  private def toTree(
    stanfordTree:edu.stanford.nlp.trees.Tree,
    position:MutableNumber[Int]):Tree[String] = {
    assert(stanfordTree != null)

    if (stanfordTree.isLeaf) {
      val tree = new Tree[String](
        stanfordTree.label.value(),
        None, 0,
        position.value,
        position.value + 1)
      position.value += 1
      return tree
    }

    // println("Converting tree: " + stanfordTree.toString)
    val children = new Array[Tree[String]](stanfordTree.numChildren())
    for (i <- 0 until stanfordTree.numChildren()) {
      children(i) = toTree(stanfordTree.getChild(i), position)
    }
    val value = stanfordTree.label.value()
    val start = children(0).startOffset
    val end = children(children.length - 1).endOffset

    val headDaughter = headFinder.determineHead(stanfordTree)
    var head = -1
    var i = 0
    while(i < stanfordTree.numChildren() && head == -1) {
      if (headDaughter == stanfordTree.getChild(i)) {
        head = i
      }
      i += 1
    }

    new Tree[String](value, Some(children), head, start, end)
  }

  def chunking(doc:Document) {
    // CoreNLP does not have shallow parsing
  }

  def labelSemanticRoles(doc:Document) {
    // CoreNLP does not have SRL
  }

  def resolveCoreference(doc:Document) {
    val annotation = basicSanityCheck(doc)
    if (annotation.isEmpty) return

    if (doc.sentences.head.tags == None)
      throw new RuntimeException("ERROR: you have to run the POS tagger before coreference resolution!")
    if (doc.sentences.head.lemmas == None)
      throw new RuntimeException("ERROR: you have to run the lemmatizer before coreference resolution!")
    if (doc.sentences.head.entities == None)
      throw new RuntimeException("ERROR: you have to run the NER before coreference resolution!")
    if(doc.sentences.head.dependencies == None)
      throw new RuntimeException("ERROR: you have to run the parser before coreference resolution!")

    coref.annotate(annotation.get)

    val chains = annotation.get.get(classOf[CorefChainAnnotation])
    val mentions = new ListBuffer[CorefMention]

    for (cid <- chains.keySet()) {
      // println("cluster " + cid)
      val mentionMap = chains.get(cid).getMentionMap
      for (mid <- mentionMap.keySet()) {
        for (mention <- mentionMap.get(mid)) {
          // val isRep = mention == cluster.getRepresentativeMention
          // println("\tmention " + mid.getSource + " " + mid.getTarget + " " + mention.startIndex + " " + mention.endIndex + " " + isRep + " [" + mention.mentionSpan + "]")

          // Processor indexes things from 0 not 1!
          val m = new CorefMention(
            mid.getSource - 1,
            mid.getTarget - 1,
            mention.startIndex - 1,
            mention.endIndex - 1,
            cid)
          mentions += m
        }
      }
    }

    doc.coreferenceChains = Some(new CorefChains(mentions.toList))
  }
}
