package org.clulab.processors

import org.clulab.discourse.rstparser.RelationDirection
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.struct.DirectedGraphEdgeIterator
import org.scalatest._
import org.clulab.processors.fastnlp.FastNLPProcessor

/**
 *
 * User: mihais
 * Date: 1/7/14
 */
class TestFastNLPProcessor extends FlatSpec with Matchers {
  var proc:Processor = new FastNLPProcessor(internStrings = true, withDiscourse = ShallowNLPProcessor.WITH_DISCOURSE)

  "FastNLPProcessor" should "generate correct dependencies in test sentence 1" in {
    val doc = proc.annotate("John Smith went to China.")

    doc.sentences.head.dependencies.get.hasEdge(1, 0, "compound") should be (true)
    doc.sentences.head.dependencies.get.hasEdge(2, 1, "nsubj") should be (true)
    doc.sentences.head.dependencies.get.hasEdge(2, 4, "nmod_to") should be (true)

    /*
    val it = new DirectedGraphEdgeIterator[String](doc.sentences.head.dependencies.get)
    while(it.hasNext) {
      val d = it.next()
      println(d._1 + " " + d._2 + " " + d._3)
    }
    */
  }

  "FastNLPProcessor" should "generate correct dependencies in test sentence 2" in {
    val doc = proc.annotate("He bought some shoes.")

    //println(doc.sentences.head.dependencies)
    doc.sentences.head.dependencies.get.hasEdge(1, 0, "nsubj") should be (true)
    doc.sentences.head.dependencies.get.hasEdge(1, 3, "dobj") should be (true)
    doc.sentences.head.dependencies.get.hasEdge(1, 4, "punct") should be (true)
    doc.sentences.head.dependencies.get.hasEdge(3, 2, "det") should be (true)
  }

  "FastNLPProcessor" should "generate correct discourse relations in test 3" in {
    val doc = proc.annotate("John Smith went to China. He visited Beijing, on January 10th, 2013.")
    doc.clear()

    val d = doc.discourseTree.get
    d.relationLabel should be ("elaboration")
    d.relationDirection should be (RelationDirection.LeftToRight)
    d.isTerminal should be (false)
    d.children.length should be (2)
  }

  // For more information, see
  //   https://github.com/clulab/eidos/issues/261
  //   https://github.com/stanfordnlp/CoreNLP/issues/669
  //   https://github.com/stanfordnlp/CoreNLP/issues/83
  // This is fixed by props.put("maxAdditionalKnownLCWords", "0") in ShallowNLPProcessor.MkNer.
  "FastNLPProcessor" should "not have NER unaffected by state" in {
    val texts = Seq(
      "The highest potential areas for agricultural production are Western Equatoria and the southern half of Central Equatoria, or the so-called Green Belt, where annual rainfall ranges from 900 to 2,000 mm per year (Table 2.6). Rainfall in the Hills and Mountains region of the northern half of Central Equatoria and the western half of Eastern Equatoria (500 to 800 mm per year) is also sufficient to support substantial crop agriculture (WFP 2011).",

      "However, prospects for 2017 aggregate cereal production are generally unfavourable as agricultural activities continue to be severely affected by the protracted and widespread insecurity, which is constraining farmers' access to fields and is causing large scale displacement of people, input shortages and damage to households' productive assets. In the traditionally surplus-producing areas of southern Greater Equatoria Region, crop production is expected to be lower than the already poor 2016 output due to recent massive displacements outside the former Central and Eastern Equatoria states. Notably, about 75 percent of the population of the former Central Equatoria State has reportedly left their living areas.",
      "In the capital, Juba, prices of maize and sorghum more than doubled in the first semester of 2017, reaching record levels in June, driven by a tight supply situation, market disruptions, hyperinflation and a significant depreciation of the local currency. Subsequently, they declined by about 12 percent between June and August, following the first season harvest in southern bi-modal rainfall areas and the establishment, by the Government, of a trading company selling basic food commodities at subsidized prices. Prices of groundnuts decreased by 22 percent over the same period, while prices of wheat flour continued to soar in recent months, reaching new record highs in August. Overall, prices of these food staples in August were more than twice the high levels in August last year and up to 12 times higher than in the corresponding period two years earlier.",
      "According to the latest IPC analysis, famine conditions, previously reported in February 2017 in former Leer and Mayendit counties in former Unity State, were no longer occurring by late June 2017.Overall, the number of people facing IPC Phase 5: \"Catastrophe\" food security conditions declined from over 100 000 in February to about 45 000 in June due to sustained multi-sectoral humanitarian assistance operations and the two counties are currently classified as IPC Phase 4: \"Emergency\".",
      "Since the start of the conflict in mid-December 2013, about 3.9 million people were forced to flee their homes due to insecurity, including about 1.9 million IDPs and 2 million that sought refuge in neighbouring countries (Uganda, the Sudan, the Democratic Republic of the Congo, Ethiopia and Kenya).",
      "However, nationwide, the food insecure caseload (IPC Phases 3, 4 and 5) increased from about 5 million in February to a record high of 6 million in June as food access continues to be severely constrained by widespread insecurity, large scale displacements, high food prices, market disruptions, macro-economic collapse and exhaustion of households' coping mechanisms. The areas of major concern are Greater Jonglei and Unity states, where over 60 percent of the population faces \"Crisis\", \"Emergency\" and \"Catastrophe\" levels of food insecurity. In particular, the people facing catastrophic conditions are located in Ayod County in Greater Jonglei State and in Leer, Koch and Mayendit counties in Unity State.",
      "In southern bi-modal rainfall areas, harvesting of first season crops was concluded in August. Seasonal rains were above-average in the \"green belt\", including the former Central and Western Equatoria states, while in the former Eastern Equatoria State they started in late April with about a one-month delay. In northern and central uni-modal rainfall areas, harvesting of short cycle sorghum and maize crops has recently started, while long cycle sorghum crops will be gathered from November to January. Weather conditions have been generally favourable so far as seasonal rains have been average to above average, thus benefiting vegetation conditions.",
      "In addition, Fall Armyworm infestations have been reported in all regions of the country, with significant crop damage, especially in parts of former Northern Bahr el Ghazal, Eastern Equatoria and Central Equatoria states.",

      "The highest potential areas for agricultural production are Western Equatoria and the southern half of Central Equatoria, or the so-called Green Belt, where annual rainfall ranges from 900 to 2,000 mm per year (Table 2.6). Rainfall in the Hills and Mountains region of the northern half of Central Equatoria and the western half of Eastern Equatoria (500 to 800 mm per year) is also sufficient to support substantial crop agriculture (WFP 2011)."
    )

    def getEntitiesForWord(documents: Seq[Document], searchWord: String): Seq[String] = {
      val entities = for {
        doc <- documents
        sentence <- doc.sentences
        i <- sentence.words.indices
        if (sentence.words(i) == searchWord)
      }
      yield sentence.entities.get(i)

      entities
    }

    val docs = texts.map { proc.annotate(_) }
    val entities = getEntitiesForWord(docs, "Belt")

    entities.size >= 2 should be (true)
    entities.exists { entity => entity != entities.head } should be (false)
  }
}
