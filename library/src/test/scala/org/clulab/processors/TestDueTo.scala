package org.clulab.processors

import org.clulab.struct.GraphMap

class TestDueTo extends CluTest {
  // Tags for "due to" are trained "JJ TO" rather than "IN TO".

  val sentences = Seq(
    // sentences about Ghana
    "This was largely due to the impact of COVID-19 on production of cocoa beans and its purchases.",
    "Also, 773,378 metric tonnes of cocoa beans were sold, which was 9.01% below projected sales of 850,000 metric tonnes due to lower production volumes.",
    "He said in spite of bountiful harvests due to the efficiency of the ‘Planting for Food and Jobs’ initiative, external factors such as Covid-19 and the Russia-Ukraine war, have grossly affected the prices of food products on the local market.",
    "The return to the Central Bank’s medium term target band is due to the anticipation that headline inflation will resume its disinflationary path.",
    // TODO: The parser fails on the sentence below
    // "In the scenario where there is a delay of the disbursement of the $350m fifth tranche in May 2025 under the IMF programme as a result of Government failing to meet set targets for the $350m disbursement due to overspending during the 2024 elections – which is usually the case in election years – this is expected to lead to some level of depreciation in the cedi, adding to the inflationary pressures and contributing to an uptick in the country’s inflation rate.", 
    "The country’s yield curve is expected to remain inverted (negative sloping) in the medium term as yields on long term debts, due to the offering of new notes and bonds by Government under the domestic debt restructuring programme (DDEP) are less than yields on short term debt instruments like treasury bills.",
    "Yields on the new longer term debts are expected to remain below that of shorter term debts on the back of less trading activity in the new notes and bonds market primarily due to the flow of investor funds to the shorter term debt instruments that have higher yields.",
    "However, we expect the Pound to weaken further in the near term due to the economic strains the country is facing.",
    "Annual inflation eased slightly in November, slowing to 9.5% from 9.6% in October, mainly due to seasonal factors and falling international food prices.",
    "They made so much money that he was paying in dollars for his rent but due to poor management he was back to square one.",

    // training data for scala transformers
    "\"Due to health reasons, Lt. Col. Noriega has elected to postpone his visit to Washington,\" it read.",
    "Public protests against him were triggered in June 1987 due to charges by Diaz Herrera, his former chief of staff, that Mr. Noriega had stolen the 1984 election and had ordered the killing of Messrs. Spadafora and Torrijos.",
    "Some competing forest-products firms have recently reported improved results due to strong pulp and paper business.",
    "Manufacturers Hanover had a loss due to a big reserve addition.",
    "Amgen Inc. said its second-quarter earnings increased more than tenfold to $3.9 million, or 22 cents a share, due to increased sales of the company's new antianemia drug for kidney patients.",
    "In one ingenious recent example of a Newport Beach boiler room, prospective investors in Capital Trust Inc. were allegedly told that their investment in precious metals was insured against losses \"caused by employees due to dishonesty, destruction or disappearance,\" according to an indictment handed up by a federal grand jury in Los Angeles last month.",
    "Rorer Group Inc. will report that third-quarter profit rose more than 15% from a year earlier, though the gain is wholly due to asset sales, Robert Cawthorn, chairman, president and chief executive officer, said.",
    "Mr. Cawthorn said the profit growth in the latest quarter was due to the sale of two Rorer drugs.",
    "Although this widow earns only twice the minimum wage, largely due to the earnings limit, she would have to earn an additional $4,930 to offset her catastrophic surtax of $496.",
    "Past Colombian government tolerance of the \"narcotraficantes\" was due to the drug lords' history of wiping out leftists in the hinterlands." // ,

    // due X to
//    "As for joint ventures, Mr. Houghton said profit was \"essentially flat\" due primarily to a slow recovery at Samsung-Corning Co. in Korea following a strike at a major customer and the disruption of shipments to China.",
//    "Apple Computer Inc. posted improved fiscal fourth-quarter profit due largely to a $48 million gain on the sale of its stock in Adobe Systems Inc.",
//    "\"A loss of the warning symptoms of hypoglycemia is a complex problem that is very unlikely to be due simply to the type of insulin used,\" the American association said.",
//    "That is due mostly to payments from Allianz for most of the 50% stake it has agreed to acquire in Navigation Mixte's insurance business.",
//    "The economy's slowdown is due only partly to the austerity program launched in September 1988 to cool an overheated economy and tame inflation.",
//    "The increase was due mainly to a $325 million order from Turkey to equip its fleet of F-16 fighters with Loral's ALQ-178 Rapport III electronic countermeasures system.",
//    "But the Dow Jones Transportation Average went down for the seventh consecutive session, due largely to further selling in UAL.",
//    "Polish brewer Zywiec's 1996 profit slump may last into next year due in part to hefty depreciation charges, but recent high investment should help the firm defend its 10-percent market share, the firm's chief executive said."
  )

  behavior of "BalaurProcessor"

  def test(sentence: String): Unit = {
    it should sentence in {
      val expectedDueTag = "IN"
      val expectedToTag = "TO"
      val expectedDueToRelation = "mwe"

      val doc = this.proc.annotate(sentence)
      val sent = doc.sentences.head
      val lowerWords = sent.words.map(_.toLowerCase)
      val dueIndex = lowerWords.indexOf("due")
      val toIndex = lowerWords.indexOf("to", dueIndex + 1)

      dueIndex should be >= (0)
      toIndex should be > dueIndex

      val tags = sent.tags.get
      val actualDueTag = tags(dueIndex)
      val actualToTag = tags(toIndex)
      val edges = sent.graphs(GraphMap.UNIVERSAL_ENHANCED).edges
      val actualDueToRelationOpt = edges
          .find { edge =>
            edge.source == dueIndex && edge.destination == toIndex
          }
          .map(_.relation)

      actualDueTag should be (expectedDueTag)
      actualToTag should be (expectedToTag)
      actualDueToRelationOpt should be (Some(expectedDueToRelation))
    }
  }

  sentences.foreach(test)
}
