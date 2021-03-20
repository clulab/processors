package org.clulab.processors

import org.clulab.dynet.Utils
import org.clulab.processors.clucore.CluCoreProcessor
import org.clulab.sequences.LexiconNER
import org.clulab.struct.TrueEntityValidator
import org.scalatest.{FlatSpec, Matchers}

class TestCluCoreProcessor extends FlatSpec with Matchers {
  val proc = new CluCoreProcessor()

  val kbs = Seq(
    "org/clulab/processors/CLUA.tsv",
    "org/clulab/processors/CLUB.tsv"
  )
  val lexiconNer = LexiconNER(kbs, Seq(false, true), new TrueEntityValidator, false) // case sensitive match on the first KB, case insensitive on the second
  val procWithOptNer = new CluCoreProcessor(optionalNER = Some(lexiconNer))

  Utils.initializeDyNet()

  "CluCoreProcessor" should "recognize everything :)" in {
    val doc = proc.annotate("John Doe visited China, on Jan. 1st, 1997, where he worked for Google Corp. for $5 per hour.")

    /*
    Named entities: B-PER I-PER O B-LOC O O B-DATE I-DATE I-DATE I-DATE O O O O O B-ORG I-ORG O B-MONEY I-MONEY O B-DURATION O
    Normalized entities:       1997-01-01 1997-01-01 1997-01-01 1997-01-01         $5.0 $5.0  PT1H
    */

    val ents = doc.sentences.head.entities.get
    val norms = doc.sentences.head.norms.get

    ents(0) should be ("B-PER")
    ents(1) should be ("I-PER")
    ents(3) should be ("B-LOC")
    ents(6) should be ("B-DATE")
    ents(7) should be ("I-DATE")
    ents(8) should be ("I-DATE")
    ents(9) should be ("I-DATE")
    ents(15) should be ("B-ORG")
    ents(16) should be ("I-ORG")
    ents(18) should be ("B-MONEY")
    ents(19) should be ("I-MONEY")
    ents(21) should be ("B-DURATION")

    norms(6) should be ("1997-01-01")
    norms(18) should be ("$5.0")
    norms(21) should be ("PT1H")

    /*
    Enhanced syntactic dependencies:
    head:1 modifier:0 label:compound
    head:2 modifier:1 label:nsubj
    head:2 modifier:3 label:dobj
    head:2 modifier:4 label:punct
    head:2 modifier:7 label:nmod_on
    head:2 modifier:22 label:punct
    head:6 modifier:8 label:punct
    head:6 modifier:10 label:punct
    head:6 modifier:13 label:acl:relcl
    head:7 modifier:5 label:case
    head:7 modifier:6 label:compound
    head:7 modifier:9 label:nummod
    head:13 modifier:11 label:advmod
    head:13 modifier:12 label:nsubj
    head:13 modifier:16 label:nmod_for
    head:13 modifier:19 label:nmod_for
    head:16 modifier:14 label:case
    head:16 modifier:15 label:compound
    head:19 modifier:17 label:case
    head:19 modifier:18 label:dep
    head:19 modifier:21 label:nmod_per
    head:21 modifier:20 label:case
     */

    val deps = doc.sentences.head.universalEnhancedDependencies.get
    deps.hasEdge(2, 1, "nsubj")
    deps.hasEdge(2, 3, "dobj")
    deps.hasEdge(13, 12, "nsubj")
    deps.hasEdge(13, 16, "nmod_for")

    /*
    Enhanced semantic dependencies:
    head:2 modifier:1 label:A0
    head:2 modifier:3 label:A1
    head:2 modifier:7 label:AM-TMP
    head:13 modifier:9 label:AM-TMP
    head:13 modifier:12 label:A0
    head:13 modifier:16 label:Ax_for
    head:13 modifier:19 label:Ax_for
     */

    val roles = doc.sentences.head.enhancedSemanticRoles.get
    roles.hasEdge(2, 1, "A0")
    roles.hasEdge(2, 3, "A1")
    roles.hasEdge(2, 7, "AM-TMP")
    roles.hasEdge(13, 9, "AM-TMP")
    roles.hasEdge(13, 12, "A0")
    roles.hasEdge(13, 16, "Ax_for")
    roles.hasEdge(13, 19, "Ax_for")
  }

  it should "recognize custom NE labels from the optional lexiconNer" in {
    val doc = procWithOptNer.annotate("John Doe A A a a b b C C")
    val ents = doc.sentences.head.entities.get

    println("Entities with the optional NER: " + ents.mkString(", "))

    ents(0) should be ("B-PER")
    ents(1) should be ("I-PER")
    ents(2) should be ("B-CLUA")
    ents(3) should be ("I-CLUA")
    ents(4) should be ("O")
    ents(5) should be ("O")
    ents(6) should be ("B-CLUB")
    ents(7) should be ("I-CLUB")
    ents(8) should be ("B-CLUB")
    ents(9) should be ("I-CLUB")
  }
}

