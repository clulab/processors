package org.clulab.numeric

import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.clulab.struct.Interval
import org.scalatest.{FlatSpec, Matchers}

class TestNumericEntityRecognition extends FlatSpec with Matchers {
  Utils.initializeDyNet()
  val ner = new NumericEntityRecognizer
  val proc = new CluProcessor()

  //
  // unit tests starts here
  //

  // these should be captured by rules date-1 and date-2
  "the numeric entity recognizer" should "recognize dates in the European format" in {
    ensure("It is 12 May, 2000", Interval(2, 6), "DATE", "2000-05-12")
    ensure("It was May 2000", Interval(2, 4), "DATE", "2000-05-XX")
    ensure("It was 25 May", Interval(2, 4), "DATE", "XXXX-05-25")
  }

  // these should be captured by rules date-3 and date-4
   it should "recognize dates in the American format" in {
     ensure("It is 2000, May 12", Interval(2, 6), "DATE", "2000-05-12")
     ensure("It was May 31", Interval(2, 4), "DATE", "XXXX-05-31")
     ensure("It was 2000", Interval(2,3), "DATE", "2000-XX-XX")
     ensure("It was 2000, May", Interval(2, 5), "DATE", "2000-05-XX")
   }

  it should "recognize numeric dates" in {
    // these should be captured by rule date-yyyy-mm-dd
    ensure("It is 2000:05:12", Interval(2, 3), "DATE", "2000-05-12")
    ensure("It is 2000/05/12", Interval(2, 3), "DATE", "2000-05-12")
    ensure("It is 2000-05-12", Interval(2, 3), "DATE", "2000-05-12")

    // these should be captured by rule date-dd-mm-yyyy
    ensure("It is 12/05/2000", Interval(2, 3), "DATE", "2000-05-12")
    ensure("It is 12:05:2000", Interval(2, 3), "DATE", "2000-05-12")
    ensure("It is 12-05-2000", Interval(2, 3), "DATE", "2000-05-12")
  }

  it should "recognize numeric dates 2" in {
    // these tests should be captured by yyyy-mm-dd
    ensure(sentence= "ISO date is 1988-02-17.", Interval(3, 4), goldEntity= "DATE", goldNorm= "1988-02-17")
    ensure(sentence= "1988-02-17.", Interval(0, 1), goldEntity= "DATE", goldNorm= "1988-02-17")
    ensure(sentence= "1988/02/17.", Interval(0, 1), goldEntity= "DATE", goldNorm= "1988-02-17")

    // Any confusion between European and American date format. We go with American one.
    ensure(sentence= "ISO date is 1988-02-03.", Interval(3, 4), goldEntity= "DATE", goldNorm= "1988-02-03")
    ensure(sentence= "ISO date is 1988/02/03.", Interval(3, 4), goldEntity= "DATE", goldNorm= "1988-02-03")
    ensure(sentence= "1988/02/03.", Interval(0, 1), goldEntity= "DATE", goldNorm= "1988-02-03")

  }

  it should "recognize numeric dates of form yy-mm-dd" in  {
    ensure(sentence= "88/02/15.", Interval(0, 1), goldEntity= "DATE", goldNorm= "XX88-02-15")
    ensure(sentence= "ISO date is 88/02/15.", Interval(3, 4), goldEntity= "DATE", goldNorm= "XX88-02-15")
  }

  it should "recognize numeric dates of form mm-yyyy" in  {
    // These tests should be captured by rule mm-yyyy
    ensure(sentence= "02-1988.", Interval(0, 1), goldEntity= "DATE", goldNorm= "1988-02-XX")
    ensure(sentence= "ISO date is 02/1988.", Interval(3, 4), goldEntity= "DATE", goldNorm= "1988-02-XX")
    ensure(sentence= "02/1988.", Interval(0, 1), goldEntity= "DATE", goldNorm= "1988-02-XX")
    ensure(sentence= "ISO date is 02/1988.", Interval(3, 4), goldEntity= "DATE", goldNorm= "1988-02-XX")
    ensure(sentence= "02/1988.", Interval(0, 1), goldEntity= "DATE", goldNorm= "1988-02-XX")
  }

  it should "recognize numeric dates of form yyyy-mm" in {
    // These tests are captured by rule yyyy-mm
    ensure(sentence= "ISO date is 1988-02.", Interval(3, 4), goldEntity= "DATE", goldNorm= "1988-02-XX")
    ensure(sentence= "1988-02.", Interval(0, 1), goldEntity= "DATE", goldNorm= "1988-02-XX")
    ensure(sentence= "ISO date is 1988/02.", Interval(3, 4), goldEntity= "DATE", goldNorm= "1988-02-XX")
    ensure(sentence= "1988/02.", Interval(0, 1), goldEntity= "DATE", goldNorm= "1988-02-XX")
  }

  it should "recognize numeric dates of form yy-mm" in {
    ensure(sentence= "19:02.", Interval(0, 1), goldEntity= "DATE", goldNorm= "XX19-02-XX")
  }

  it should "recognize date ranges" in {
    ensure("between 2020/10/10 and 2020/11/11", Interval(0, 4), "DATE-RANGE", "2020-10-10 - 2020-11-11")
    ensure("from July 20 to July 31", Interval(0, 6), "DATE-RANGE", "XXXX-07-20 - XXXX-07-31")
    ensure("from 20 to July 31", Interval(0, 5), "DATE-RANGE", "XXXX-07-20 - XXXX-07-31")
  }

  it should "recognize measurement units" in {
    ensure("It was 12 ha", Interval(2, 4), "MEASUREMENT", "12.0 ha")

    // TODO: need to implement UnitNormalizer.norm (Mihai)
    ensure("It was 12 hectares", Interval(2, 4), "MEASUREMENT", "12.0 ha")

    // TODO: need to implement a number grammar (Marco)
    ensure("It was twelve hundred ha", Interval(2, 5), "MEASUREMENT", "1200.0 ha")
    ensure("It was 12 hundred ha", Interval(2, 5), "MEASUREMENT", "1200.0 ha")

  }

  //
  // End unit tests for date recognition.
  //

  //
  // Helper methods below this point
  //

  /** Makes sure that the given span has the right entity labels and norms */
  def ensure(sentence: String,
             span: Interval,
             goldEntity: String,
             goldNorm: String): Unit = {
    val (words, entities, norms) = numericParse(sentence)

    println("Verifying the following text:")
    println("Words:    " + words.mkString(", "))
    println("Entities: " + entities.mkString(", "))
    println("Norms:    " + norms.mkString(", "))

    var first = true
    for(i <- span.indices) {
      val prefix = if(first) "B-" else "I-"
      val label = prefix + goldEntity

      entities(i) should be (label)
      norms(i) should be (goldNorm)

      first = false
    }
  }

  /** Runs the actual numeric entity recognizer */
  def numericParse(sentence: String): (Array[String], Array[String], Array[String]) = {
    val doc = proc.annotate(sentence)
    val mentions = ner.extractFrom(doc)
    setLabelsAndNorms(doc, mentions)

    // assume 1 sentence per doc
    val sent = doc.sentences.head
    Tuple3(sent.words, sent.entities.get, sent.norms.get)
  }
}
