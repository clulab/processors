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
//
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
//
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
    ensure(sentence= "April 1999.", Interval(0, 2), goldEntity= "DATE", goldNorm= "1999-04-XX")
  }

  it should "recognize numeric dates of form yyyy-mm" in {
    // These tests are captured by rule yyyy-mm
    ensure(sentence= "ISO date is 1988-02.", Interval(3, 4), goldEntity= "DATE", goldNorm= "1988-02-XX")
    ensure(sentence= "1988-02.", Interval(0, 1), goldEntity= "DATE", goldNorm= "1988-02-XX")
    ensure(sentence= "ISO date is 1988/02.", Interval(3, 4), goldEntity= "DATE", goldNorm= "1988-02-XX")
    ensure(sentence= "1988/02.", Interval(0, 1), goldEntity= "DATE", goldNorm= "1988-02-XX")
  }

  it should "recognize intensive SUTimes tests without a year" in {
    ensure(sentence= "Sun Apr 21", Interval(1,3), goldEntity= "DATE", goldNorm= "XXXX-04-21")
    ensure(sentence= "Sun Apr 24", Interval(1,3), goldEntity= "DATE", goldNorm= "XXXX-04-24")
    ensure(sentence= "Sun Apr 26", Interval(1,3), goldEntity= "DATE", goldNorm= "XXXX-04-26")
    ensure(sentence= "Wed May 1", Interval(1,3), goldEntity= "DATE", goldNorm= "XXXX-05-01")
    ensure(sentence= "Wed May 3", Interval(1,3), goldEntity= "DATE", goldNorm= "XXXX-05-03")
    ensure(sentence= "Wed May 5", Interval(1,3), goldEntity= "DATE", goldNorm= "XXXX-05-05")
    ensure(sentence= "Wed May 10", Interval(1,3), goldEntity= "DATE", goldNorm= "XXXX-05-10")
    ensure(sentence= "Fri May 11", Interval(1,3), goldEntity= "DATE", goldNorm= "XXXX-05-11")
    ensure(sentence= "Mon May 15", Interval(1,3), goldEntity= "DATE", goldNorm= "XXXX-05-15")
    ensure(sentence= "Wed May 18", Interval(1,3), goldEntity= "DATE", goldNorm= "XXXX-05-18")
    ensure(sentence= "Thur May 22", Interval(1,3), goldEntity= "DATE", goldNorm= "XXXX-05-22")
    ensure(sentence= "Mon May 27", Interval(1,3), goldEntity= "DATE", goldNorm= "XXXX-05-27")
    ensure(sentence= "Tue May 31", Interval(1,3), goldEntity= "DATE", goldNorm= "XXXX-05-31")
    ensure(sentence= "Mon Jun 3", Interval(1,3), goldEntity= "DATE", goldNorm= "XXXX-06-03")
    ensure(sentence= "Jun 8", Interval(0,2), goldEntity= "DATE", goldNorm= "XXXX-06-08")
    ensure(sentence= "Jun 18", Interval(0,2), goldEntity= "DATE", goldNorm= "XXXX-06-18")
    ensure(sentence= "Jun 18 2018", Interval(0,2), goldEntity= "DATE", goldNorm= "2018-06-18")
    ensure(sentence= "2018 Jun 18", Interval(0,2), goldEntity= "DATE", goldNorm= "2018-06-18")
  }

  it should "recognize numeric SUTimes tests" in {
    ensure(sentence= "2010-11-15", Interval(0,1), goldEntity= "DATE", goldNorm= "2010-11-15")
    ensure(sentence= "2010-11-16", Interval(0,1), goldEntity= "DATE", goldNorm= "2010-11-16")
    ensure(sentence= "2010-11-17", Interval(0,1), goldEntity= "DATE", goldNorm= "2010-11-17")
    ensure(sentence= "2010/11/18", Interval(0,1), goldEntity= "DATE", goldNorm= "2010-11-18")
    //TODO "1988-SP", "1988-SU", "1988-FA", "1988-FA", "1988-WI" we can extend this to capture this or SV cropping seasons
    //TODO "1988-02", "1988-Q2" needs Mihai approval
    ensure(sentence= "Cropping season starts on 2010-07", Interval(4,5), goldEntity= "DATE", goldNorm= "2010-07-XX")
    ensure(sentence= "It is 2010-08", Interval(2,3), goldEntity= "DATE", goldNorm= "2010-08-XX")
    ensure(sentence= "2010-10", Interval(0,1), goldEntity= "DATE", goldNorm= "2010-10-XX")
    ensure(sentence= "2010-12", Interval(0,1), goldEntity= "DATE", goldNorm= "2010-12-XX")
  }

  it should "recognize numeric dates of form yy-mm" in {
    ensure(sentence= "19:02.", Interval(0, 1), goldEntity= "DATE", goldNorm= "XX19-02-XX")
  }

  it should "recognize dates with ordinal days" in {
      ensure(sentence = "Planting dates are between July 1st and August 2nd.", Interval(3, 9), goldEntity = "DATE-RANGE", "XXXX-07-01 -- XXXX-08-02")
  }

  it should "recognize date ranges" in {
    ensure("between 2020/10/10 and 2020/11/11", Interval(0, 4), "DATE-RANGE", "2020-10-10 -- 2020-11-11")
    ensure("from July 20 to July 31", Interval(0, 6), "DATE-RANGE", "XXXX-07-20 -- XXXX-07-31")
    ensure("from 20 to July 31", Interval(0, 5), "DATE-RANGE", "XXXX-07-20 -- XXXX-07-31")
    //TODO : below all were new dote format found in habitus docs. add on august 8th 2021. Need mihai approval
    ensure("Harvest occurred from 4 to 7 November", Interval(2, 6), "DATE-RANGE", "XXXX-11-04 -- XXXX-11-07")
    ensure("from 1987 to 2015", Interval(0, 3), "DATE-RANGE", "1987-XX-XX -- 2015-XX-XX")
    ensure("released annually between 2016 and 2019", Interval(2, 5), "DATE-RANGE", "2016-XX-XX -- 2019-XX-XX")
    ensure("a rainy season from July to October", Interval(3, 6), "DATE-RANGE", "XXXX-07-XX -- XXXX-10-XX")
    //TODO : end of august 8th changes
  }

  it should "recognize measurement units" in {
    ensure("It was 12 ha", Interval(2, 4), "MEASUREMENT", "12.0 ha")
    //TODO : below all were new measurement formats found in habitus docs. added on august 8th 2021. Need mihai approval
    ensure(sentence= "yield potentials of 12 mt.", Interval(3, 5), goldEntity="MEASUREMENT", goldNorm= "12.0 mt")
    ensure(sentence= "yield potentials of 12 tonnes.", Interval(3, 5), goldEntity="MEASUREMENT", goldNorm= "12.0 t")
    ensure(sentence= "yield potentials of 12 mega grams.", Interval(3, 5), goldEntity="MEASUREMENT", goldNorm= "12.0 meg")


    // tests for unit normalization
    ensure("It was 12 hectares", Interval(2, 4), "MEASUREMENT", "12.0 ha")
    ensure(sentence= "It was 12 meters long.", Interval(2, 4), goldEntity="MEASUREMENT", goldNorm= "12.0 m")
    ensure(sentence= "It was 12 kilograms.", Interval(2,4), goldEntity="MEASUREMENT", goldNorm= "12.0 kg")
    //TODO : below all were new measurement formats found in habitus docs. added on august 8th 2021. Need mihai approval
    ensure(sentence= "yield potentials of 12 metric tonnes.", Interval(3, 6), goldEntity="MEASUREMENT", goldNorm= "12.0 mt")
    ensure(sentence= "yield potentials of 12 grammes.", Interval(3, 5), goldEntity="MEASUREMENT", goldNorm= "12.0 g")

    // test for parsing literal numbers
    ensure("It was twelve hundred ha", Interval(2, 5), "MEASUREMENT", "1200.0 ha")
    ensure("It was 12 hundred ha", Interval(2, 5), "MEASUREMENT", "1200.0 ha")
    ensure(sentence= "Crops are 2 thousands ha wide.", Interval(2,5), goldEntity="MEASUREMENT", goldNorm= "2000.0 ha")
    ensure(sentence= "Rice crops are 1.5 thousands ha wide", Interval(3, 6), goldEntity="MEASUREMENT", goldNorm= "1500.0 ha")
    ensure(sentence= "Rice crops are 1 ha wide", Interval(3, 5), goldEntity="MEASUREMENT", goldNorm= "1.0 ha")
    ensure(sentence= "Rice crops are one ha wide", Interval(3, 5), goldEntity="MEASUREMENT", goldNorm= "1.0 ha")
    ensure(sentence= "Rice crops are ten ha wide", Interval(3, 5), goldEntity="MEASUREMENT", goldNorm= "10.0 ha")
    ensure(sentence= "Rice crops are twenty five ha wide", Interval(3, 5), goldEntity="MEASUREMENT", goldNorm= "25.0 ha")
    ensure(sentence= "Rice crops are twenty-five ha wide", Interval(3, 5), goldEntity="MEASUREMENT", goldNorm= "25.0 ha")
    ensure(sentence= "Rice crops are one hundred ha wide", Interval(3, 5), goldEntity="MEASUREMENT", goldNorm= "100.0 ha")
    ensure(sentence= "Rice crops are one thousand ha wide", Interval(3, 6), goldEntity="MEASUREMENT", goldNorm= "1000.0 ha")
    ensure(sentence= "Rice crops are one hundred thousand ha wide", Interval(3, 6), goldEntity="MEASUREMENT", goldNorm= "100000.0 ha")


  }

//  //
//  // End unit tests for date recognition.
//  //
//
//  //
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
