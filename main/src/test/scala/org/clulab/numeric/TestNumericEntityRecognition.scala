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

  // TODO: We need a parser for dot dates separated
  it should "recognize Numerical dates with dot separated" in {
    // ensure("on 15.07.2016", Interval(0, 2), "DATE", "2016-07-15")
    // ensure("on 07.2016", Interval(0, 2), "DATE", "2016-07-XX")
    // ensure("on 15.07", Interval(0, 2), "DATE", "XXXX-07-15")
    // ensure("Sowing depended on the available soil moisture and was done on 15.07.2016", Interval(10, 12), "DATE", "2016-07-15")
    // ensure("resulting in harvest in October or November", Interval(4, 7), "DATE", "XXXX-11-XX")

  }

  it should "recognize literal date ranges" in {
    ensure("between 2020/10/10 and 2020/11/11", Interval(0, 4), "DATE-RANGE", "2020-10-10 -- 2020-11-11")
    ensure("from July 20 to July 31", Interval(0, 6), "DATE-RANGE", "XXXX-07-20 -- XXXX-07-31")
    ensure("from 20 to July 31", Interval(0, 5), "DATE-RANGE", "XXXX-07-20 -- XXXX-07-31")
  
  }


  // TODO: Happenings in the middle of months

  it should "recognize dates as the middle part of months" in {
    // ensure("planting from mid-February", Interval(2, 5), "DATE", "XXXX-02-15")
    // ensure("planting from mid-March", Interval(2, 5), "DATE", "XXXX-03-15")
    // ensure("As a function of the onset of rains, rice was sown mid-July in 2016 and early July in 2017", Interval(2, 5), "DATE", "XXXX-03-15")
    // ensure("sowing normally occurs in summer mid-June", Interval(5, 8), "DATE-RANGE",  "XXXX-10-25 -- XXXX-12-10")

    }

  //TODO: Additional mixed dates ranges 

  it should "recognize date ranges" in {
    // ensure("to harvesting in June-July", Interval(3, 6), "DATE-RANGE", "XXXX-06-XX -- XXXX-07-XX")
    // // ensure("planting from mid-February and mid-March", Interval(3, 9), "DATE-RANGE",  "XXXX-02-14 -- XXXX-03-14")
    // ensure("harvesting from October through December", Interval(1, 5), "DATE-RANGE",  "XXXX-10-XX -- XXXX-12-XX")
    ensure("sowing from 25th Oct to 10th Dec", Interval(1, 7), "DATE-RANGE",  "XXXX-10-25 -- XXXX-12-10")
    // ensure("rainfall pattern from June to mid-September", Interval(2, 8), "DATE-RANGE",  "XXXX-06-XX -- XXXX-09-15")
    // ensure("when heading occurred between August 10 and 25", Interval(3, 8), "DATE-RANGE",  "XXXX-08-10 -- XXXX-08-25")
    // ensure("drier season between November and March", Interval(2, 8), "DATE-RANGE",  "XXXX-11-XX -- XXXX-03-XX")
    // ensure("flooding are expected to occur in July to August 2021", Interval(5, 10), "DATE-RANGE",  "2021-07-XX -- 2021-08-XX")
    ensure("farmers sowed Jaya between 20 June and 1 July", Interval(3, 8), "DATE-RANGE",  "XXXX-06-20 -- XXXX-07-01")

  // TODO: It would be interesting to handle such dates ranges 1st week of July: "XXXX-07-01 -- XXXX-07-07
    // ensure(sentence= "transplanted during the 1st week of July", Interval(3, 7), goldEntity= "DATE", goldNorm= "XXXX-07-01")

  }

  // TODO: Other dates that should be recognized

  it should "recognize numeric dates of form mm" in {
    // ensure(sentence= "Rice is normally sown at the end of May", Interval(8, 9), goldEntity= "DATE", goldNorm= "XXXX-05-XX")
    // ensure(sentence= "harvested the following August", Interval(3, 4), goldEntity= "DATE", goldNorm= "XXXX-08-XX")
    // ensure(sentence= "wheat is mostly sown in late September", Interval(6, 7), goldEntity= "DATE", goldNorm= "XXXX-09-XX")
    // ensure(sentence= "Rains are expected to start in July", Interval(6, 7), goldEntity= "DATE", goldNorm= "XXXX-07-XX")
  
  }

  it should "recognize numeric dates of form dd-mm" in {
    // ensure(sentence= "transplanted during the 1st of July", Interval(3, 6), goldEntity= "DATE", goldNorm= "XXXX-07-01")
    // ensure(sentence= "the 20th of October", Interval(1, 4), goldEntity= "DATE", goldNorm= "XXXX-10-20")
  }

  // TODO: need to decide on the output of such dates
  it should "recognize numeric dates of form yyyy" in {
    // ensure(sentence= "the highest grain yield in 1998/99", Interval(5,7), goldEntity= "DATE-RANGE", goldNorm= "1999-XX-XX")
  }

  it should "recognize numeric dates of form mm-dd" in {
    ensure(sentence= "before Aug. 15th", Interval(1, 3), goldEntity= "DATE", goldNorm= "XXXX-08-15")
    ensure(sentence= "after March 5th", Interval(1, 3), goldEntity= "DATE", goldNorm= "XXXX-03-05")
    ensure(sentence= "Farmers planted on July 11", Interval(3, 5), goldEntity= "DATE", goldNorm= "XXXX-07-11")

  }

  it should "recognize numeric dates of form mm-yy" in {
    // ensure(sentence= "July in 2016", Interval(0, 3), goldEntity= "DATE", goldNorm= "2016-07-XX")
    // ensure(sentence= "we’ll have more seed available again in Nov/Dec 2021", Interval(7, 11), goldEntity= "DATE", goldNorm= "2021-12-XX")
  }

  // TODO: We need a parser for dates of the form: dd of mm, yy or mm in yy
  it should "recognize numeric dates of form dd-mm-yy" in {
    // ensure(sentence= "SSP and potassium SOP were applied at sowing time on 24th of June, 2010", Interval(10, 15), goldEntity= "DATE", goldNorm= "2010-06-24")
    // ensure(sentence= "Jaya was planted on 14th of July 2020", Interval(4, 8), goldEntity= "DATE", goldNorm= "2000-07-14")
    ensure(sentence= "on 6th Jan, 2009", Interval(1, 5), goldEntity= "DATE", goldNorm= "2009-01-06")
    // ensure(sentence= "on 18th of Oct 2019", Interval(1, 5), goldEntity= "DATE", goldNorm= "2019-10-18")
    // ensure(sentence= "old seedlings transplanted on 14 July in 1999/00", Interval(4, 8), goldEntity= "DATE", goldNorm= "2000-07-14")

  }
  


  it should "recognize measurement units" in {
    ensure("It was 12 ha", Interval(2, 4), "MEASUREMENT", "12.0 ha")

    // tests for unit normalization
    ensure("It was 12 hectares", Interval(2, 4), "MEASUREMENT", "12.0 ha")
    ensure(sentence= "It was 12 meters long.", Interval(2, 4), goldEntity="MEASUREMENT", goldNorm= "12.0 m")
    ensure(sentence= "It was 12 kilograms.", Interval(2,4), goldEntity="MEASUREMENT", goldNorm= "12.0 kg")

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


