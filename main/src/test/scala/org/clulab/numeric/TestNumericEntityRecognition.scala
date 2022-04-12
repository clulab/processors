package org.clulab.numeric

import org.clulab.dynet.Utils
import org.clulab.processors.Sentence
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.clu.tokenizer.Tokenizer
import org.clulab.struct.Interval
import org.scalatest.{FlatSpec, Matchers}

import scala.util.matching.Regex

class TestNumericEntityRecognition extends FlatSpec with Matchers {

  class HabitusTokenizer(tokenizer: Tokenizer) extends Tokenizer(tokenizer.lexer, tokenizer.steps, tokenizer.sentenceSplitter) {
    // TODO: Make sure en dash is preserved in raw somehow!

    override def tokenize(text: String, sentenceSplit: Boolean = true): Array[Sentence] = {
      // Cheat and swap out some en dashes if necessary.
      val habitusText =
        if (text.contains(HabitusTokenizer.endash))
          HabitusTokenizer.regex.replaceAllIn(text,HabitusTokenizer.replacer)
        else
          text

      tokenizer.tokenize(habitusText, sentenceSplit)
    }
  }

  object HabitusTokenizer {
    val dash = "-"
    val endash = "\u2013"
    val regex = {
      val start = "^"
      val end = "$"
      val notDigit = "[^\\d]"
      val exponent = "[12]" // So far 1 and are all we've encountered as exponents.

      s"($start|$notDigit)$endash($exponent($notDigit|$end))".r
    }
    val replacer: Regex.Match => String = m => s"${m.group(1)}$dash${m.group(2)}"
  }

  class HabitusProcessor() extends CluProcessor {
    lazy val habitusTokenizer: HabitusTokenizer = new HabitusTokenizer(localTokenizer)
    override lazy val tokenizer: Tokenizer = habitusTokenizer
  }

  Utils.initializeDyNet()
  val ner = NumericEntityRecognizer()
  val proc = new HabitusProcessor()

  //
  // unit tests starts here
  //

  // these should be captured by rules date-1 and date-2
  "the numeric entity recognizer" should "recognize dates in the European format" in {
    ensure("It is 12 May, 2000", Interval(2, 6), "DATE", "2000-05-12")
    ensure("It was May 2000", Interval(2, 4), "DATE", "2000-05-XX")
    ensure("It was 25 May", Interval(2, 4), "DATE", "XXXX-05-25")
    ensure("It was May", Interval(2, 3), "DATE", "XXXX-05-XX")
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

  it should "recognize numeric dates of form month of year" in {
    ensure(sentence= "sowing date is best in May of 2020", Interval(5, 8), goldEntity= "DATE", goldNorm= "2020-05-XX")
    ensure(sentence= "sowing date in July of 2020", Interval(3, 6), goldEntity= "DATE", goldNorm= "2020-07-XX")
    ensure(sentence= "It is not desirable to sow in January of 2001", Interval(7, 10), goldEntity= "DATE", goldNorm= "2001-01-XX")
    ensure(sentence= "It is not desirable to sow in Jan of 2001", Interval(7, 10), goldEntity= "DATE", goldNorm= "2001-01-XX")
    ensure(sentence= "It is not desirable to sow in Jul of 2001", Interval(7, 10), goldEntity= "DATE", goldNorm= "2001-07-XX")
    ensure(sentence= "It is not desirable to sow in Aug of 2001", Interval(7, 10), goldEntity= "DATE", goldNorm= "2001-08-XX")
    ensure(sentence= "It is not desirable to sow in Dec of 2001", Interval(7, 10), goldEntity= "DATE", goldNorm= "2001-12-XX")
  }

  it should "recognize numeric dates of form month date of year" in {
    ensure(sentence= "sowing date is best on October 8 of 2020", Interval(5, 9), goldEntity= "DATE", goldNorm= "2020-10-08")
    ensure(sentence= "Rain will be on April 8 of 2020", Interval(4, 8), goldEntity= "DATE", goldNorm= "2020-04-08")
    ensure(sentence= "December 18 of 2002", Interval(0, 4), goldEntity= "DATE", goldNorm= "2002-12-18")
    ensure(sentence= "February 21 of 1002", Interval(0, 4), goldEntity= "DATE", goldNorm= "1002-02-21")
  }

  it should "recognize dates with ordinal days" in {
    ensure(sentence = "Planting dates are between July 1st and August 2nd.", Interval(3, 9), goldEntity = "DATE-RANGE", "XXXX-07-01 -- XXXX-08-02")
  }

  it should "recognize \"month of X\" patterns" in {
    ensure(sentence = "It was the month of January", Interval(3, 6), goldEntity = "DATE", goldNorm = "XXXX-01-XX")
    ensure(sentence = "It was the month of January, 2020", Interval(3, 8), goldEntity = "DATE", goldNorm = "2020-01-XX")
  }

  it should "recognize date from holiday" in {
    ensure(sentence= "Christmas 2016", Interval(0, 2), goldEntity= "DATE", goldNorm= "2016-12-25")
    ensure(sentence= "Independence Day", Interval(0, 2), goldEntity= "DATE", goldNorm= "XXXX-07-04")
    ensure(sentence= "independence day", Interval(0, 2), goldEntity= "DATE", goldNorm= "XXXX-07-04")
    ensure(sentence= "New Years Eve", Interval(0, 2), goldEntity= "DATE", goldNorm= "XXXX-12-31")
    ensure(sentence= "new year's eve", Interval(0, 2), goldEntity= "DATE", goldNorm= "XXXX-12-31")
    ensure(sentence= "Martin Luther King Jr. Day 2022", Interval(0, 4), goldEntity= "DATE", goldNorm= "2022-01-17")
    ensure(sentence= "MLK day 2022", Interval(0, 1), goldEntity= "DATE", goldNorm= "2022-01-17")
    ensure(sentence= "before Patriots' day 2021", Interval(0, 2), goldEntity= "DATE-RANGE", goldNorm= "XXXX-XX-XX -- 2021-04-19")
    ensure(sentence= "between Christmas and New Year", Interval(0, 4), goldEntity= "DATE-RANGE", goldNorm= "XXXX-12-25 -- XXXX-01-01")
  }

  it should "recognize date ranges" in {
    ensure("between 2020/10/10 and 2020/11/11", Interval(0, 4), "DATE-RANGE", "2020-10-10 -- 2020-11-11")
    ensure("from July 20 to July 31", Interval(0, 6), "DATE-RANGE", "XXXX-07-20 -- XXXX-07-31")
    ensure("from 20 to July 31", Interval(0, 5), "DATE-RANGE", "XXXX-07-20 -- XXXX-07-31")
    ensure("between October 31 and December 31 of 2020", Interval(0, 8), "DATE-RANGE", "2020-10-31 -- 2020-12-31")
    ensure("Sowing between October 31 and December 30 , 2020 is optimal", Interval(1, 9), "DATE-RANGE", "2020-10-31 -- 2020-12-30")
  }

  it should "recognize date month ranges" in {
    ensure("between January and June 2017", Interval(0, 5), "DATE-RANGE", "2017-01-XX -- 2017-06-XX")
    ensure("March to May 2017", Interval(0, 4), "DATE-RANGE", "2017-03-XX -- 2017-05-XX")
    ensure("May - October 2016", Interval(0, 4), "DATE-RANGE", "2016-05-XX -- 2016-10-XX")
  }

  it should "recognize different dates" in {
    ensure("January 2016 and June 2017", Interval(0, 2), "DATE", "2016-01-XX")
    ensure("January 2016 and June 2017", Interval(3, 5), "DATE", "2017-06-XX")
  }

  it should "recognize relative dates" in {
    ensure("Since January 2016", Interval(0, 3), "DATE-RANGE", "2016-01-XX -- ref-date")
    ensure("Until January 2016", Interval(0, 3), "DATE-RANGE", "ref-date -- 2016-01-XX")
    ensure("from January 2016", Interval(0, 3), "DATE-RANGE", "2016-01-XX -- ref-date")
  }

  it should "recognize unbounded date ranges" in {
    ensure("before July 2016", Interval(0, 3), "DATE-RANGE", "XXXX-XX-XX -- 2016-07-XX")
    ensure("prior to July 2016", Interval(0, 4), "DATE-RANGE", "XXXX-XX-XX -- 2016-07-XX")
    ensure("after July 2016", Interval(0, 3), "DATE-RANGE", "2016-07-XX -- XXXX-XX-XX")
    ensure("before July 15", Interval(0, 3), "DATE-RANGE", "XXXX-XX-XX -- XXXX-07-15")
    ensure("after July 15", Interval(0, 3), "DATE-RANGE", "XXXX-07-15 -- XXXX-XX-XX")
  }

  it should "recognize date ranges from seasons" in {
    ensure("winter 2017", Interval(0, 2), "DATE-RANGE", "2016-12-21 -- 2017-03-20")
    ensure("spring 2017", Interval(0, 2), "DATE-RANGE", "2017-03-20 -- 2017-06-21")
    ensure("summer 2017", Interval(0, 2), "DATE-RANGE", "2017-06-21 -- 2017-09-22")
    ensure("autumn 2017", Interval(0, 2), "DATE-RANGE", "2017-09-22 -- 2017-12-21")
    ensure("autumn in 2017", Interval(0, 3), "DATE-RANGE", "2017-09-22 -- 2017-12-21")
    ensure("2017 autumn", Interval(0, 2), "DATE-RANGE", "2017-09-22 -- 2017-12-21")
    ensure("winter", Interval(0, 1), "DATE-RANGE", "XXXX-12-21 -- XXXX-03-20")
    ensure("spring", Interval(0, 1), "DATE-RANGE", "XXXX-03-20 -- XXXX-06-21")

  }

  it should "recognize date ranges with seasons" in {
    ensure("from spring to autumn 2017", Interval(0, 4), "DATE-RANGE", "2017-06-21 -- 2017-09-22")
    ensure("between spring and autumn 2017", Interval(0, 4), "DATE-RANGE", "2017-06-21 -- 2017-09-22")
    ensure("between autumn 2017 and spring 2018", Interval(0, 5), "DATE-RANGE", "2017-12-21 -- 2018-03-20")
    ensure("between autumn and spring", Interval(0, 4), "DATE-RANGE", "XXXX-12-21 -- XXXX-03-20")
    ensure("Since winter 2017", Interval(0, 3), "DATE-RANGE", "2017-03-20 -- ref-date")
    ensure("Since winter", Interval(0, 2), "DATE-RANGE", "XXXX-03-20 -- ref-date")
    ensure("Until summer 2017", Interval(0, 3), "DATE-RANGE", "ref-date -- 2017-06-21")
    ensure("Until summer", Interval(0, 2), "DATE-RANGE", "ref-date -- XXXX-06-21")
  }

  it should "recognize unbounded date ranges with seasons" in {
    ensure("before summer 2016", Interval(0, 3), "DATE-RANGE", "XXXX-XX-XX -- 2016-06-21")
    ensure("prior to summer 2016", Interval(0, 4), "DATE-RANGE", "XXXX-XX-XX -- 2016-06-21")
    ensure("after summer 2016", Interval(0, 3), "DATE-RANGE", "2016-09-22 -- XXXX-XX-XX")
    ensure("before summer", Interval(0, 2), "DATE-RANGE", "XXXX-XX-XX -- XXXX-06-21")
    ensure("after summer", Interval(0, 2), "DATE-RANGE", "XXXX-09-22 -- XXXX-XX-XX")
  }

  it should "recognize dates and date-ranges with single months" in {
    ensure("It was January", Interval(2, 3), "DATE", "XXXX-01-XX")
    ensure("It was Jan", Interval(2, 3), "DATE", "XXXX-01-XX")
    ensure("It was between January and March", Interval(2, 3), "DATE-RANGE", "XXXX-01-XX -- XXXX-03-XX")
    ensure("until January", Interval(0, 2), "DATE-RANGE", "ref-date -- XXXX-01-XX")
    ensure("after January", Interval(0, 2), "DATE-RANGE", "XXXX-01-XX -- XXXX-XX-XX")
  }

  it should "recognize dates and date-ranges with modifier" in {
    ensure(sentence= "It was the start of 2020", Interval(2, 6), goldEntity= "DATE", goldNorm= "2020-01-XX")
    ensure(sentence= "It was mid-2020", Interval(2, 4), goldEntity= "DATE", goldNorm= "2020-06-XX")
    ensure(sentence= "It was the end of 2020", Interval(2, 6), goldEntity= "DATE", goldNorm= "2020-12-XX")
    ensure(sentence= "It was the start of January", Interval(2, 6), goldEntity= "DATE", goldNorm= "XXXX-01-01")
    ensure(sentence= "It was mid-January", Interval(2, 4), goldEntity= "DATE", goldNorm= "XXXX-01-15")
    ensure(sentence= "It was the end of January", Interval(2, 6), goldEntity= "DATE", goldNorm= "XXXX-01-31")
    ensure(sentence= "It was the end of February 2020", Interval(2, 7), goldEntity= "DATE", goldNorm= "2020-02-29")
    ensure(sentence= "It was around January 15", Interval(2, 4), goldEntity= "DATE", goldNorm= "XXXX-01-15 [APPROX]")
    ensure(sentence= "It was around New Years Eve", Interval(2, 4), goldEntity= "DATE", goldNorm= "XXXX-12-31 [APPROX]")
    ensure(sentence= "It was around mid-January", Interval(2, 4), goldEntity= "DATE", goldNorm= "XXXX-01-15 [APPROX]")
    ensure(sentence= "It was around the start of 2020", Interval(2, 7), goldEntity= "DATE", goldNorm= "2020-01-XX [APPROX]")
    ensure(sentence= "before the end of 2020", Interval(0, 5), goldEntity= "DATE-RANGE", goldNorm= "XXXX-XX-XX -- 2020-12-XX")
    ensure(sentence= "from early June to mid-September", Interval(0, 6), goldEntity= "DATE-RANGE", goldNorm= "XXXX-06-01 -- XXXX-09-15")
    ensure(sentence= "since around the end of 2020", Interval(0, 6), goldEntity= "DATE-RANGE", goldNorm= "2020-12-XX [APPROX] -- ref-date")
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

  // tests for recognizing fertilizer, seeds and yield measurement units
  it should "recognize literal measurement units" in {
    // these tests should pass 
    ensure(sentence= "Imports of rice in the decade 2008-2017 amounted on average to 1500000 tonnes", Interval(11, 13), goldEntity="MEASUREMENT", goldNorm="1500000.0 t")
    ensure(sentence= "They had yield potentials of 10 metric tons per hectare", Interval(5, 10), goldEntity="MEASUREMENT", goldNorm="10.0 t/ha")
    ensure(sentence= "Such observations were replaced with a cap value of 700 kilograms per hectare", Interval(9, 13), goldEntity="MEASUREMENT", goldNorm="700.0 kg/ha")
    ensure(sentence= "The production from the SRV was therefore 360000 tons of paddy", Interval(7, 9), goldEntity="MEASUREMENT", goldNorm="360000.0 t")
    ensure(sentence= "Total production was 6883 thousand tons", Interval(3, 6), goldEntity="MEASUREMENT", goldNorm="6883000.0 t")
    ensure(sentence= "During 2009-10, area under rice cultivation was 2883 thousand hectares", Interval(8, 11), goldEntity="MEASUREMENT", goldNorm="2883000.0 ha")
    ensure(sentence= "Senegal is forecast at 2.4 million MT", Interval(4, 7), goldEntity="MEASUREMENT", goldNorm="2400000.0 t")
    ensure(sentence= "To determine the effect of planting date on key agronomic traits in rice, an 8 yr data", Interval(15, 17), goldEntity="MEASUREMENT", goldNorm="8.0 y")
    ensure(sentence= "Planting dates were tentatively spaced by 2 wk", Interval(6, 8), goldEntity="MEASUREMENT", goldNorm="2.0 w")
    
    // I propose to ignore this test. If we handle the dot here, we will parse incorrectly all the numbers with decimals
    // ensure(sentence= "1.68 ton for one hectare as a result of that the rainfall", Interval(0, 5), goldEntity="MEASUREMENT", goldNorm="1.68 t/ha")
    // ensure(sentence= "Rice is planted in early May next 5% reduction is only 7 d after that (24 April)", Interval(12, 14), goldEntity="MEASUREMENT", goldNorm="7.0 d")
    // ensure(sentence= "Imports of rice in the decade 2008-2017 amounted on average to 1,500,000 tonnes", Interval(11, 13), goldEntity="MEASUREMENT", goldNorm="1500000.0 t")

    // I propose to ignore this test. If we handle the dot here, we will parse incorrectly all the numbers with decimals
    // ensure(sentence= "The production from the SRV was therefore 360.000 tons of paddy", Interval(7, 9), goldEntity="MEASUREMENT", goldNorm="360000.0 t")

    // measurements that contain number ranges should work
    ensure(sentence= "Weeding timing ranged from 2 to 17 days", Interval(3, 8), goldEntity="MEASUREMENT", goldNorm="2.0 -- 17.0 d")
    
    // TODO: not sure what should be the output of such measurement '3 or 4 days'
    ensure(sentence= "and lasted 3 or 4 days in both wet seasons", Interval(4, 6), goldEntity="MEASUREMENT", goldNorm="4.0 d")

  }

  // TODO: this requires non trivial changes to the tokenizer
  /*
  // tests for recognizing units which are sticked to values
  it should "recognize measurement units which are sticked to values" in {
    ensure(sentence= "Single cropping rice area is 4561.9km2", Interval(5, 7), goldEntity="MEASUREMENT", goldNorm="4561.9 km2")
    ensure(sentence= "Application dosage is 200kg/ha for compound fertilizer and 180kg/ha for urea", Interval(3, 6), goldEntity="MEASUREMENT", goldNorm="200.0 kg/ha")
    ensure(sentence= "The maximum seed yield was (3.43ton ha-1) gained", Interval(6, 12), goldEntity="MEASUREMENT", goldNorm="3.43 t/ha")

  }
  */

  // tests for recognizing units which change their meaning after normalization
  it should "recognize measurement units which should not be normalized" in {
    
    // TODO: Mihai ==> How do we handle cases like (Mg/ha or Mg/m3) which shouldn't be normalized as this is one of the preferred unit for yield or application rate
    // ensure(sentence= "Genetically improved rice varieties have grain yield potential of 10 Mg ha-1", Interval(9, 12), goldEntity="MEASUREMENT", goldNorm="10.0 Mg/ha")

  }

  // tests for recognizing complex measurement units
  it should "recognize complex measurement units" in {
    ensure(sentence= "Recommended seed usage is 130 kg/ha", Interval(4, 8), goldEntity="MEASUREMENT", goldNorm="130.0 kg/ha")
    ensure(sentence= "1.25 to 1.65 mt/ha higher on average", Interval(0, 6), goldEntity="MEASUREMENT", goldNorm="1.25 -- 1.65 t/ha")
    // TODO: not handling ranging in a single token like this, yet
    //ensure(sentence= "With average yields of 6-7 mt/ha", Interval(4, 10), goldEntity="MEASUREMENT", goldNorm="6-7 t/ha")
    ensure(sentence= "Average yield reached 7.2 t ha-1 in 1999", Interval(3, 6), goldEntity="MEASUREMENT", goldNorm="7.2 t/ha")
    ensure(sentence= "The Nakhlet farmers’ organization bought 7 tonnes of urea", Interval(6, 8), goldEntity="MEASUREMENT", goldNorm="7.0 t")
    // ensure(sentence= "Fertilizers were given to farmers proportionally to their cultivated area at the rate of 250 kg urea ha-1", Interval(14, 18), goldEntity="MEASUREMENT", goldNorm="250.0 kg/ha")
    
    // TODO: not handling ranging in a single token like this, yet
    // ensure(sentence= "Rainfed rice yields average 1-2 MT/hectare", Interval(4, 10), goldEntity="MEASUREMENT", goldNorm="1.0 -- 2.0 t/ha")
    
    // TODO: wondering if we can handle such measures:mxm
    // ensure(sentence= "having a gross plot size of 3.0 m × 6.0 m", Interval(6, 11), goldEntity="MEASUREMENT", goldNorm="18.0 m2")
   
    ensure(sentence= "500 mL acre-1 was applied on moist soil after 30-35 days of planting each crop", Interval(0, 3), goldEntity="MEASUREMENT", goldNorm="500.0 ml/acre")
    ensure(sentence= "The total area represented in each image was 3.24 cm2", Interval(8, 10), goldEntity="MEASUREMENT", goldNorm="3.24 cm2")
    ensure(sentence= "Average yields, at 1 to 2 tonnes/ha, are much lower than in the SRV", Interval(4, 10), goldEntity="MEASUREMENT", goldNorm="1.0 -- 2.0 t/ha")
    ensure(sentence= "Irrigated rice yields are consistently high, averaging 5 to 6 MT/hectare", Interval(8, 14), goldEntity="MEASUREMENT", goldNorm="5.0 -- 6.0 t/ha")
    ensure(sentence= "Pandan Wangi has a grain yield of 4.94 tons/ha", Interval(7, 11), goldEntity="MEASUREMENT", goldNorm="4.94 t/ha")
    
    // TODO: Constructions like kg N, P ha-1 aren't really units, so we need a fix for this
    // ensure(sentence= "close to the recommendations of 120 kg N ha-1", Interval(5, 11), goldEntity="MEASUREMENT", goldNorm="120.0 kg/ha")
    // ensure(sentence= "19 kg P ha-1 were applied in two top-dressed applications", Interval(0, 6), goldEntity="MEASUREMENT", goldNorm="19.0 kg/ha")
    // ensure(sentence= "cultivated area at the rate of 250 kg urea ha-1", Interval(7, 13), goldEntity="MEASUREMENT", goldNorm="250.0 kg/ha")
    // ensure(sentence= "Potassium (150 kg K2O ha-1) was split equally at basal fertilization", Interval(2, 8), goldEntity="MEASUREMENT", goldNorm="150.0 kg/ha")
    ensure(sentence= "East with land area of 147,141 Km2", Interval(5, 7), goldEntity="MEASUREMENT", goldNorm="147141.0 km2")
    
    // TODO: Temperatures measures need be to addressed
    // ensure(sentence= "Rice should not be planted when the average air and soil temperature is below 15 ˚C", Interval(14, 17), goldEntity="MEASUREMENT", goldNorm="15 ˚C")
    // ensure(sentence= "daily maximum temperature of 40-45 C in May", Interval(4, 8), goldEntity="MEASUREMENT", goldNorm="40.0 -- 45.0 °C")

    // TODO: not handling values hyphen separated with theit units
    // ensure(sentence= "Grain yield was determined from a 5-m2 area in each plot", Interval(6, 9), goldEntity="MEASUREMENT", goldNorm="5.0 m2")
    // ensure(sentence= "Punjab has 3.5 million ha under wheat cultivation with productivity of 5.2-ton ha-1 respectively.", Interval(11, 13), goldEntity="MEASUREMENT", goldNorm="5.2 t/ha")

  }

  // tests for mass and concentation units (Soil bulk density, volume basis etc)
  it should "recognize mass and concentration measurement units" in {
    ensure(sentence= "N content ranged from 0.37 to 0.71 g kg-1 soil", Interval(3, 9), goldEntity="MEASUREMENT", goldNorm="0.37 -- 0.71 g/kg")
    ensure(sentence= "C content ranged from 4.4 to 7.9 mg g-1 soil, ", Interval(3, 9), goldEntity="MEASUREMENT", goldNorm="4.4 -- 7.9 mg/g")
    
    // TODO: Need a fix for concentration units with dot ex: g.kg-1
    // ensure(sentence= "P-Olsen ranged from 4.3 to 17 g.kg-1 soil", Interval(2, 7), goldEntity="MEASUREMENT", goldNorm="4.3 -- 17.0 g/kg")
    // I propose to ignore this test. If we handle the dot here, we will parse incorrectly all the numbers with decimals
    // ensure(sentence= "with concentrations reaching 3.99 mg kg-1", Interval(3, 8), goldEntity="MEASUREMENT", goldNorm="3.99 mg/kg")
    // ensure(sentence= "with a corresponding increase in unit yield of 337.5 kg·ha-1 and only 249 kg·ha-1", Interval(8, 14), goldEntity="MEASUREMENT", goldNorm="337.5 kg/ha")

    ensure(sentence= "the irrigation water supply was above 700 mm", Interval(6, 8), goldEntity="MEASUREMENT", goldNorm="700.0 mm")
    
    // TODO: Fix for measurements units with Greek letters
    // ensure(sentence= "sugar 6976 µg/g", Interval(1, 5), goldEntity="MEASUREMENT", goldNorm="6976.0 µg/g")
    // ensure(sentence= "1.1 mg/g uronic acid", Interval(0, 4), goldEntity="MEASUREMENT", goldNorm="1.1 mg/g")
    // ensure(sentence= "731.5 µg/g protein", Interval(0, 4), goldEntity="MEASUREMENT", goldNorm="731.5 µg/g")
    // ensure(sentence= "Saturated water content 4.54 m3 m-3", Interval(3, 7), goldEntity="MEASUREMENT", goldNorm="4.54 m3/m3")
    // ensure(sentence= "Soil organic carbon (SOC) under fallow varied from 7.1 g kg-1", Interval(8, 13), goldEntity="MEASUREMENT", goldNorm="7.1 g/kg")
  }

  it should "recognize percentages" in {
    ensure("20% of the area is planted", Interval(0, 2), goldEntity = "PERCENTAGE", goldNorm = "20.0 %")
    ensure("20 pct of the area is planted", Interval(0, 2), goldEntity = "PERCENTAGE", goldNorm = "20.0 %")
  }

  it should "work correctly with en dashes" in {
    val endash = "\u2013"

    ensure(sentence= "Imports of rice in the decade 2008" + endash + "2017 amounted on average to 1500000 tonnes", Interval(13, 15), goldEntity="MEASUREMENT", goldNorm="1500000.0 t")
    ensure(sentence= "Imports of rice in the decade 2008" + endash + "2017 amounted on average to 1,500,000 tonnes", Interval(13, 15), goldEntity="MEASUREMENT", goldNorm="1500000.0 t")
    ensure(sentence= "Average yield reached 7.2 t ha" + endash + "1 in 1999", Interval(3, 6), goldEntity="MEASUREMENT", goldNorm="7.2 t/ha")
    ensure(sentence= "Fertilizers were given to farmers proportionally to their cultivated area at the rate of 250 kg urea ha" + endash + "1", Interval(14, 18), goldEntity="MEASUREMENT", goldNorm="250.0 kg/ha")
    //lowercase N ensure(sentence= "close to the recommendations of 120 kg N ha" + endash + "1", Interval(5, 9), goldEntity="MEASUREMENT", goldNorm="120.0 kg/ha")
    //lowercase P ensure(sentence= "19 kg P ha" + endash + "1 were applied in two top-dressed applications", Interval(0, 4), goldEntity="MEASUREMENT", goldNorm="19.0 kg/ha")
    ensure(sentence= "cultivated area at the rate of 250 kg urea ha" + endash + "1", Interval(6, 10), goldEntity="MEASUREMENT", goldNorm="250.0 kg/ha")
    ensure(sentence= "farmers applied a combination of propanil 4 liters ha" + endash + "1", Interval(6, 9), goldEntity="MEASUREMENT", goldNorm="4.0 l/ha")
    //interval ensure(sentence= "daily maximum temperature of 40" + endash + "45 C in May", Interval(4, 8), goldEntity="MEASUREMENT", goldNorm="40 -- 45 °C")
    ensure(sentence= "CEC varied from 18 to 29 cmol kg" + endash + "1", Interval(2, 5), goldEntity="MEASUREMENT", goldNorm="18.0 -- 29.0 cmol/kg") // TODO: check on interval, get two BIO sets
    ensure(sentence= "C content ranged from 4.4 to 7.9 mg g" + endash + "1 soil, ", Interval(3, 9), goldEntity="MEASUREMENT", goldNorm="4.4 -- 7.9 mg/g")
    ensure(sentence= "the rainfall during October and November was about (144 ml) during 2015" + endash + "2016 season may cause better moisture availability", Interval(9, 11), goldEntity="MEASUREMENT", goldNorm="144.0 ml")
  }

  it should "it should not crash on weird texts" in {
    ensure(sentence= "Sown cultivar and areaJaya: 15.0 haJaya: 27.5 haJaya: 26.6 haSahel 202: 27.5 haSahel 202: 22.5 haSahel 108: 12.5 haSahel 108: 0.9 haJaya: 5.0 haWeedingtype and rate2 l 2-4D ha–1+ 4 l Propanil ha–12 l 2-4D ha–1+ 4 l Propanil ha–1manual2 l 2-4D ha–1+ 4 l Propanil ha–12 l 2-4D ha–1+ 4 l Propanil",
        Interval(0, 1), goldEntity="", goldNorm = "")
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

    if(goldEntity.nonEmpty) {
      var first = true
      for (i <- span.indices) {
        val prefix = if (first) "B-" else "I-"
        val label = prefix + goldEntity

        entities(i) should be(label)
        norms(i) should be(goldNorm)

        first = false
      }
    }
  }

  /** Runs the actual numeric entity recognizer */
  def numericParse(sentence: String): (Array[String], Array[String], Array[String]) = {
    val doc = proc.annotate(sentence)
    val mentions = ner.extractFrom(doc)
    setLabelsAndNorms(doc, mentions)

    // assume 1 sentence per doc
    val sent = doc.sentences.head
    (sent.words, sent.entities.get, sent.norms.get)
  }
}
