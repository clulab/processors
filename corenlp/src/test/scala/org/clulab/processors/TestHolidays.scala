package org.clulab.processors

import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.utils.Test

class TestHolidays extends Test {
  val processor = new ShallowNLPProcessor()

  def test(text: String): Unit = {
    val threw = try {
      processor.annotate(text)
      false
    }
    catch {
      case throwable: Throwable => true
    }

    threw should be(false)
  }

  behavior of "ShallowNLPProcessor"

  it should "Christmas 2017" in {
    test("Watch Kwahu Christmas 2017 as we countdown to 2018")
  }

  it should "Easter 2017" in {
    test("Watch Kwahu Easter 2017 as we countdown to 2018")
  }

  it should "Easter Sunday" in {
    test("The live music event is scheduled to take place at the Solace Bar and Restaurant in Cape Coast on Easter Sunday, April 9, 2023, at 7pm.")
  }

  it should "Mother's Day" in {
    test("The donation exercise which took place on Mother's Day, the 8th of May, 2022, saw Naa Dzama and her Purple Angels Foundation visiting the young cancer warriors at the Korle-buOncology unit.")
  }
}
