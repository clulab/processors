package org.clulab.processors

import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.utils.Test

class TestCoreNLPHolidays extends Test {
  val processor = new ShallowNLPProcessor()

  def test(norm: String, text: String): Unit = {
    val (threw, contained) = try {
      val document = processor.annotate(text)
      val norms = document.sentences.head.norms.get
      val contained = norms.contains(norm)

      (false, contained)
    }
    catch {
      case throwable: Throwable =>
        throwable.printStackTrace()
        (true, false)
    }

    threw should be (false)
    if (!threw)
      contained should be (true)
  }

  behavior of "ShallowNLPProcessor"

  it should "Christmas 2017" in {
    test("2017-12-25", "Watch Kwahu Christmas 2017 as we countdown to 2018")
  }

  it should "Easter 2017" in {
    test("2017-04-16", "Watch Kwahu Easter 2017 as we countdown to 2018")
  }

  it should "Easter Sunday 2023" in {
    test("2023-04-09", "The live music event is scheduled to take place at the Solace Bar and Restaurant in Cape Coast on Easter Sunday, April 9, 2023, at 7pm.")
  }

  it should "Mother's Day 2022" in {
    test("2022-05-08", "The donation exercise which took place on Mother's Day, the 8th of May, 2022, saw Naa Dzama and her Purple Angels Foundation visiting the young cancer warriors at the Korle-buOncology unit.")
  }

  it should "Easter Sunday 1922" in {
    test("1922-04-16", "He was subsequently received into the subdiaconate on Easter Sunday, April 16, 1922.")
  }

  it should "Pentecost Sunday 1922" in {
    test("1922-06-04", "He was ordained deacon on Pentecost Sunday, June 4, 1922.")
  }

  it should "Thanksgiving 2016" in {
    test("2016-11-24", "E.F. then began transferring money under the promise she would be repaid when he returned to Virginia Beach to celebrate Thanksgiving in 2016.")
  }

  it should "Good Friday 1998" in {
    test("1998-04-10", "Northern Ireland’s 1998 Good Friday peace agreement contains provisions to protect and develop relations both on a north/south basis on the island of Ireland and on an east/west basis between the island and Great Britain.")
  }

  it should "Good Friday" in {
    test("GOOD_FRIDAY", "Christians must, therefore, eschew vices such as alcoholism, stealing, fornication, adultery and robbery, he emphasised in his Good Friday sermon.")
  }

  it should "Good Friday 2023" in {
    test("2023-04-07", "Bishop Heward-Mills gave the admonishment when he delivered a sermon on the “Cross” at the Church’s 2023 Good Friday Miracle Service, held at the Black Stars Square, in Accra.")
  }

  it should "Good Friday again" in {
    test("GOOD_FRIDAY", "Christians across the globe on Friday, marked Good Friday, which is one of the significant pillars of their faith.")
  }
}
