package org.clulab.processors

import org.clulab.processors.clu.CluProcessor
import org.clulab.utils.Test

class TestHolidays extends Test {
  val processor = new CluProcessor()
  val badBefore = ignore
  val badAfter = ignore
  val unstable = ignore
  val notFound = ignore // not in JollyDay database

  def test(norm: String, text: String): Unit = {
    val (threw, contained) = try {
      val document = processor.annotate(text)
      val norms = document.sentences.head.norms.get
      // println(norms.mkString(", "))
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

  behavior of "CluProcessor"

  // Please try to alphabetize the holidays.

  it should "All Souls' Day 2019" in {
    test("2019-11-02", "I didn't celebrate All Souls' Day 2019.")
  }

  it should "Christmas 2017" in {
    test("2017-12-25", "Watch Kwahu Christmas 2017 as we countdown to 2018")
  }

  it should "Easter 2017" in {
    test("2017-04-16", "Watch Kwahu Easter 2017 as we countdown to 2018")
  }

  it should "Easter Sunday 2023" in {
    test("2023-04-09", "The live music event is scheduled to take place at the Solace Bar and Restaurant in Cape Coast on Easter Sunday 2023, at 7pm.")
  }

  it should "Easter Sunday 1922" in {
    // test("1922-04-16", "He was subsequently received into the subdiaconate on Easter Sunday, April 16, 1922.")
    test("1922-04-16", "He was subsequently received into the subdiaconate on Easter Sunday 1922.")
  }

  // This is a CluHoliday that is not yet in JollyDay (0.4.9).
  it should "Election Day 2023" in {
    test("2023-11-07", "When will Election Day 2023 be?")
  }

  badBefore should "1998 Good Friday" in {
    // Assumed this year, 2024-03-29 -> XXXX-03-29, which is wrong
    test("1998-04-10", "Northern Ireland’s 1998 Good Friday peace agreement contains provisions to protect and develop relations both on a north/south basis on the island of Ireland and on an east/west basis between the island and Great Britain.")
  }

  it should "Good Friday 1998" in {
    test("1998-04-10", "Northern Ireland’s Good Friday 1998 peace agreement contains provisions to protect and develop relations both on a north/south basis on the island of Ireland and on an east/west basis between the island and Great Britain.")
  }

  unstable should "Good Friday" in {
    // Assumed this year, 2024-03-29
    // It would fail next year, so ignore this test.
    test("XXXX-03-29", "Christians must, therefore, eschew vices such as alcoholism, stealing, fornication, adultery and robbery, he emphasised in his Good Friday sermon.")
  }

  unstable should "Good Friday again" in {
    // Assumed this year, 2024-03-29
    // It would fail next year, so ignore this test.
    test("XXXX-03-29", "Christians across the globe on Friday, marked Good Friday, which is one of the significant pillars of their faith.")
  }

  badBefore should "2023 Good Friday" in {
    // Assumed this year, 2024-03-29 -> XXXX-03-29, which is wrong
    test("2023-04-07", "Bishop Heward-Mills gave the admonishment when he delivered a sermon on the “Cross” at the Church’s 2023 Good Friday Miracle Service, held at the Black Stars Square, in Accra.")
  }

  it should "Good Friday 2023" in {
    test("2023-04-07", "Bishop Heward-Mills gave the admonishment when he delivered a sermon on the “Cross” at the Church’s Good Friday 2023 Miracle Service, held at the Black Stars Square, in Accra.")
  }

  it should "Groundhog Day 2023" in {
    test("2023-02-02", "When did Groundhog Day 2023 take place?")
  }

  // This is a CluHoliday that is not yet in JollyDay (0.4.9).
  it should "Juneteenth 2023" in {
    test("2023-06-19", "When was Juneteenth 2023?")
  }

  it should "Mother's Day 2022" in {
    // test("2022-05-XX", "The donation exercise which took place on Mother's Day, the 8th of May, 2022, saw Naa Dzama and her Purple Angels Foundation visiting the young cancer warriors at the Korle-buOncology unit.")
    test("2022-05-08", "The donation exercise which took place on Mother's Day 2022, saw Naa Dzama and her Purple Angels Foundation visiting the young cancer warriors at the Korle-buOncology unit.")
  }

  it should "Pentecost Sunday 1922" in {
    // test("1922-06-04", "He was ordained deacon on Pentecost Sunday, June 4, 1922.")
    test("1922-06-04", "He was ordained deacon on Pentecost Sunday 1922.")
  }

  it should "Pentecost 2024" in {
    test("2024-05-19", "When is Pentecost 2024?")
  }

  it should "PI day 2015" in {
    test("2015-03-14", "Pi Day 2015 was an event to remember!")
  }

  // This is a CluHoliday that is not yet in JollyDay (0.4.9).
  it should "Presidents Day" in {
    test("2023-02-20", "When was Presidents Day 2023?")
  }

  badAfter should "Thanksgiving in 2016" in {
    // Assumed this year, 2024-11-28 -> XXXX-11-28, which is wrong
    test("2016-11-24", "E.F. then began transferring money under the promise she would be repaid when he returned to Virginia Beach to celebrate Thanksgiving in 2016.")
  }

  it should "Thanksgiving 2016" in {
    test("2016-11-24", "E.F. then began transferring money under the promise she would be repaid when he returned to Virginia Beach to celebrate Thanksgiving 2016.")
  }
}
