package org.clulab.processors

class TestFailedSentences extends CluTest {

  "Processor" should "parse sentence #1 without breaking" in {
    val doc = proc.mkDocument(""" " I ´ m very proud of the citizens of this state , " Gov. John Baldacci said after votes from Tuesday ´ s referendum were counted .""")
    val annotatedDoc = proc.annotate(doc)
  }

  it should "parse sentence #2 without breaking" in {
    val doc = proc.mkDocument(""" GRE q 159 v149 awa3.5 gpa 6.62 whre to get admission MS computer in us? """)
    val annotatedDoc = proc.annotate(doc)
  }

  it should "parse sentence #3 without breaking" in {
    val doc = proc.mkDocument(""" If you are in a job interview and asked to name three of your flaws, what´s the best way to deal with this question? """)
    val annotatedDoc = proc.annotate(doc)
  }

  it should "parse sentence #4 without breaking" in {
    val doc = proc.mkDocument(""" What´s the sense of life? """)
    val annotatedDoc = proc.annotate(doc)
  }

  it should "parse sentence #5 without breaking" in {
    val doc = proc.mkDocument(""" I'm doing heavy weighted dips regularly (body weight + 28–30 kg). Can this eventually lead to a shoulder injury? """)
    val annotatedDoc = proc.annotate(doc)
  }

  it should "parse sentence #6 without breaking" in {
    val doc = proc.mkDocument(""" How good is the International Business programme of Sailesh J Mehta School of Management?  """)
    val annotatedDoc = proc.annotate(doc)
  }

  it should "parse sentence #7 without breaking" in {
    val doc = proc.mkDocument(""" Is Facebook the gateway to the Internet?  """)
    val annotatedDoc = proc.annotate(doc)
  }
}
