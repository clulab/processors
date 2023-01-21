package org.clulab.processors

import org.clulab.processors.clu.tokenizer.{EnglishSentenceSplitter, OpenDomainEnglishLexer, RawToken, Tokenizer, TokenizerStepContractions}
import org.clulab.utils.PrintUtils._
import org.clulab.utils.{Test, Timers}

import scala.util.Random

class TestContractions extends Test {

  case class Specification(contraction: String, words: String*) {
    val string = words.mkString(" ")
  }

  val specifications = Array(
    Specification("let's", "let", "'s"), //
    Specification("here's", "here", "'s"), //  -> not possessive, is
    Specification("that's", "that", "'s"), //  -> possibly possessive, is or maybe has, it's that's thing, not your thing
    Specification("there's", "there", "'s"), //  -> not possessive (their), is or has
    Specification("what's", "what", "'s"), //  -> is or has
    Specification("how's", "how", "'s"), //  -> not possessive, is or has
    Specification("he's", "he", "'s"), //  -> otherwise his, is or has
    Specification("it's", "it", "'s"), //  -> otherwise its, is or has
    Specification("she's", "she", "'s"), //  -> otherwise her, is or has
    Specification("who's", "who", "'s"), //  -> otherwise whose, is or has
    Specification("this's", "this", "'s"), //  -> not possessive, is or has
    Specification("when's", "when", "'s"), //  -> not possessive, is or has
    Specification("where's", "where", "'s"), //  -> not possessive
    Specification("which's", "which", "'s"), //  -> not possessive
    Specification("why's", "why", "'s"), //  -> not possessive
    Specification("somebody's", "somebody", "'s"), // , is or has, can it be possessive?
    Specification("someone's", "someone", "'s"), //  can be possessive
    Specification("something's", "something", "'s"), //
    Specification("one's", "one", "'s"), //  can be possessive

    Specification("n't", "not"),
    Specification("ain't", "ai", "not"), //  -> multiple, wrong
    Specification("didn't", "did", "not"),
    Specification("don't", "do", "not"),
    Specification("doesn't", "does", "not"),
    Specification("can't", "ca", "not"), //  -> can not, wrong!
    Specification("isn't", "is", "not"),
    Specification("aren't", "are", "not"),
    Specification("shouldn't", "should", "not"),
    Specification("couldn't", "could", "not"),
    Specification("wouldn't", "would", "not"),
    Specification("hasn't", "has", "not"),
    Specification("wasn't", "was", "not"),
    Specification("won't", "will", "not"),
    Specification("weren't", "were", "not"),
    Specification("shan't", "sha", "not"), // shall not, wrong!

    Specification("I'm", "I", "am"),

    Specification("you're", "you're"),
    Specification("we're", "we're"),
    Specification("they're", "they're"),
    Specification("who're", "who're"),

    Specification("I'll", "I", "will"),
    Specification("we'll", "we", "will"),
    Specification("you'll", "you", "will"),
    Specification("it'll", "it", "will"),
    Specification("she'll", "she", "will"),

    Specification("I've", "I've"),
    Specification("should've", "should've"),
    Specification("you've", "you've"),
    Specification("could've", "could've"),
    Specification("they've", "they've"),
    Specification("we've", "we've"),

    Specification("I'd", "I", "'d"),
    Specification("they'd", "they", "'d"),
    Specification("you'd", "you", "'d"),
    Specification("we'd", "we", "'d"),
    Specification("he'd", "he", "'d"),
    Specification("she'd", "she", "'d"),
    Specification("how'd", "how", "'d"),
    Specification("that'd", "that", "'d"), //  would only, not had
    Specification("what'd", "what", "'d"), //  would only? what'd you done?
    Specification("who'd", "who", "'d"),
    Specification("where'd", "where", "'d"), // did
    Specification("cont'd", "cont'd") // exception
  )

  // These are different because the tokenizer takes them apart
  // before the TokenizerStepContractions has its chance.
  case class ShortSpecification(contraction: String, words: String*) {
    val string = "' " + contraction.substring(1)
    val contractionString = words.mkString(" ")
  }

  val shortSpecifications = Array(
    ShortSpecification("'s", "'s"),
    ShortSpecification("'m", "am"),
    ShortSpecification("'re", "'re"),
    ShortSpecification("'ll", "will"), // wrong because raw turned to will
    ShortSpecification("'ve", "'ve"),
    ShortSpecification("'d", "'d")
  )

  val tokenizerStepContractions = new TokenizerStepContractions
  val tokenizer = new Tokenizer(new OpenDomainEnglishLexer, Seq(tokenizerStepContractions), new EnglishSentenceSplitter)

  behavior of "TokenizerStepContractions"

  it should "run specifications with Tokenizer" in {
    val random = new Random(0)

    specifications.foreach { specification =>
      val leftOffset = random.nextInt(10)
      val rightOffset = random.nextInt(10)
      val sentence = tokenizer.tokenize(" " * leftOffset + specification.contraction + " " * rightOffset, sentenceSplit = false).head

      specification.contraction.print("", "", ": ")
      sentence.words.println(", ")

      sentence.startOffsets.head should be (leftOffset)
      sentence.raw.mkString should be (specification.contraction)
      sentence.endOffsets.last - sentence.startOffsets.head should be (specification.contraction.length)
      sentence.words.mkString(" ") should be (specification.string)
    }
  }

  it should "run short specifications with Tokenizer" in {
    val random = new Random(0)

    shortSpecifications.foreach { shortSpecification =>
      val leftOffset = random.nextInt(10)
      val rightOffset = random.nextInt(10)
      val sentence = tokenizer.tokenize(" " * leftOffset + shortSpecification.contraction + " " * rightOffset, sentenceSplit = false).head

      shortSpecification.contraction.print("", "", ": ")
      sentence.words.println(", ")

      sentence.startOffsets.head should be(leftOffset)
      sentence.raw.mkString should be(shortSpecification.contraction)
      sentence.endOffsets.last - sentence.startOffsets.head should be(shortSpecification.contraction.length)
      sentence.words.mkString(" ") should be(shortSpecification.string)
    }
  }

  it should "run specifications with TokenizerStepContractions" in {
    val random = new Random(0)

    specifications.foreach { specification =>
      val offset = random.nextInt(10)
      val inputs = Array(RawToken(specification.contraction, offset))
      val outputs = tokenizerStepContractions.process(inputs)

      specification.contraction.println("", "", ": ")
      outputs.map(_.word).println("words: ", ", ", "")
      outputs.map(_.raw).println("raws: ", ", ", "")

      val wordString = outputs.map(_.word).mkString(" ")
      val rawString = outputs.map(_.raw).mkString

      outputs.head.beginPosition should be (offset)
      rawString should be (specification.contraction)
      outputs.last.endPosition - outputs.head.beginPosition should be(specification.contraction.length)
      wordString should be (specification.string)
    }
  }

  it should "run short specifications with TokenizerStepContractions" in {
    val random = new Random(0)

    shortSpecifications.foreach { shortSpecification =>
      val offset = random.nextInt(10)
      val inputs = Array(RawToken(shortSpecification.contraction, offset))
      val outputs = tokenizerStepContractions.process(inputs)

      shortSpecification.contraction.println("", "", ": ")
      outputs.map(_.word).println("words: ", ", ", "")
      outputs.map(_.raw).println("raws: ", ", ", "")

      val wordString = outputs.map(_.word).mkString(" ")
      val rawString = outputs.map(_.raw).mkString

      outputs.head.beginPosition should be(offset)
      // if (shortSpecification.contraction != "'ll")
        rawString should be(shortSpecification.contraction)
      outputs.last.endPosition - outputs.head.beginPosition should be(shortSpecification.contraction.length)
      wordString should be(shortSpecification.contractionString)
    }
  }

  it should "run quickly" in {
    // With old code
    // 0:00:00:10.567
    // 0:00:00:11.223
    // 0:00:00:10.438
    // 0:00:00:11.267
    // 0:00:00:11.555
    // With new code 1
    // 0:00:00:13.312
    // 0:00:00:12.742
    // With new code 2
    // 0:00:00:08.777
    // 0:00:00:10.804
    val timer = Timers.getOrNew("run")
    val inputs = specifications.flatMap { specification =>
      Array(RawToken(specification.contraction, 0))
    }

    timer.time {
      1.to(100000).foreach { index =>
        val outputs = tokenizerStepContractions.process(inputs)
        // outputs.map(_.word).println(", ")
      }
    }
    println(timer.elapsedToString)
  }
}
