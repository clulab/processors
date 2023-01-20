package org.clulab.processors

import org.clulab.processors.clu.tokenizer.{EnglishSentenceSplitter, OpenDomainEnglishLexer, RawToken, Tokenizer, TokenizerStepContractions}
import org.clulab.utils.PrintUtils._
import org.clulab.utils.Test

class TestContractions extends Test {

  case class Specification(contraction: String, words: Array[String], raws: Array[String]) {

    def wordString(tokenizer: Boolean): String =
        // If the tokenizer splits it up, ignore the words.
        if (tokenizer && contraction.startsWith("'")) "' " + contraction.substring(1)
        else words.mkString(" ")

    def rawString(tokenizer: Boolean): String =
        if (tokenizer && contraction.startsWith("'")) "' " + contraction.substring(1)
        else raws.mkString(" ")
  }

  // TODO avoid double contractions or take off 've and then work on the next part
  // they all seem to be with 've which isn't expanded anyway
  // 'til, 'tis, 'twas

  val specifications = Array(
    Specification("'s",          Array("'s"),              Array("")),
    Specification("let's",       Array("let", "'s"),       Array("")), //
    Specification("here's",      Array("here", "'s"),      Array("")), //  -> not possessive, is
    Specification("that's",      Array("that", "'s"),      Array("")), //  -> possibly possessive, is or maybe has, it's that's thing, not your thing
    Specification("there's",     Array("there", "'s"),     Array("")), //  -> not possessive (their), is or has
    Specification("what's",      Array("what", "'s"),      Array("")), //  -> is or has
    Specification("how's",       Array("how", "'s"),       Array("")), //  -> not possessive, is or has
    Specification("he's",        Array("he", "'s"),        Array("")), //  -> otherwise his, is or has
    Specification("it's",        Array("it", "'s"),        Array("")), //  -> otherwise its, is or has
    Specification("she's",       Array("she", "'s"),       Array("")), //  -> otherwise her, is or has
    Specification("who's",       Array("who", "'s"),       Array("")), //  -> otherwise whose, is or has
    Specification("this's",      Array("this", "'s"),      Array("")), //  -> not possessive, is or has
    Specification("when's",      Array("when", "'s"),      Array("")), //  -> not possessive, is or has
    Specification("where's",     Array("where", "'s"),     Array("")), //  -> not possessive
    Specification("which's",     Array("which", "'s"),     Array("")), //  -> not possessive
    Specification("why's",       Array("why", "'s"),       Array("")), //  -> not possessive
    Specification("somebody's",  Array("somebody", "'s"),  Array("")), // , is or has, can it be possessive?
    Specification("someone's",   Array("someone", "'s"),   Array("")), //  can be possessive
    Specification("something's", Array("something", "'s"), Array("")), //
    Specification("one's",       Array("one", "'s"),       Array("")), //  can be possessive

    Specification("n't",       Array("not"),           Array("")),
    Specification("ain't",     Array("ai", "not"),     Array("")), //  -> multiple, wrong
    Specification("didn't",    Array("did", "not"),    Array("")), //
    Specification("don't",     Array("do", "not"),     Array("")), //
    Specification("doesn't",   Array("does", "not"),   Array("")), //
    Specification("can't",     Array("ca", "not"),     Array("")), //  -> can not, wrong!
    Specification("isn't",     Array("is", "not"),     Array("")), //
    Specification("aren't",    Array("are", "not"),    Array("")), //
    Specification("shouldn't", Array("should", "not"), Array("")), //
    Specification("couldn't",  Array("could", "not"),  Array("")), //
    Specification("wouldn't",  Array("would", "not"),  Array("")), //
    Specification("hasn't",    Array("has", "not"),    Array("")), //
    Specification("wasn't",    Array("was", "not"),    Array("")), //
    Specification("won't",     Array("will", "not"),   Array("")), //  -> will not
    Specification("weren't",   Array("were", "not"),   Array("")), //
    // Add shan't -> shall not

    Specification("'m", "'", "m"),
    Specification("I'm", "I", "am"), //

    Specification("'re", "'", "re"),
    Specification("you're", "you're"), // not productive?
    Specification("we're", "we're"), //
    Specification("they're", "they're"), //
    Specification("who're", "who're"), //

    Specification("'ll", "'", "ll"),
    Specification("I'll", "I", "will"), //
    Specification("we'll", "we", "will"), //
    Specification("you'll", "you", "will"), //
    Specification("it'll", "it", "will"), //
    Specification("she'll", "she", "will"), //

    Specification("'ve", "'", "ve"),
    Specification("I've", "I've"), //
    Specification("should've", "should've"), //
    Specification("you've", "you've"), //
    Specification("could've", "could've"), //
    Specification("they've", "they've"), //
    Specification("we've", "we've"), //

    Specification("'d", "'", "d"),
    Specification("I'd", "I", "'d"), //   // would or had
    Specification("they'd", "they", "'d"), //
    Specification("you'd", "you", "'d"), //
    Specification("we'd", "we", "'d"), //
    Specification("he'd", "he", "'d"), //
    Specification("she'd", "she", "'d"), //
    Specification("how'd", "how", "'d"), //
    Specification("that'd", "that", "'d"), //  would only, not had
    Specification("what'd", "what", "'d"), //  would only? what'd you done?
    Specification("who'd", "who", "'d") //  did, had, or would
    // where'd did
  )

  val tokenizerStepContractions = new TokenizerStepContractions
  val tokenizer = new Tokenizer(new OpenDomainEnglishLexer, Seq(tokenizerStepContractions), new EnglishSentenceSplitter)

  behavior of "TokenizerStepContractions"

  it should "work with tokenizer" in {
    specifications.foreach { specification =>
      val sentence = tokenizer.tokenize(specification.contraction, sentenceSplit = false).head
      val wordString = sentence.words.mkString(" ")
      val rawString = sentence.raw.mkString(" ")

      specification.contraction.print("", "", ": ")
      sentence.words.println(", ")

       should be (specification.tokenString)
    }
  }

  it should "work on its own" in {
    specifications.foreach { specification =>
      val inputs = Array(RawToken(specification.contraction, 0))
      val outputs = tokenizerStepContractions.process(inputs)

      specification.contraction.println("", "", ": ")
      outputs.map(_.word).println("words: ", ", ", "")
      outputs.map(_.raw).println("raws: ", ", ", "")

      val wordString = outputs.map(_.word).mkString(" ")
      val rawString = outputs.map(_.raw).mkString(" ")

      wordString should be (specification.wordString)
      rawString should be (specification.rawString)
    }
  }

  it should "work quickly" in {

  }

  /*
  https://www.grammarly.com/blog/contractions/
  https://en.wikipedia.org/wiki/Wikipedia:List_of_English_contractions

  // where's could mean where does, apparently?

  in aren't I, means am I not?

  let's -> not possessive, us
  here's -> not possessive, is

  there's -> not possessive (their), is or has
  that's -> possibly possessive, is or maybe has, it's that's thing, not your thing
  this's -> not possessive, is or has

  what's -> is or has
  how's -> not possessive, is or has
  who's -> otherwise whose, is or has, or does!
  when's -> not possessive, is or has
  where's -> not possessive
  which's -> not possessive
  why's -> not possessive or does!

  he's -> otherwise his, is or has
  she's -> otherwise her, is or has
  it's -> otherwise its, is or has

  somebody's, is or has, can it be possessive?
  someone's can be possessive
  something's
  one's can be possessive

  ain't -> multiple
  isn't
  aren't
  wasn't
  won't -> will not
  weren't

  didn't
  don't
  doesn't

  can't -> can not

  shouldn't
  couldn't
  wouldn't
  hasn't

  I'm

  you're
  we're
  they're
  who're

  I'll
  you'll will or shall
  it'll
  he'll
  she'll
  we'll

  I've
  you've
  we've
  they've
  should've
  could've

  I'd  // would or had
  you'd
  he'd
  she'd
  we'd
  they'd
  how'd
  that'd would only, not had
  what'd would only? what'd you done?
  who'd did, had, or would

   */
}
