package org.clulab.processors

import org.clulab.processors.clu.tokenizer.{EnglishSentenceSplitter, OpenDomainEnglishLexer, Tokenizer, TokenizerStepContractions}
import org.clulab.utils.PrintUtils._
import org.clulab.utils.Test

class TestContractions extends Test {

  case class Specification(contraction: String, words: String*) {
    val string = words.mkString(" ")
  }

  // TODO avoid double contractions or take off 've and then work on the next part
  // they all seem to be with 've which isn't expanded anyway
  // 'til, 'tis, 'twas

  val specifications = Array(
    Specification("'s", "'", "s"),
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
    Specification("didn't", "did", "not"), //
    Specification("don't", "do", "not"), //
    Specification("doesn't", "does", "not"), //
    Specification("can't", "ca", "not"), //  -> can not, wrong!
    Specification("isn't", "is", "not"), //
    Specification("aren't", "are", "not"), //
    Specification("shouldn't", "should", "not"), //
    Specification("couldn't", "could", "not"), //
    Specification("wouldn't", "would", "not"), //
    Specification("hasn't", "has", "not"), //
    Specification("wasn't", "was", "not"), //
    Specification("won't", "will", "not"), //  -> will not
    Specification("weren't", "were", "not"), //
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

  val tokenizer = new Tokenizer(new OpenDomainEnglishLexer, Seq(new TokenizerStepContractions), new EnglishSentenceSplitter)

  behavior of "TokenizerStepContractions"

  it should "run" in {
    specifications.foreach { specification =>
      val sentence = tokenizer.tokenize(specification.contraction, sentenceSplit = false).head

      specification.contraction.print("", "", ": ")
      sentence.words.println(", ")

      sentence.words.mkString(" ") should be (specification.string)
    }
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
