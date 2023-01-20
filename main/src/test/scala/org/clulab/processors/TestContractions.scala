package org.clulab.processors

import org.clulab.processors.clu.tokenizer.{EnglishSentenceSplitter, OpenDomainEnglishLexer, Tokenizer}
import org.clulab.utils.PrintUtils._
import org.clulab.utils.Test

class TestContractions extends Test {

  // Add expected result to this
  case class Specification(word: String)

  val specifications = Array(
    Specification("let's"), //
    Specification("here's"), //  -> not possessive, is
    Specification("that's"), //  -> possibly possessive, is or maybe has, it's that's thing, not your thing
    Specification("there's"), //  -> not possessive (their), is or has
    Specification("what's"), //  -> is or has
    Specification("how's"), //  -> not possessive, is or has
    Specification("he's"), //  -> otherwise his, is or has
    Specification("it's"), //  -> otherwise its, is or has
    Specification("she's"), //  -> otherwise her, is or has
    Specification("who's"), //  -> otherwise whose, is or has
    Specification("this's"), //  -> not possessive, is or has
    Specification("when's"), //  -> not possessive, is or has
    Specification("when's"), //  -> not possessive, is or has
    Specification("where's"), //  -> not possessive
    Specification("which's"), //  -> not possessive
    Specification("why's"), //  -> not possessive
    Specification("somebody's"), // , is or has, can it be possessive?
    Specification("someone's"), //  can be possessive
    Specification("something's"), //
    Specification("one's"), //  can be possessive

    Specification("ain't"), //  -> multiple
    Specification("didn't"), //
    Specification("don't"), //
    Specification("doesn't"), //
    Specification("can't"), //  -> can not
    Specification("isn't"), //
    Specification("aren't"), //
    Specification("shouldn't"), //
    Specification("couldn't"), //
    Specification("wouldn't"), //
    Specification("hasn't"), //
    Specification("wasn't"), //
    Specification("won't"), //  -> will not
    Specification("weren't"), //

    Specification("I'm"), //

    Specification("you're"), //
    Specification("we're"), //
    Specification("they're"), //
    Specification("who're"), //

    Specification("I'll"), //
    Specification("we'll"), //
    Specification("you'll"), //
    Specification("it'll"), //
    Specification("she'll"), //

    Specification("I've"), //
    Specification("should've"), //
    Specification("you've"), //
    Specification("could've"), //
    Specification("they've"), //
    Specification("we've"), //

    Specification("I'd"), //   // would or had
    Specification("they'd"), //
    Specification("you'd"), //
    Specification("we'd"), //
    Specification("he'd"), //
    Specification("she'd"), //
    Specification("how'd"), //
    Specification("that'd"), //  would only, not had
    Specification("what'd"), //  would only? what'd you done?
    Specification("who'd") //  did, had, or would
  )

  val tokenizer = new Tokenizer(new OpenDomainEnglishLexer, Seq.empty, new EnglishSentenceSplitter)

  behavior of "TokenizerStepContractions"

  it should "run" in {
    specifications.foreach { specification =>
      val sentence = tokenizer.tokenize(specification.word, sentenceSplit = false).head

      specification.word.print("", "", ": ")
      sentence.words.println(", ")
    }
  }

  /*
  https://www.grammarly.com/blog/contractions/
  https://en.wikipedia.org/wiki/Wikipedia:List_of_English_contractions

  in aren't I, means am not I?

  let's -> not possessive, us
  here's -> not possessive, is

  there's -> not possessive (their), is or has
  that's -> possibly possessive, is or maybe has, it's that's thing, not your thing
  this's -> not possessive, is or has

  what's -> is or has
  how's -> not possessive, is or has
  who's -> otherwise whose, is or has
  when's -> not possessive, is or has
  where's -> not possessive
  which's -> not possessive
  why's -> not possessive

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
  you'll
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
