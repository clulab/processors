package org.clulab.processors

import org.clulab.processors.clu.tokenizer.OpenDomainEnglishTokenizer
import org.scalatest.{FlatSpec, Matchers}

/**
  *
  * User: mihais
  * Date: 3/15/17
  */
class TestTokenizer extends FlatSpec with Matchers {
  "the tokenizer" should "provide fundamental tokenization functionality" in {
    var sents = tok("the of $><1/2 1.3 another test.")
    sents(0).size should be (10)

    sents = tok("-LRB- -RRB- -LCB- -RCB- -LSB- -RSB-.")
    sents(0).size should be (7)

    sents = tok("first sentence. second sentence.")
    sents.length should be (2)

    sents = tok("Artocarpus lacucha Buch.-Ham. ex D.Don is monkey fruit. Second sentence.")
    sents.length should be (2)
    sents = tok("Zizania caduciflora Hand.-Mazz. is a grass. Second sentence.")
    sents.length should be (2)
    sents = tok("Fig. 1 shows the fungus Gaeumannomyces graminis var. tritici (Ggt), a major root disease of wheat.")
    sents.length should be (1)
    sents = tok("Campylobacter spp. are ubiquitous.")
    sents.length should be (1)

    sents = tok("today is 12/25/2017")
    sents(0).size should be (3)
    sents = tok("today is 12/255/2017")
    sents(0).size > 3 should be (true)

    sents = tok("number is -122,333")
    sents(0).size should be (3)

    sents = tok("gap-mediated reaction")
    sents(0).size should be (2)

    sents = tok("3M is a company")
    sents(0).size should be (4)

    sents = tok("cont'd nat'l program")
    sents(0).size should be (3)

    sents = tok("@user_1 likes #havingfun")
    sents(0).size should be (3)

    sents = tok("h1.cpp is a C++ file")
    sents(0).size should be (5)

    sents = tok("http://google.com is Google's home page")
    sents(0).size should be (6)

    sents = tok("so is www.google.com and google.com")
    sents(0).size should be (5)

    sents = tok("won't isn't hadn't")
    sents(0).size should be (6)

    sents = tok("me@privacy.net is a valid email, while me#privacy.net is not.")
    sents(0).size should be (11)

    sents = tok("<start-text>some text</end-text>")
    sents(0).size should be (4)

    sents = tok("IBM is Co. not Ltd.")
    sents(0).size should be (5)

    sents = tok("A.M. Bancorp. A.T.B.") // known abbreviations
    sents(0).size should be (3)

    sents = tok("I'm happy :) not sad :(.")
    sents(0).size should be (8)
  }

  it should "handle contractions correctly" in {
    val sents = tok("I'm won't don't cont'd he's he'd.")
    sents(0).size should be (12)

    sents(0).raw.mkString(" ") should be ("I 'm wo n't do n't cont'd he 's he 'd .")
    sents(0).words.mkString(" ") should be ("I am will not do not cont'd he 's he 'd .")
  }

  it should "transform double quotes into Treebank quotes" in {
    val sents = tok("\"I'm happy\", he said.")
    sents(0).size should be (9)
    sents(0).words(0) should be ("``")
    sents(0).words(4) should be ("''")
  }

  it should "tokenize quotes correctly" in {
    val sent = tok("\"The levels of malnutrition among children continue to be truly alarming,\" said Mahimbo Mdoe, UNICEF's Representative in South Sudan.").head

    sent.raw(0) should be ("\"")
    sent.raw(1) should be ("The")
    sent.raw(12) should be (",")
    sent.raw(13) should be ("\"")
  }

  it should "normalize parentheses correctly" in {
    val sent = tok("This is a test (of parentheses).").head

    sent.raw.length should be (9)
    sent.words(4) should be ("(") // -LRB- if normalization enabled
    sent.words(7) should be (")") // -RRB- if normalization enabled
  }

  it should "not tokenize letter emoticons too aggressively" in {
    var sent = tok("Lecithin :cholesterol and phospholipid :diacylglycerol acyltransferase").head
    sent.raw.length should be (8)
    sent.words(1) should be (":")
    sent.words(5) should be (":")

    sent = tok("ATP :dephospho-CoA triphosphoribosyl transferase").head
    sent.raw.length should be (5)
    sent.words(1) should be (":")

    sent = tok("CDP-Glycerol :Poly (glycerophosphate) glycerophosphotransferase").head
    sent.raw.length should be (7)
    sent.words(1) should be (":")
    sent.words(3) should be ("(")
    sent.words(5) should be (")")

    sent = tok("T=pseudo3 icosahedral viral capsid").head
    sent.raw.length should be (6)
    sent.words(1) should be ("=")

    sent = tok("Myristoyl-CoA :protein N-myristoyltransferase,C-terminal").head
    sent.raw.length should be (6)
    sent.words(1) should be (":")

    sent = tok("I am :), she said").head
    sent.raw.length should be (6)
    sent.words(2) should be (":)")

    sent = tok("I am :( today, she said").head
    sent.raw.length should be (7)
    sent.words(2) should be (":(")
  }

  it should "tokenize question mark correctly" in {
    val sents = tok("An increase in assessed settlements reported access to functional boreholes in November , which can be attributed to the scaling up of humanitarian activities from the WASH cluster to rehabilitate boreholes in Jur River and Wau Counties?17 The overall percentage of functional boreholes increased across WBeG , specifically in Jur River and Wau Counties in November .")
    sents.length should be (2)
    sents(0).words(0) should be ("An")
    sents(1).words(0) should be ("17")
  }

  it should "tokenize punctuation correctly" in {
    val sents = tok("An increase in assessed settlements reported access to functional boreholes in November , which can be attributed to the scaling up of humanitarian activities from the WASH cluster to rehabilitate boreholes in Jur River and Wau Counties.17 The overall percentage of functional boreholes increased across WBeG , specifically in Jur River and Wau Counties in November .")
    sents.length should be (2)
    sents(0).words(0) should be ("An")
    sents(1).words(0) should be ("17")
  }

  it should "tokenize common emoticons correctly" in {
    val orig = """:-) :) :-] :] :-> :> :-} :} :^) =] =) :-)) :-( :( :-< :< :-[ :[ :-|| >:[ :{ :@ >:( :-( :( :-) :) :-* :* ;-) ;) ;-] ;] ;^) :-, :-/ :/ :-. >:\\ >:/ :\\ =/ =\\ :-| :| :$ :-# :# :-& :& >:-) >:) }:-) }:) >;) |;-) <:-| ,:-|"""
    val sents = tok(orig)
    sents.length should be (1)
    val text = sents(0).words.mkString(" ")
    text should be (orig)
  }

  it should "tokenize emoticons with letters correctly" in {
    val orig = """:-3 :3 8-) 8) :-D :D 8-D 8D x-D xD X-D XD =D =3 :-c :c :o) :c) :-O :O :-o :o :-0 8-0 >:O ;D :-P :P X-P XP x-p xp :x :-p :p :-P :P :-b :b =p >:P :L =L :S ://3 :-X :X O:-) O:) 0:-3 0:3 0:-) 0:) 0;^) >:3 >;3 :-J ,:-l"""
    val sents = tok(orig)
    sents.length should be (1)
    val text = sents(0).words.mkString(" ")
    text should be (orig)
  }

  it should "split sentences correctly" in {
    // This is taken from an actual document.
    val orig = """Rapidly deploying as many as 25,000 troops, over 1,000 armored vehicles and a strategy that included the Russian navy taking control of Poti, Georgia’s main port city, 2 some argued that Russia’s actions resembled a well planned offensive strike.3 According to Russian President Dmitry Medvedev, however, Russia’s actions were necessary to “protect . . . the Russian citizens living in [South Ossetia]."""

    val sents = tok(orig)
    sents.length should be > 1

    val startOffsets = sents.flatMap(_.startOffsets)
    val badStartOpt = startOffsets.indices.drop(1).find(i => startOffsets(i) < startOffsets(i - 1))
    badStartOpt should be (empty)

    val endOffsets = sents.flatMap(_.endOffsets)
    val badEndOpt = startOffsets.indices.drop(1).find(i => endOffsets(i) < endOffsets(i - 1))
    badEndOpt should be (empty)

    val raws = sents.flatMap(_.raw)
    val badRawOpt = raws.indices.find(i => orig.slice(startOffsets(i), endOffsets(i)) != raws(i))
    badRawOpt should be (empty)
  }

  it should "split some hyphenated words" in {
    val orig = "Weather is nice in mid-July"
    val sents = tok(orig)
    sents(0).size should be (6)
  }

  def tok(s:String):Array[Sentence] = {
    println(s"Tokenizing text: $s")
    val t = new OpenDomainEnglishTokenizer(None)
    val sents = t.tokenize(s)
    for(i <- sents.indices) {
      println(s"\tSentence #$i: " + sents(i).words.mkString(" "))
    }
    sents
  }
}
