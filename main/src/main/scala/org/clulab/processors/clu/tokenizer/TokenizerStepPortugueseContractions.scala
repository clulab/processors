package org.clulab.processors.clu.tokenizer

import scala.collection.mutable.ArrayBuffer

/**
  * Resolves Portugese contractions
  * Author: dane
  * Author: mihais
  * Date: 7/10/2018
  */
class TokenizerStepPortugueseContractions extends TokenizerStep {
  // is this word ALL CAPS, Sentence Caps, or lowercase?
  protected def cap(s: String): String = {
    val letters = s.filter(_.isLetter)
    letters match {
      case "" => "lower"
      case lower if !letters.head.isUpper => "lower"
      case upper if letters.length > 1 && letters.forall(c => !c.isLower) => "upper"
      case sentenceCaps if !letters.head.isLower => "sentence"
      case _ => "lower"
    }
  }

  //
  protected def matchCase(source: String, target: String): String = cap(source) match {
    case "lower" => target.toLowerCase
    case "sentence" => target.head.toUpper +: target.tail
    case "upper" => target.toUpperCase
  }

  override def process(inputs:Array[RawToken]): Array[RawToken] = {
    //
    // We must separate important linguistic constructs here
    // TODO: this is slow. This should be handled in the Antlr grammar
    //

    val tokens = new ArrayBuffer[RawToken]()

    for(input <- inputs) {
      //  TODO: change IFs to a MATCH (improve readability)
      if("""(?i)^do$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "o")
      }
      else if("""(?i)^da$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "a")
      }
      else if("""(?i)^dos$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "os")
      }
      else if("""(?i)^das$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "as")
      }
      else if("""(?i)^dum$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "um")
      }
      else if("""(?i)^duma$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "uma")
      }
      else if("""(?i)^duns$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "uns")
      }
      else if("""(?i)^dumas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "umas")
      }
      else if("""(?i)^dele$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "ele")
      }
      else if("""(?i)^dela$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "ela")
      }
      else if("""(?i)^deles$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "eles")
      }
      else if("""(?i)^delas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "elas")
      }
      else if("""(?i)^deste$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "este")
      }
      else if("""(?i)^desta$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "esta")
      }
      else if("""(?i)^destes$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "estes")
      }
      else if("""(?i)^destas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "estas")
      }
      else if("""(?i)^desse$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "esse")
      }
      else if("""(?i)^dessa$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "essa")
      }
      else if("""(?i)^desses$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "esses")
      }
      else if("""(?i)^dessas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "essas")
      }
      else if("""(?i)^daquele$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "aquele")
      }
      else if("""(?i)^daquela$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "aquela")
      }
      else if("""(?i)^daqueles$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "aqueles")
      }
      else if("""(?i)^daquelas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "aquelas")
      }
      else if("""(?i)^no$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "o")
      }
      else if("""(?i)^na$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "a")
      }
      else if("""(?i)^nos$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "os")
      }
      else if("""(?i)^nas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "as")
      }
      else if("""(?i)^num$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "um")
      }
      else if("""(?i)^numa$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "uma")
      }
      else if("""(?i)^nuns$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "uns")
      }
      else if("""(?i)^numas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "umas")
      }
      else if("""(?i)^nele$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "ele")
      }
      else if("""(?i)^nela$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "ela")
      }
      else if("""(?i)^neles$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "eles")
      }
      else if("""(?i)^nelas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "elas")
      }
      else if("""(?i)^neste$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "este")
      }
      else if("""(?i)^nesta$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "esta")
      }
      else if("""(?i)^nestes$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "estes")
      }
      else if("""(?i)^nestas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "estas")
      }
      else if("""(?i)^nesse$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "esse")
      }
      else if("""(?i)^nessa$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "essa")
      }
      else if("""(?i)^nesses$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "esses")
      }
      else if("""(?i)^nessas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "essas")
      }
      else if("""(?i)^naquele$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "aquele")
      }
      else if("""(?i)^naquela$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "aquela")
      }
      else if("""(?i)^naqueles$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "aqueles")
      }
      else if("""(?i)^naquelas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "aquelas")
      }
      else if("""(?i)^ao$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "a"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "o")
      }
      else if("""(?i)^à$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "a"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "a")
      }
      else if("""(?i)^aos$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "a"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "os")
      }
      else if("""(?i)^às$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "a"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "as")
      }
      // àquele -> a aquele
      // àquela -> a aquela
      // àqueles -> a aqueles
      // àquelas -> a aquelas
      else if("""(?i)^àquel(e|a|es|as)$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "a"))
        tokens += RawToken(input.raw, input.beginPosition, "a" + input.raw.substring(1))
      }
      // pelo -> por o
      // pela -> por a
      // pelos -> por  os
      // pelas -> por as
      else if("""(?i)^pel(os|as|o|a)$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 3), input.beginPosition, matchCase(input.raw, "por"))
        tokens += RawToken(input.raw.substring(3), input.beginPosition+3, input.endPosition, input.raw.substring(3))
      }
      // doutros -> de outros
      // doutras -> de outras
      // doutra -> de outra
      // doutro -> de outro
      else if("""(?i)^doutr(os|as|o|a)$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, input.raw.substring(1))
      }
      // noutras -> em outras
      // noutros -> em outros
      // noutra -> em outra
      // noutro -> em outro
      else if("""(?i)^noutr(os|as|o|a)$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, input.raw.substring(1))
      }      
      // dalguns -> de alguns
      // dalgumas -> de algumas
      // dalguma -> de alguma
      // dalgum -> de algum
      // dalguém -> de alguém
      // dali -> de ali
      else if("""(?i)^dal(guns|gumas|guma|gum|guém|i)$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, input.raw.substring(1))
      }
      // nalguns - em - alguns
      // nalgumas - em algumas
      else if("""(?i)^nal(guns|gumas|gum|guma)$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, input.raw.substring(1))
      }
      // donde - de onde
      else if("""(?i)^donde$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "onde")
      }
      // TODO:
      // doutros -> de outros
      // doutras -> de outras
      // doutra -> de outra
      // doutro -> de outro
      else if("""(?i)^doutr(os|as|o|a)$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, input.raw.substring(1))
      }
      // noutras -> em outras
      // noutros -> em outros
      // noutra -> em outra
      // noutro -> em outro
      else if("""(?i)^noutr(os|as|o|a)$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, input.raw.substring(1))
      }      
      // dalguns -> de alguns
      // dalgumas -> de algumas
      // dalguma -> de alguma
      // dalgum -> de algum
      // dalguém -> de alguém
      // dali -> de ali
      else if("""(?i)^dal(guns|gumas|guma|gum|guém|i)$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, input.raw.substring(1))
      }
      // nalguns - em - alguns
      // nalgumas - em algumas
      else if("""(?i)^nal(guns|gumas)$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "em"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, input.raw.substring(1))
      }
      // donde - de onde
      else if("""(?i)^donde$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition+1, "onde")
      }
      // mesoclises 3 parts
      // 3 parts has to be executed first than 2 parts otherwise 2 parts will overmach some tokens
      else if("""(?i)-(se-á|lhe-ia|la-ia|me-ia|se-ia|se-ão|se-é)$""".r.findFirstIn(input.raw).isDefined) {
        // get the head word
        val tokenNoMesoc = input.raw.replaceAll("""(?i)-(se-á|lhe-ia|la-ia|me-ia|se-ia|se-ão|se-é)$""", "")
        // get extra two parts of the mesoc
        val mesocParts = input.raw.substring(tokenNoMesoc.length+1).split("-")
        //
        tokens += RawToken(input.raw.substring(0, tokenNoMesoc.length), input.beginPosition, tokenNoMesoc+mesocParts(1))
        tokens += RawToken(input.raw.substring(tokenNoMesoc.length+1, tokenNoMesoc.length+1+mesocParts(0).length), input.beginPosition + tokenNoMesoc.length + 1 , mesocParts(0))
      }
      // mesoclises 2 parts
      else if("""(?i)(?<!-[a-z]{1,5})-(se|lhe|me|lo|nos|o|la|los|a|lhes|os|las|as|no|na|nas|te|vos)$""".r.findFirstIn(input.raw).isDefined) {
        // get head word
        val tokenNoMesoc = input.raw.replaceAll("""(?i)-(se|lhe|me|lo|nos|o|la|los|a|lhes|os|las|as|no|na|nas|te|vos)$""", "")
        // get mesoc part
        val mesoc = input.raw.substring(tokenNoMesoc.length+1)
        //
        tokens += RawToken(input.raw.substring(0, tokenNoMesoc.length), input.beginPosition, tokenNoMesoc)
        tokens += RawToken(input.raw.substring(tokenNoMesoc.length+1), input.beginPosition+tokenNoMesoc.length+1, mesoc)
      }

      // any other token
      else {
        tokens += input
      }
    }

    tokens.toArray
  }
}
