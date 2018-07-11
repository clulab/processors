package org.clulab.processors.clu.tokenizer

import scala.collection.mutable.ArrayBuffer

/**
  * Resolves Portugese contractions
  * Author: dane
  * Author: mihais
  * Date: 7/10/2018
  */
class TokenizerStepPortugueseContractions extends TokenizerStep {
  override def process(inputs:Array[RawToken]): Array[RawToken] = {
    //
    // We must separate important linguistic constructs here
    // TODO: this is slow. This should be handled in the Antlr grammar
    //

    val tokens = new ArrayBuffer[RawToken]()

    for(input <- inputs) {

      if("""(?i)^do$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "o")
      }
      else if("""(?i)^da$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "a")
      }
      else if("""(?i)^dos$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "os")
      }
      else if("""(?i)^das$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "as")
      }
      else if("""(?i)^dum$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "um")
      }
      else if("""(?i)^duma$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "uma")
      }
      else if("""(?i)^duns$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "uns")
      }
      else if("""(?i)^dumas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "umas")
      }
      else if("""(?i)^dele$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "ele")
      }
      else if("""(?i)^dela$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "ela")
      }
      else if("""(?i)^deles$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "eles")
      }
      else if("""(?i)^delas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "delas")
      }
      else if("""(?i)^deste$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "este")
      }
      else if("""(?i)^desta$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "esta")
      }
      else if("""(?i)^destes$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "estes")
      }
      else if("""(?i)^destas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "estas")
      }
      else if("""(?i)^desse$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "esse")
      }
      else if("""(?i)^dessa$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "essa")
      }
      else if("""(?i)^desses$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "esses")
      }
      else if("""(?i)^dessas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "essas")
      }
      else if("""(?i)^daquele$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "aquele")
      }
      else if("""(?i)^daquela$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "aquela")
      }
      else if("""(?i)^daqueles$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "aqueles")
      }
      else if("""(?i)^daquelas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "de")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "aquelas")
      }
      else if("""(?i)^no$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "o")
      }
      else if("""(?i)^na$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "a")
      }
      else if("""(?i)^nos$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "os")
      }
      else if("""(?i)^nas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "as")
      }
      else if("""(?i)^num$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "um")
      }
      else if("""(?i)^numa$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "uma")
      }
      else if("""(?i)^nuns$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "uns")
      }
      else if("""(?i)^numas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "umas")
      }
      else if("""(?i)^nele$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "ele")
      }
      else if("""(?i)^nela$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "ela")
      }
      else if("""(?i)^neles$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "eles")
      }
      else if("""(?i)^nelas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "elas")
      }
      else if("""(?i)^neste$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "este")
      }
      else if("""(?i)^nesta$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "esta")
      }
      else if("""(?i)^nestes$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "estes")
      }
      else if("""(?i)^nestas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "estas")
      }
      else if("""(?i)^nesse$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "esse")
      }
      else if("""(?i)^nessa$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "essa")
      }
      else if("""(?i)^nesses$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "esses")
      }
      else if("""(?i)^nessas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "essas")
      }
      else if("""(?i)^naquele$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "aquele")
      }
      else if("""(?i)^naquela$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "aquela")
      }
      else if("""(?i)^naqueles$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "naqueles")
      }
      else if("""(?i)^naquelas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "em")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "naquelas")
      }
      else if("""(?i)^ao$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "a")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "o")
      }
      else if("""(?i)^à$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "a")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "a")
      }
      else if("""(?i)^aos$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "a")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "os")
      }
      else if("""(?i)^às$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "a")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "as")
      }
      else if("""(?i)^àquele$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "a")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "aquele")
      }
      else if("""(?i)^àquela$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "a")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "aquela")
      }
      else if("""(?i)^àqueles$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "a")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "aqueles")
      }
      else if("""(?i)^àquelas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "a")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "aquelas")
      }
      else if("""(?i)^pelo$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "por")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "o")
      }
      else if("""(?i)^pela$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "por")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "a")
      }
      // TODO: Sometimes 'pelos' means 'por' + 'eles'
      else if("""(?i)^pelos$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "por")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "os")
      }
      // TODO: Sometimes 'pelas' means 'por' + 'elas'
      else if("""(?i)^pelas$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, "por")
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "as")
      }
      // any other token
      else {
        tokens += input
      }
    }

    tokens.toArray
  }
}

