package org.clulab.processors

import org.clulab.processors.clu.tokenizer.{TokenizerStepPortugueseContractions, RawToken}
import org.scalatest.{FlatSpec, Matchers}
import scala.collection.mutable.ArrayBuffer

/**
  *
  * User: gcgbarbosa
  * Date: 8/1/18
  */
class TestPortugueseTokenizerStepContractions extends FlatSpec with Matchers {
  "the step contratctions tokenizer component" should "expand contractions correctly" in {
    var sents = getContractedForm("fuja dela")
    sents.size should be (3)
    sents(0).word should be ("fuja")
    sents(1).word should be ("de")
    sents(2).word should be ("ela")

    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (5)
    sents(2).beginPosition should be (6)
  }

  it should "work for do, da, dos, das, dum, duma, duns, and dumas" in {
    var sents = getContractedForm("do")
    sents(0).word should be ("de")
    sents(1).word should be ("o")
    sents(0).beginPosition should be (0)
    sents(0).endPosition should be (1)
    sents(1).beginPosition should be (1)
    sents(1).endPosition should be (2)

    sents = getContractedForm("da")
    sents(0).word should be ("de")
    sents(1).word should be ("a")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)

    sents = getContractedForm("dos")
    sents(0).word should be ("de")
    sents(1).word should be ("os")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)

    sents = getContractedForm("das")
    sents(0).word should be ("de")
    sents(1).word should be ("as")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)

    sents = getContractedForm("dum")
    sents(0).word should be ("de")
    sents(1).word should be ("um")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)

    sents = getContractedForm("duma")
    sents(0).word should be ("de")
    sents(1).word should be ("uma")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)

    sents = getContractedForm("duns")
    sents(0).word should be ("de")
    sents(1).word should be ("uns")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)

    sents = getContractedForm("dumas")
    sents(0).word should be ("de")
    sents(1).word should be ("umas")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)
  }

  it should "work for dele, dela, deles, and delas" in {
    var sents = getContractedForm("dele")
    sents(0).word should be ("de")
    sents(1).word should be ("ele")
    sents(0).beginPosition should be (0)
    sents(0).endPosition should be (1)
    sents(1).beginPosition should be (1)
    sents(1).endPosition should be (4)

    sents = getContractedForm("dela")
    sents(0).word should be ("de")
    sents(1).word should be ("ela")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)

    sents = getContractedForm("deles")
    sents(0).word should be ("de")
    sents(1).word should be ("eles")
    sents(0).beginPosition should be (0)
    sents(0).endPosition should be (1)
    sents(1).beginPosition should be (1)
    sents(1).endPosition should be (5)

    sents = getContractedForm("delas")
    sents(0).word should be ("de")
    sents(1).word should be ("elas")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)
  }

  it should "work for deste, desta, destes, destas, desse, dessa, desses, dessas" in {
    var sents = getContractedForm("deste")
    sents(0).word should be ("de")
    sents(1).word should be ("este")
    sents(0).beginPosition should be (0)
    sents(0).endPosition should be (1)
    sents(1).beginPosition should be (1)
    sents(1).endPosition should be (5)

    sents = getContractedForm("desta")
    sents(0).word should be ("de")
    sents(1).word should be ("esta")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)

    sents = getContractedForm("destes")
    sents(0).word should be ("de")
    sents(1).word should be ("estes")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)

    sents = getContractedForm("destas")
    sents(0).word should be ("de")
    sents(1).word should be ("estas")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)
    
    sents = getContractedForm("desse")
    sents(0).word should be ("de")
    sents(1).word should be ("esse")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)

    sents = getContractedForm("dessa")
    sents(0).word should be ("de")
    sents(1).word should be ("essa")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)

    sents = getContractedForm("desses")
    sents(0).word should be ("de")
    sents(1).word should be ("esses")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)

    sents = getContractedForm("dessas")
    sents(0).word should be ("de")
    sents(1).word should be ("essas")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)
  }

  it should "work for daquele, daquela, daqueles, daquelas" in {
    var sents = getContractedForm("daquele")
    sents(0).word should be ("de")
    sents(1).word should be ("aquele")
    sents(0).beginPosition should be (0)
    sents(0).endPosition should be (1)
    sents(1).beginPosition should be (1)
    sents(1).endPosition should be (7)

    sents = getContractedForm("daquela")
    sents(0).word should be ("de")
    sents(1).word should be ("aquela")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)

    sents = getContractedForm("daqueles")
    sents(0).word should be ("de")
    sents(1).word should be ("aqueles")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)

    sents = getContractedForm("daquelas")
    sents(0).word should be ("de")
    sents(1).word should be ("aquelas")
    sents(0).beginPosition should be (0) 
    sents(1).beginPosition should be (1)
  }

  it should "work for no, na, nos nas" in {
    var sents = getContractedForm("no")
    sents(0).word should be ("em")
    sents(1).word should be ("o")
    sents(0).beginPosition should be (0)
    sents(0).endPosition should be (1)
    sents(1).beginPosition should be (1)
    sents(1).endPosition should be (2)

    sents = getContractedForm("na")
    sents(0).word should be ("em")
    sents(1).word should be ("a")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)

    sents = getContractedForm("nos")
    sents(0).word should be ("em")
    sents(1).word should be ("os")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)
    
    sents = getContractedForm("nas")
    sents(0).word should be ("em")
    sents(1).word should be ("as")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)
  }


  it should "work for num, numa, nuns, numas" in {
    var sents = getContractedForm("num")
    sents(0).word should be ("em")
    sents(1).word should be ("um")
    sents(0).beginPosition should be (0)
    sents(0).endPosition should be (1)
    sents(1).beginPosition should be (1)
    sents(1).endPosition should be (3)

    sents = getContractedForm("numa")
    sents(0).word should be ("em")
    sents(1).word should be ("uma")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)

    sents = getContractedForm("nuns")
    sents(0).word should be ("em")
    sents(1).word should be ("uns")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)
    
    sents = getContractedForm("numas")
    sents(0).word should be ("em")
    sents(1).word should be ("umas")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)
  }


  it should "work for nele, nela, neles, nelas" in {
    var sents = getContractedForm("nele")
    sents(0).word should be ("em")
    sents(1).word should be ("ele")
    sents(0).beginPosition should be (0)
    sents(0).endPosition should be (1)
    sents(1).beginPosition should be (1)
    sents(1).endPosition should be (4)

    sents = getContractedForm("nela")
    sents(0).word should be ("em")
    sents(1).word should be ("ela")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)

    sents = getContractedForm("neles")
    sents(0).word should be ("em")
    sents(1).word should be ("eles")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)
    
    sents = getContractedForm("nelas")
    sents(0).word should be ("em")
    sents(1).word should be ("elas")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)
  }

  it should "work for neste, nesta, nestes, nestas" in {
    var sents = getContractedForm("neste")
    sents(0).word should be ("em")
    sents(1).word should be ("este")
    sents(0).beginPosition should be (0)
    sents(0).endPosition should be (1)
    sents(1).beginPosition should be (1)
    sents(1).endPosition should be (5)

    sents = getContractedForm("nesta")
    sents(0).word should be ("em")
    sents(1).word should be ("esta")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)

    sents = getContractedForm("nestes")
    sents(0).word should be ("em")
    sents(1).word should be ("estes")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)
    
    sents = getContractedForm("nestas")
    sents(0).word should be ("em")
    sents(1).word should be ("estas")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)
  }

  it should "work for nesse, nessa, nesses, nessas" in {
    var sents = getContractedForm("nesse")
    sents(0).word should be ("em")
    sents(1).word should be ("esse")
    sents(0).beginPosition should be (0)
    sents(0).endPosition should be (1)
    sents(1).beginPosition should be (1)
    sents(1).endPosition should be (5)

    sents = getContractedForm("nessa")
    sents(0).word should be ("em")
    sents(1).word should be ("essa")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)

    sents = getContractedForm("nesses")
    sents(0).word should be ("em")
    sents(1).word should be ("esses")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)
    
    sents = getContractedForm("nessas")
    sents(0).word should be ("em")
    sents(1).word should be ("essas")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)
  }

  it should "work for aquele, aquela, aqueles, aquelas" in {
    var sents = getContractedForm("naquele")
    sents(0).word should be ("em")
    sents(1).word should be ("aquele")
    sents(0).beginPosition should be (0)
    sents(0).endPosition should be (1)
    sents(1).beginPosition should be (1)
    sents(1).endPosition should be (7)

    sents = getContractedForm("naquela")
    sents(0).word should be ("em")
    sents(1).word should be ("aquela")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)

    sents = getContractedForm("naqueles")
    sents(0).word should be ("em")
    sents(1).word should be ("aqueles")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)
    
    sents = getContractedForm("naquelas")
    sents(0).word should be ("em")
    sents(1).word should be ("aquelas")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (1)
  }

  it should "work for ao, à, aos, às" in {
    var sents = getContractedForm("ao")
    sents(0).word should be ("a")
    sents(1).word should be ("o")
    sents(0).beginPosition should be (0)
    sents(0).endPosition should be (1)
    sents(1).beginPosition should be (1)
    sents(1).endPosition should be (2)

    sents = getContractedForm("à")
    sents(0).word should be ("a")
    sents(1).word should be ("a")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (0)

    sents = getContractedForm("aos")
    sents(0).word should be ("a")
    sents(1).word should be ("os")
    sents(0).beginPosition should be (0)
    sents(0).endPosition should be (1)
    sents(1).beginPosition should be (1)
    sents(1).endPosition should be (3)
    
    sents = getContractedForm("às")
    sents(0).word should be ("a")
    sents(1).word should be ("as")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (0)
  }

  it should "work for àquele, àquela, àqueles, àquelas" in {
    var sents = getContractedForm("àquele")
    sents(0).word should be ("a")
    sents(1).word should be ("aquele")
    sents(0).beginPosition should be (0)
    sents(0).endPosition should be (1)
    sents(1).beginPosition should be (0)
    sents(1).endPosition should be (6)

    sents = getContractedForm("àquela")
    sents(0).word should be ("a")
    sents(1).word should be ("aquela")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (0)

    sents = getContractedForm("àqueles")
    sents(0).word should be ("a")
    sents(1).word should be ("aqueles")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (0)
    
    sents = getContractedForm("àquelas")
    sents(0).word should be ("a")
    sents(1).word should be ("aquelas")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (0)
  }

  it should "work for pelo, pela, pelos, pelas" in {
    var sents = getContractedForm("pelo")
    sents(0).word should be ("por")
    sents(1).word should be ("o")
    sents(0).beginPosition should be (0)
    sents(0).endPosition should be (3)
    sents(1).beginPosition should be (3)
    sents(1).endPosition should be (4)

    sents = getContractedForm("pela")
    sents(0).word should be ("por")
    sents(1).word should be ("a")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (3)

    sents = getContractedForm("pelos")
    sents(0).word should be ("por")
    sents(1).word should be ("os")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (3)
    
    sents = getContractedForm("pelas")
    sents(0).word should be ("por")
    sents(1).word should be ("as")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (3)
  }

  it should "work for doutro" in {
    var sents = getContractedForm("doutro")
    sents(0).word should be ("de")
    sents(1).word should be ("outro")
    sents(0).beginPosition should be (0)
    sents(0).endPosition should be (1)
    sents(1).beginPosition should be (1)
    sents(1).endPosition should be (6)
  }

  it should "work for noutro" in {
    var sents = getContractedForm("noutro")
    sents(0).word should be ("em")
    sents(1).word should be ("outro")
    sents(0).beginPosition should be (0)
    sents(0).endPosition should be (1)
    sents(1).beginPosition should be (1)
    sents(1).endPosition should be (6)
  }

  it should "work for danlgum" in {
    var sents = getContractedForm("dalgum")
    sents(0).word should be ("de")
    sents(1).word should be ("algum")
    sents(0).beginPosition should be (0)
    sents(0).endPosition should be (1)
    sents(1).beginPosition should be (1)
    sents(1).endPosition should be (6)
  }

  it should "work for nalgum" in {
    var sents = getContractedForm("nalgum")
    sents(0).word should be ("em")
    sents(1).word should be ("algum")
    sents(0).beginPosition should be (0)
    sents(0).endPosition should be (1)
    sents(1).beginPosition should be (1)
    sents(1).endPosition should be (6)
  }

  it should "work for donde" in {
    var sents = getContractedForm("donde")
    sents(0).word should be ("de")
    sents(1).word should be ("onde")
    sents(0).beginPosition should be (0)
    sents(0).endPosition should be (1)
    sents(1).beginPosition should be (1)
    sents(1).endPosition should be (5)
  }

  it should "work for mesocliticos with 2 parts" in {
    var sents = getContractedForm("confirma-se")
    sents(0).word should be ("confirma")
    sents(1).word should be ("se")
    sents(0).beginPosition should be (0)
    sents(0).endPosition should be (8)
    sents(1).beginPosition should be (9)
    sents(1).endPosition should be (11)

    sents = getContractedForm("acariciar-lhe")
    sents(0).word should be ("acariciar")
    sents(1).word should be ("lhe")
    sents(0).beginPosition should be (0)
    sents(0).endPosition should be (9)
    sents(1).beginPosition should be (10)
    sents(1).endPosition should be (13)

    sents = getContractedForm("arriscar-me")
    sents(0).word should be ("arriscar")
    sents(1).word should be ("me")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (9)

    sents = getContractedForm("precisamos medi-lo")
    sents(0).word should be ("precisamos")
    sents(1).word should be ("medi")
    sents(2).word should be ("lo")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (11)
    sents(2).beginPosition should be (16)

    sents = getContractedForm("acode-nos")
    sents(0).word should be ("acode")
    sents(1).word should be ("nos")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (6)

    sents = getContractedForm("something-o")
    sents(0).word should be ("something")
    sents(1).word should be ("o")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (10)

    sents = getContractedForm("something-la")
    sents(0).word should be ("something")
    sents(1).word should be ("la")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (10)

    sents = getContractedForm("something-los")
    sents(0).word should be ("something")
    sents(1).word should be ("los")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (10)

    sents = getContractedForm("something-a")
    sents(0).word should be ("something")
    sents(1).word should be ("a")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (10)

    sents = getContractedForm("something-lhes")
    sents(0).word should be ("something")
    sents(1).word should be ("lhes")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (10)

    sents = getContractedForm("something-os")
    sents(0).word should be ("something")
    sents(1).word should be ("os")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (10)

    sents = getContractedForm("something-las")
    sents(0).word should be ("something")
    sents(1).word should be ("las")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (10)

    sents = getContractedForm("something-as")
    sents(0).word should be ("something")
    sents(1).word should be ("as")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (10)

    sents = getContractedForm("something-no")
    sents(0).word should be ("something")
    sents(1).word should be ("no")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (10)

    sents = getContractedForm("something-na")
    sents(0).word should be ("something")
    sents(1).word should be ("na")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (10)

    sents = getContractedForm("something-nas")
    sents(0).word should be ("something")
    sents(1).word should be ("nas")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (10)

    sents = getContractedForm("something-te")
    sents(0).word should be ("something")
    sents(1).word should be ("te")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (10)

    sents = getContractedForm("something-vos")
    sents(0).word should be ("something")
    sents(1).word should be ("vos")
    sents(0).beginPosition should be (0)
    sents(1).beginPosition should be (10)
  }


  it should "work for mesocliticos with 3 parts" in {
    var sents = getContractedForm("ver-se-á")
    sents(0).word should be("verá")
    sents(1).word should be("se")
    sents(0).beginPosition should be(0)
    sents(0).endPosition should be(3)
    sents(1).beginPosition should be(4)
    sents(1).endPosition should be(6)

    sents = getContractedForm("permitir-lhe-ia")
    sents(0).word should be("permitiria")
    sents(1).word should be("lhe")
    sents(0).beginPosition should be(0)
    sents(0).endPosition should be(8)
    sents(1).beginPosition should be(9)
    sents(1).endPosition should be(12)

    // this needs a lemmatizer to work
//    sents = getContractedForm("torná-la-ia")
//    sents(0).word should be("tornaria")
//    sents(1).word should be("me")
//    sents(0).beginPosition should be(0)
//    sents(1).beginPosition should be(6)

    sents = getContractedForm("parecer-me-ia")
    sents(0).word should be("pareceria")
    sents(1).word should be("me")
    sents(0).beginPosition should be(0)
    sents(1).beginPosition should be(8)

    sents = getContractedForm("seguir-se-ia")
    sents(0).word should be("seguiria")
    sents(1).word should be("se")
    sents(0).beginPosition should be(0)
    sents(1).beginPosition should be(7)

    sents = getContractedForm("juntar-se-ão")
    sents(0).word should be("juntarão")
    sents(1).word should be("se")
    sents(0).beginPosition should be(0)
    sents(1).beginPosition should be(7)

    // this is a dictionary based example
//    sents = getContractedForm("ver-se-é")
//    sents(0).word should be("verá")
//    sents(1).word should be("se")
//    sents(0).beginPosition should be(0)
//    sents(1).beginPosition should be(4)
  }

  def getContractedForm(s: String): Array[RawToken] = {
    // fake lexer
    // split string
    val tokensString = s.split(" ")
    var tokensRaw = new ArrayBuffer[RawToken]()
    var stringSize = 0
    // for each word in string
    for(token <- tokensString){
      // create a RawToken
      //val rawToken = new RawToken()
      val tokenSize = token.length()
      tokensRaw += RawToken(token, stringSize)
      // +1 here accounts for spaces in the sentence
      stringSize = stringSize + tokenSize + 1
      // fill the start position of the raw token
    }
    // use StepContractions PT class to get contractions
    val tokenizer = new TokenizerStepPortugueseContractions()    
    val processedTokens = tokenizer.process(tokensRaw.to[Array])

    return(processedTokens)
  }
}
