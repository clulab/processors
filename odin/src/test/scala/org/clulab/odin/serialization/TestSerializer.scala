package org.clulab.odin.serialization

import org.clulab.TestUtils.jsonStringToDocument
import org.clulab.odin.ExtractorEngine
import org.scalatest._

// See TestJSONSerializer for the test upon which this is based.
class TestSerializer extends FlatSpec with Matchers {

  object Serializer {
    import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream, ObjectStreamClass}
  
    def serialize(anyOut: Any): Boolean = {
      val streamOut = new ByteArrayOutputStream()
      val encoder = new ObjectOutputStream(streamOut)
      encoder.writeObject(anyOut)
  
      val bytes = streamOut.toByteArray
      val streamIn = new ByteArrayInputStream(bytes)
      val decoder = new ObjectInputStream(streamIn) {
        // See https://stackoverflow.com/questions/16386252/scala-deserialization-class-not-found
        override def resolveClass(desc: ObjectStreamClass): Class[_] = {
          try {
            Class.forName(desc.getName, false, getClass.getClassLoader)
          }
          catch {
            case ex: ClassNotFoundException => super.resolveClass(desc)
          }
        }
      }
      val anyIn = decoder.readObject()
      decoder.close()
      anyIn == anyOut
    }
  }
      
  {
    val rules =
      """
        |# NE rules
        |
        |- name: "ner-person"
        |  label: [Person, PossiblePerson, Entity]
        |  priority: 1
        |  type: token
        |  pattern: |
        |   ([entity="PERSON"]+ | "Gonzo")
        |
        |# Events
        |
        |# optional location and date
        |- name: "marry-syntax-1"
        |  label: [Marry, Event]
        |  priority: 3
        |  example: "Gonzo married Camilla."
        |  type: dependency
        |  pattern: |
        |    trigger = [lemma="marry"]
        |    spouse: Entity+ = <xcomp? /^nsubj/ | dobj
      """.stripMargin
    val engine = ExtractorEngine(rules)
    val text = "Gonzo married Camilla."
    val doc = jsonStringToDocument(""" {"sentences":[{"words":["Gonzo","married","Camilla","."],"startOffsets":[0,6,14,21],"endOffsets":[5,13,21,22],"tags":["NNP","VBD","NNP","."],"lemmas":["Gonzo","marry","Camilla","."],"entities":["O","O","PERSON","O"],"norms":["O","O","O","O"],"chunks":["B-NP","B-VP","B-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"dobj"},{"source":1,"destination":3,"relation":"punct"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"dobj"},{"source":1,"destination":3,"relation":"punct"}],"roots":[1]}}}]} """)
    val mentions = engine.extractFrom(doc)
    
    it should "serialize/deserialize a Mention correctly " in {
      Serializer.serialize(mentions) should be (true)
    }
  }

  {
    val rule =
      """
        |# NE rules
        |
        |- name: "anaphor"
        |  label: Anaphor
        |  priority: 1
        |  keep: false
        |  type: token
        |  pattern: |
        |   [lemma=he]
        |
        |- name: "el-presidente"
        |  label: Entity
        |  keep: false
        |  type: token
        |  pattern: |
        |    "Barack"? "Obama"
        |
        |- name: coref-example
        |  label: Coref
        |  priority: 2
        |  # the maximum number of sentences to search to the left of the anchor
        |  left-window: 1
        |  # don't attempt to match anywhere to the right of the anchor
        |  example: "Barack Obama is the 44th President of the United States. He was born on August 4, 1961 in Honolulu, Hawaii."
        |  type: cross-sentence
        |  pattern: |
        |    # start here at the pattern's anchor
        |    # the pronoun to be resolved
        |    anaphor: Anaphor = @Anaphor
        |    antecedent: Entity = @Entity
      """.stripMargin
    val ee = ExtractorEngine(rule)
    val text = "Barack Obama is the 44th President of the United States. He was born on August 4, 1961 in Honolulu, Hawaii."
    val doc = jsonStringToDocument(""" {"sentences":[{"words":["Barack","Obama","is","the","44th","President","of","the","United","States","."],"startOffsets":[0,7,13,16,20,25,35,38,42,49,55],"endOffsets":[6,12,15,19,24,34,37,41,48,55,56],"tags":["NNP","NNP","VBZ","DT","JJ","NN","IN","DT","NNP","NNPS","."],"lemmas":["Barack","Obama","be","the","44th","president","of","the","United","States","."],"entities":["PERSON","PERSON","O","O","ORDINAL","O","O","O","LOCATION","LOCATION","O"],"norms":["O","O","O","O","44.0","O","O","O","O","O","O"],"chunks":["B-NP","I-NP","B-VP","B-NP","I-NP","I-NP","B-PP","B-NP","I-NP","I-NP","O"],"graphs":{}},{"words":["He","was","born","on","August","4",",","1961","in","Honolulu",",","Hawaii","."],"startOffsets":[57,60,64,69,72,79,80,82,87,90,98,100,106],"endOffsets":[59,63,68,71,78,80,81,86,89,98,99,106,107],"tags":["PRP","VBD","VBN","IN","NNP","CD",",","CD","IN","NNP",",","NNP","."],"lemmas":["he","be","bear","on","August","4",",","1961","in","Honolulu",",","Hawaii","."],"entities":["O","O","O","O","DATE","DATE","DATE","DATE","O","LOCATION","O","LOCATION","O"],"norms":["O","O","O","O","1961-08-04","1961-08-04","1961-08-04","1961-08-04","O","O","O","O","O"],"chunks":["B-NP","B-VP","I-VP","B-PP","B-NP","I-NP","O","B-NP","B-PP","B-NP","O","B-NP","O"],"graphs":{}}]} """)
    val mentions = ee.extractFrom(doc)
    
    it should "serialize/deserialize a CrossSentenceMention correctly" in {
      Serializer.serialize(mentions) should be (true)
    }
  }
}
