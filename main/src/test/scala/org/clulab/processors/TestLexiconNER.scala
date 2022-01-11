package org.clulab.processors

import org.clulab.sequences.FileOverrideKbSource
import org.clulab.sequences.FileStandardKbSource
import org.clulab.sequences.LexicalVariations

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import org.clulab.sequences.LexiconNER
import org.clulab.sequences.MemoryOverrideKbSource
import org.clulab.sequences.MemoryStandardKbSource
import org.clulab.sequences.NoLexicalVariations
import org.clulab.sequences.ResourceOverrideKbSource
import org.clulab.sequences.ResourceStandardKbSource
import org.clulab.struct.EntityValidator
import org.clulab.struct.TrueEntityValidator
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.SeqOdometer

import java.io.File

class TestLexiconNER extends FatdynetTest {

  def mkSentence(text: String): Sentence = {
    val doc = proc.mkDocument(text)

    proc.mkConstEmbeddings(doc)
    proc.lemmatize(doc)
    doc.sentences.head
  }

  def reconstitute(ner: LexiconNER): LexiconNER = {

    // See https://stackoverflow.com/questions/39369319/convert-any-type-in-scala-to-arraybyte-and-back.
    def serialize(value: Any): Array[Byte] = {
      val byteArrayOutputStream = new ByteArrayOutputStream()

      new ObjectOutputStream(byteArrayOutputStream).autoClose { objectOutputStream =>
        objectOutputStream.writeObject(value)
      }
      byteArrayOutputStream.toByteArray
    }

    def deserialize(bytes: Array[Byte]): Any = {
      val ner = new ObjectInputStream(new ByteArrayInputStream(bytes)).autoClose { objectInputStream =>
        objectInputStream.readObject
      }

      ner
    }

    val nerBytes = serialize(ner)
    val reconstitutedNer = deserialize(nerBytes).asInstanceOf[LexiconNER]
    val reconstitutedNerBytes = serialize(reconstitutedNer)

    val nerBytesString = nerBytes.mkString(", ")
    val reconstitutedNerBytesString = reconstitutedNerBytes.mkString(", ")

    // Sometimes the strings are identical.
    if (nerBytesString != reconstitutedNerBytesString) {
      // Sometimes the set will be different
      if (!ner.equalsForSerialization(reconstitutedNer)) {
        assert(false, "The NER didn't serialize properly.")
        // Debug the following line to see what went wrong.
        ner.equalsForSerialization(reconstitutedNer)
      }
    }
    reconstitutedNer
  }

  def testKBsAndNers(kbs: Seq[String], overrideKBs: Seq[String], text: String, nes: Seq[String],
      entityValidator: EntityValidator, useLemmas: Boolean, caseInsensitive: Boolean,
      caseInsensitivesOpt: Option[Seq[Boolean]] = None, lexicalVariationsOpt: Option[LexicalVariations] = None): Unit = {
    val sentence = mkSentence(text)

    def testNer(ner: LexiconNER): Unit = {
      val suffix = s"entityValidator = ${entityValidator.getClass.getSimpleName} useLemmas = $useLemmas caseInsensitive = $caseInsensitive"
      val name = s"${ner.getClass.getSimpleName} with $suffix from $kbs and $overrideKBs"
      println(s"Testing $name")

      val labels = ner.find(sentence)
      labels should be(nes)

      val reconstitutedNer = reconstitute(ner)
      val reconstitutedLabels = reconstitutedNer.find(sentence)
      reconstitutedLabels should be(nes)
    }

    val lexicalVariations = lexicalVariationsOpt.getOrElse(new NoLexicalVariations())
    val fastsAndCompacts = Seq(
      (false, false), // SeparatedLexiconNER
      (true, false),  // CombinedLexiconNER
      (true, true)    // CompactLexiconNER
    )

    fastsAndCompacts.foreach { case (fast, compact) =>
      LexiconNER.USE_FAST = fast
      LexiconNER.USE_COMPACT = compact

      val ner1 =
        if (caseInsensitivesOpt.isEmpty)
          LexiconNER(kbs, Some(overrideKBs), entityValidator, lexicalVariations, useLemmas, caseInsensitive)
        else
          LexiconNER(kbs, Some(overrideKBs), caseInsensitivesOpt.get, entityValidator, lexicalVariations, useLemmas, caseInsensitive, None)

      testNer(ner1)

      val ner2 = {
        val caseInsensitives = caseInsensitivesOpt.getOrElse {
          kbs.map(_ => caseInsensitive)
        }
        val standardKbSources = kbs.zip(caseInsensitives).map { case (kb, caseInsensitive) =>
          val resourceStandardKbSource = new ResourceStandardKbSource(kb, caseInsensitive)
          val label = resourceStandardKbSource.getLabel
          val lines = resourceStandardKbSource.getLines

          new MemoryStandardKbSource(label, lines, caseInsensitive)
        }
        val overrideKbSources = overrideKBs.map { overrideKB =>
          val resourceOverrideKbSource = new ResourceOverrideKbSource(overrideKB)
          val lines = resourceOverrideKbSource.getLines

          new MemoryOverrideKbSource(lines)
        }

        LexiconNER(standardKbSources, Some(overrideKbSources), lexicalVariations, entityValidator, useLemmas, caseInsensitive)
      }

      testNer(ner2)

      val ner3 = {
        val baseDir = new File("./main/src/test/resources")
        assert(baseDir.exists)
        val caseInsensitives = caseInsensitivesOpt.getOrElse {
          kbs.map(_ => caseInsensitive)
        }
        val standardKbSources = kbs.zip(caseInsensitives).map { case (kb, caseInsensitive) =>
          val fileStandardKbSource = new FileStandardKbSource(kb, caseInsensitive, baseDir)
          val label = fileStandardKbSource.getLabel
          val lines = fileStandardKbSource.getLines

          new MemoryStandardKbSource(label, lines, caseInsensitive)
        }
        val overrideKbSources = overrideKBs.map { overrideKB =>
          val fileOverrideKbSource = new FileOverrideKbSource(overrideKB, baseDir)
          val lines = fileOverrideKbSource.getLines

          new MemoryOverrideKbSource(lines)
        }

        LexiconNER(standardKbSources, Some(overrideKbSources), lexicalVariations, entityValidator, useLemmas, caseInsensitive)
      }

      testNer(ner3)
    }
  }

  val kbs = Seq(
    "org/clulab/processors/A.tsv",
    "org/clulab/processors/B.tsv",
    "org/clulab/processors/C.tsv"
  )
  val overrideKBs = Seq(
    "org/clulab/processors/overrideKB.tsv"
  )

  behavior of "NERs"

  it should "work for all setting combinations" in {
    val entityValidators = Seq(new TrueEntityValidator, new LowerEntityValidator)
    val useLemmas = Seq(false, true)
    val caseInsensitives = Seq(false, true)
    val odometer = new SeqOdometer(Array(entityValidators, useLemmas, caseInsensitives))

    odometer.foreach { case Seq(entityValidator: EntityValidator, useLemmas: Boolean, caseInsensitive: Boolean) =>
      testKBsAndNers(kbs, overrideKBs, "a a b b a",   Seq("B-A", "I-A", "B-B", "I-B", "O"),        entityValidator, useLemmas, caseInsensitive)
      testKBsAndNers(kbs, overrideKBs, "a a a b b a", Seq("B-B", "I-B", "I-B", "B-B", "I-B", "O"), entityValidator, useLemmas, caseInsensitive)
    }
  }

  it should "heed the entityValidator setting" in {
    val entityValidator = new NoAEntityValidator
    val useLemmas = false
    val caseInsensitive = false

    testKBsAndNers(kbs, overrideKBs, "a a b b a",   Seq("O", "O", "B-B", "I-B", "O"),      entityValidator, useLemmas, caseInsensitive)
    testKBsAndNers(kbs, overrideKBs, "a a a b b a", Seq("O", "O", "O", "B-B", "I-B", "O"), entityValidator, useLemmas, caseInsensitive)
  }

  it should "heed the case insensitivity setting when true" in {
    val entityValidator = new TrueEntityValidator()
    val useLemmas = false
    val caseInsensitive = true

    testKBsAndNers(kbs, overrideKBs, "A a b b a",   Seq("B-A", "I-A", "B-B", "I-B", "O"),        entityValidator, useLemmas, caseInsensitive)
    testKBsAndNers(kbs, overrideKBs, "a A a B b a", Seq("B-B", "I-B", "I-B", "B-B", "I-B", "O"), entityValidator, useLemmas, caseInsensitive)
  }

  it should "heed the case insensitive setting when false" in {
    val entityValidator = new TrueEntityValidator()
    val useLemmas = false
    val caseInsensitive = false

    testKBsAndNers(kbs, overrideKBs, "A a b b a",   Seq("O", "O", "B-B", "I-B", "O"),      entityValidator, useLemmas, caseInsensitive)
    testKBsAndNers(kbs, overrideKBs, "A a a B b a", Seq("O", "B-A", "I-A", "O", "O", "O"), entityValidator, useLemmas, caseInsensitive)
  }

  it should "heed the useLemmas when true" in {
    val entityValidator = new TrueEntityValidator()
    val useLemmas = true
    val caseInsensitive = true
    val text = "Mares eat oats and does eat oats and little lambs eat ivy."

    testKBsAndNers(kbs, Seq.empty, text, Seq(
      "B-C", // mare
      "I-C", // eat
      "B-C", // oats
      "O",   // and
      "O",   // do
      "B-C", // eat
      "B-C", // oats
      "O",   // and
      "B-C", // little
      "I-C", // lamb
      "I-C", // eat
      "O",   // ivy
      "O"    // .
    ), entityValidator, useLemmas, caseInsensitive)
  }

  it should "heed the useLemmas when false" in {
    val entityValidator = new TrueEntityValidator()
    val useLemmas = false
    val caseInsensitive = true
    val text = "Mares eat oats and does eat oats and little lambs eat ivy."

    testKBsAndNers(kbs, Seq.empty, text, Seq(
      "B-C", // Mares
      "I-C", // eat
      "B-C", // oats
      "O",   // and
      "O",   // does
      "B-C", // eat
      "B-C", // oats
      "O",   // and
      "O",   // little
      "O",   // lambs
      "B-C", // eat
      "O",   // ivy
      "O"    // .
    ), entityValidator, useLemmas, caseInsensitive)
  }

  it should "use the overrideKBs" in {
    val entityValidator = new TrueEntityValidator()
    val useLemmas = true
    val caseInsensitive = true
    val text = "Mares eat oats and does eat oats and little lambs eat ivy."

    testKBsAndNers(kbs, overrideKBs, text, Seq(
      "B-D", // mare
      "I-D", // eat
      "B-C", // oats
      "O",   // and
      "O",   // do
      "B-C", // eat
      "B-C", // oats
      "O",   // and
      "B-A", // little
      "I-A", // lamb
      "I-A", // eat
      "O",   // ivy
      "O"    // .
    ), entityValidator, useLemmas, caseInsensitive)
  }

  it should "support case changes" in {
    val entityValidator = new TrueEntityValidator()
    val useLemmas = false
    val caseInsensitive = false // For D then, mare eat
    val caseInsensitives = Seq(
      true,  // A
      false, // B
      true   // C
    )

    testKBsAndNers(kbs, overrideKBs, "A a b b a Little lamb eat", Seq("B-A", "I-A", "B-B", "I-B", "O", "B-A", "I-A", "I-A"), entityValidator, useLemmas, caseInsensitive, Some(caseInsensitives))
    testKBsAndNers(kbs, overrideKBs, "a A a B b a mare eAt",      Seq("B-A", "I-A", "O", "O", "O", "O", "B-C", "I-C"),       entityValidator, useLemmas, caseInsensitive, Some(caseInsensitives))
  }

  it should "work with lexical variations" in {
    val entityValidator = new TrueEntityValidator()
    val useLemmas = false
    val caseInsensitive = false
    val lexicalVariations = new AddXYLexicalVariations()

    testKBsAndNers(kbs, Seq.empty, "ax ax by by a", Seq("B-A", "I-A", "B-B", "I-B", "O"), entityValidator, useLemmas, caseInsensitive, lexicalVariationsOpt = Some(lexicalVariations))
  }

  behavior of "serializer"

  def serialize(entityValidator: EntityValidator): Array[Byte] = {
    val byteArrayOutputStream = new ByteArrayOutputStream()

    new ObjectOutputStream(byteArrayOutputStream).autoClose { objectOutputStream =>
      objectOutputStream.writeObject(entityValidator)
    }
    byteArrayOutputStream.toByteArray
  }

  it should "serialize the default entity validator" in {
    serialize(EntityValidator.TRUE_VALIDATOR)
  }

  it should "serialize a copy of the default entity validator" in {
    serialize(new TrueEntityValidator)
  }

  it should "serialize a new kind of entity validator" in {
    serialize(new LowerEntityValidator)
  }
}

@SerialVersionUID(1000L)
class LowerEntityValidator() extends EntityValidator {

  override def validMatch(sentence: Sentence, start: Int, end: Int): Boolean = {
    !sentence.words.view(start, end).exists { word =>
      word.exists(_.isUpper) // Make sure there is none of this.
    }
  }
}

// This was not working before when it was inside the test class.  Don't do that, obviously.
@SerialVersionUID(1000L)
class NoAEntityValidator() extends EntityValidator {

  override def validMatch(sentence: Sentence, start: Int, end: Int): Boolean = {
    !sentence.words.view(start, end).contains("a")
  }
}

class AddXYLexicalVariations extends LexicalVariations {

  override def lexicalVariations(tokens: Array[String]): Seq[Array[String]] = {
    Seq(
      tokens.map(_ + "x"),
      tokens.map(_ + "y")
    )
  }
}
