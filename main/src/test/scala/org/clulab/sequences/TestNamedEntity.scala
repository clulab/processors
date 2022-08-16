package org.clulab.sequences

import org.clulab.scala.WrappedArray._
import org.clulab.utils.Test

class TestNamedEntity extends Test {

  behavior of "NamedEntity identification"

  {
    def test(id: String, bioLabelString: String, expectedNamedEntities: Seq[NamedEntity]): Unit = {
      // if (id == "1")
      it should s"collect properly test $id" in {
        val bioLabels = bioLabelString.split(' ')
        val actualNamedEntities = NamedEntity.collect(bioLabels)

        actualNamedEntities should contain theSameElementsInOrderAs (expectedNamedEntities)
      }
    }

    test("1", "", Seq.empty)
    test("2", "O", Seq.empty)

    test("3", "B-TEST", Seq(NamedEntity("TEST", Range(0, 1))))
    test("4", "O B-HELLO", Seq(NamedEntity("HELLO", Range(1, 2))))
    test("5", "O B-HELLO I-HELLO", Seq(NamedEntity("HELLO", Range(1, 3))))
    test("6", "O B-HELLO I-HELLO I-HELLO", Seq(NamedEntity("HELLO", Range(1, 4))))
    test("7", "O B-PER O", Seq(NamedEntity("PER", Range(1, 2))))
    test("8", "O B-PER I-PER O", Seq(NamedEntity("PER", Range(1, 3))))
    test("9", "O B-PER I-PER I-PER O", Seq(NamedEntity("PER", Range(1, 4))))

    test("10", "B-1 B-2", Seq(NamedEntity("1", Range(0, 1)), NamedEntity("2", Range(1, 2))))
    test("11", "B-1 I-1 B-2", Seq(NamedEntity("1", Range(0, 2)), NamedEntity("2", Range(2, 3))))
    test("12", "B-1 I-1 B-2 I-2", Seq(NamedEntity("1", Range(0, 2)), NamedEntity("2", Range(2, 4))))
    test("13", "B-1 O B-2", Seq(NamedEntity("1", Range(0, 1)), NamedEntity("2", Range(2, 3))))
    test("14", "B-1 I-1 O B-2", Seq(NamedEntity("1", Range(0, 2)), NamedEntity("2", Range(3, 4))))
    test("15", "B-1 I-1 I-1 O B-2", Seq(NamedEntity("1", Range(0, 3)), NamedEntity("2", Range(4, 5))))
  }

  {
    def test(id: String, genericBioLabelString: String, customBioLabelString: String, expectedCombinedBioLabelString: String): Unit = {
      // if (id == "1")
      it should s"combine properly test $id" in {
        val genericBioLabels = genericBioLabelString.split(' ')
        val customBioLabels = customBioLabelString.split(' ')
        val genericNamedEntities = NamedEntity.collect(genericBioLabels)
        val customNamedEntities = NamedEntity.collect(customBioLabels)
        val actualCombinedBioLabels = NamedEntity.combine(genericBioLabels, genericNamedEntities, customNamedEntities)
        val actualCombinedBioLabelString = actualCombinedBioLabels.mkString(" ")

        actualCombinedBioLabelString should be(expectedCombinedBioLabelString)
      }
    }

    test("1",
      "O",
      "O",
      "O"
    )
    test("2",
      "B-1",
      "O",
      "B-1"
    )
    test("3",
      "B-1",
      "B-2",
      "B-2"
    )
    test("4",
      "B-1 B-2",
      "O B-3",
      "B-1 B-3"
    )
    test("5",
      "O B-1 I-1 O",
      "O B-2 O O",
      "O B-1 I-1 O"
    )
    test("6",
      "O B-1 I-1 O",
      "O O B-2 O",
      "O B-1 I-1 O"
    )
    test("7",
      "O B-1 I-1 O",
      "O B-2 I-2 O",
      "O B-2 I-2 O"
    )
    test("8",
      "O B-1 I-1 I-1 O",
      "B-2 I-2 O O O",
      "O B-1 I-1 I-1 O"
    )
    test("9",
      "O B-1 I-1 I-1 O",
      "O B-2 I-2 O O",
      "O B-1 I-1 I-1 O"
    )
    test("10",
      "O B-1 I-1 I-1 O",
      "O O B-2 I-2 O",
      "O B-1 I-1 I-1 O"
    )
    test("11",
      "O B-1 I-1 I-1 O",
      "O O O B-2 I-2",
      "O B-1 I-1 I-1 O"
    )
    test("12",
      "O B-1 I-1 I-1 O",
      "O B-2 I-2 I-2 O",
      "O B-2 I-2 I-2 O"
    )
    test("13",
      "B-0 B-1 I-1 I-1 O",
      "B-2 I-2 I-2 O O",
      "B-0 B-1 I-1 I-1 O"
    )
    test("14",
      "B-1 O O O B-3",
      "B-4 B-2 I-2 I-2 B-6",
      "B-4 B-2 I-2 I-2 B-6"
    )
  }

  behavior of "NamedEntity validation"

  {
    def test(id: String, entitiesString: String, expectedResult: Boolean): Unit = {
      // if (id == "1")
      it should s"validate the BIO notation test $id" in {
        val entities = entitiesString.split(' ')
        val actualResult = NamedEntity.isValid(entities)

        actualResult should be(expectedResult)
      }
    }

    test("1", "", true)

    test("2", "B-STH", true)
    test("3", "I-STH", false)
    test("4", "I-STH O", false)

    test("5", "O B-STH", true)
    test("6", "O I-STH", false)
    test("7", "O I-STH I-STH", false)

    test("8", "B-STH I-OTH", false)
    test("9", "B-STH I-OTH I-OTH", false)
  }

  behavior of "NamedEntity correction"

  {
    def test(id: String, entitiesString: String, expectedEntitiesString: String): Unit = {
      // if (id == "1")
      it should s"correct the BIO notation test $id" in {
        val entities = entitiesString.split(' ')
        val actualEntities = NamedEntity.patch(entities)
        val actualEntitiesString = actualEntities.mkString(" ")

        actualEntitiesString should be(expectedEntitiesString)
      }
    }

    test("1", "", "")

    test("2", "B-STH", "B-STH")
    test("3", "I-STH", "B-STH")
    test("4", "I-STH O", "B-STH O")

    test("5", "O B-STH", "O B-STH")
    test("6", "O I-STH", "O B-STH")
    test("7", "O I-STH I-STH", "O B-STH I-STH")

    test("8", "B-STH I-OTH", "B-STH B-OTH")
    test("9", "B-STH I-OTH I-OTH", "B-STH B-OTH I-OTH")
  }
}
