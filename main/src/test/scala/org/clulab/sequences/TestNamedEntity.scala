package org.clulab.sequences

import org.clulab.utils.Test

class TestNamedEntity extends Test {

  behavior of "NamedEntity"

  {
    def test(bioTagString: String, expectedNamedEntities: Seq[NamedEntity]): Unit = {
      it should s"collect properly from $bioTagString" in {
        val bioTags = bioTagString.split(' ')
        val actualNamedEntities = NamedEntity.collect(bioTags)

        actualNamedEntities should contain theSameElementsInOrderAs (expectedNamedEntities)
      }
    }

    test("", Seq.empty)
    test("O", Seq.empty)

    test("B-TEST", Seq(NamedEntity("TEST", Range(0, 1))))
    test("O B-HELLO", Seq(NamedEntity("HELLO", Range(1, 2))))
    test("O B-HELLO I-HELLO", Seq(NamedEntity("HELLO", Range(1, 3))))
    test("O B-HELLO I-HELLO I-HELLO", Seq(NamedEntity("HELLO", Range(1, 4))))
    test("O B-PER O", Seq(NamedEntity("PER", Range(1, 2))))
    test("O B-PER I-PER O", Seq(NamedEntity("PER", Range(1, 3))))
    test("O B-PER I-PER I-PER O", Seq(NamedEntity("PER", Range(1, 4))))

    test("B-1 B-2", Seq(NamedEntity("1", Range(0, 1)), NamedEntity("2", Range(1, 2))))
    test("B-1 I-1 B-2", Seq(NamedEntity("1", Range(0, 2)), NamedEntity("2", Range(2, 3))))
    test("B-1 I-1 B-2 I-2", Seq(NamedEntity("1", Range(0, 2)), NamedEntity("2", Range(2, 4))))
    test("B-1 O B-2", Seq(NamedEntity("1", Range(0, 1)), NamedEntity("2", Range(2, 3))))
    test("B-1 I-1 O B-2", Seq(NamedEntity("1", Range(0, 2)), NamedEntity("2", Range(3, 4))))
    test("B-1 I-1 I-1 O B-2", Seq(NamedEntity("1", Range(0, 3)), NamedEntity("2", Range(4, 5))))
  }

  {
    def test(id: String, genericBioTagString: String, customBioTagString: String, expectedCombinedBioTagString: String): Unit = {
      // if (id == "1")
      it should s"combine properly test $id" in {
        val genericBioTags = genericBioTagString.split(' ')
        val customBioTags = customBioTagString.split(' ')
        val genericNamedEntities = NamedEntity.collect(genericBioTags)
        val customNamedEntities = NamedEntity.collect(customBioTags)
        val actualCombinedBioTags = NamedEntity.combine(genericBioTags.length, genericNamedEntities, customNamedEntities)
        val actualCombinedBioTagString = actualCombinedBioTags.mkString(" ")

        actualCombinedBioTagString should be(expectedCombinedBioTagString)
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
  }
}
