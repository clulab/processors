package org.clulab.utils

class TestArrayView extends Test {

  behavior of "ArrayView"

  it should "work with no offset" in {
    val array = Array(1, 2, 3)
    val arrayView = ArrayView(array)

    array.length should be (arrayView.length)

    arrayView.zip(array).foreach { case (arrayViewItem, arrayItem) =>
      arrayViewItem should be (arrayItem)
    }

    arrayView(0) = 4
    arrayView(0) should be (4)
    array(0) should be (4)
  }

  it should "work with an offset" in {
    val offset = 1
    val array = Array(1, 2, 3)
    val arrayView = ArrayView(array, offset)

    array.length should be (arrayView.length + offset)

    arrayView.zip(array).foreach { case (arrayViewItem, arrayItem) =>
      arrayViewItem should be (arrayItem + offset)
    }

    arrayView(0) = 4
    arrayView(0) should be (4)
    array(1) should be (4)
  }

  it should "work when clipped" in {
    val offset = 1
    val clip = 1
    val array = Array(1, 2, 3)
    val arrayView = ArrayView(array, offset, array.length - clip)

    array.length should be (arrayView.length + offset + clip)

    arrayView.zip(array).foreach { case (arrayViewItem, arrayItem) =>
      arrayViewItem should be (arrayItem + offset)
    }

    arrayView(0) = 4
    arrayView(0) should be (4)
    array(1) should be (4)
  }
}
