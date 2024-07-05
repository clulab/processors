package org.clulab.utils

class TestLazy extends Test {

  class Animal

  class Cat extends Animal
  class Dog extends Animal

  behavior of "Lazy"

  it should "not retrieve a Cat from an Animal" in {
    // Even though a Cat is a subtype of Animal,
    // Lazy[Cat] is not a subtype of Lazy[Animal].
    // This does not compile.
    // val lazyCat: Lazy[Cat] = Lazy { new Animal }
  }

  it should "retrieve an Animal from a Cat" in {
    // This does compile.
    val lazyAnimal: Lazy[Animal] = Lazy { new Cat }
  }
}
