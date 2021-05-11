package org.clulab.openie.filtering

import org.scalatest.{FlatSpec, Matchers}

class TestStopWordManager extends FlatSpec with Matchers {

  behavior of "StopWordManager"
  val stopManager: StopWordManager = StopWordManager.fromConfig()

  it should "identify words as stops" in {
    val words = Seq("apple", "try", "with", "type")
    words.map(stopManager.isStopWord) should contain inOrderOnly(false, false, true, false)
  }

  it should "identify words as transparent" in {
    val words = Seq("apple", "try", "with", "type")
    words.map(stopManager.isTransparentWord) should contain inOrderOnly(false, false, false, true)
  }

  it should "identify words as stop or transparent" in {
    val words = Seq("apple", "try", "with", "type")
    words.map(stopManager.isStopOrTransparentWord) should contain inOrderOnly(false, false, true, true)
  }

  it should "identify stop entities if populated" in {
    it should "identify words as stop or transparent" in {
      val entities = Seq("O", "O", "PERSON", "DURATION")
      entities.map(stopManager.isStopNER) should contain inOrderOnly(false, false, false, true)
    }
  }

  it should "identify stop entities if populated" in {}

  it should "not flag entity as stop if not populated" in {}

  it should "identify if there is non-stop content" in {}

}
