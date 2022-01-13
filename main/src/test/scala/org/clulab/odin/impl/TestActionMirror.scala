package org.clulab.odin.impl

import org.clulab.odin.Action
import org.clulab.odin.Actions
import org.clulab.odin.Mention
import org.clulab.odin.State
import org.clulab.utils.Test

import java.util.{List => JList}

class TestActionMirror extends Test {

  class TestActions extends Actions {
    var rawActionCalled = 0
    var plainActionCalled = 0
    var scalaActionCalled = 0
    var javaActionCalled = 0
    var otherActionCalled = 0

    val rawAction: Action = (mentions: Seq[Mention], state: State) => {
      rawActionCalled += 1
      mentions
    }

    def plainAction: Action = (mentions: Seq[Mention], state: State) => {
      plainActionCalled += 1
      mentions
    }

    def scalaAction(mentions: Seq[Mention], state: State): Seq[Mention] = {
      scalaActionCalled += 1
      mentions
    }

    def javaAction(mentions: JList[Mention], state: State): JList[Mention] = {
      javaActionCalled += 1
      mentions
    }

    def otherAction(mentions: JList[Mention], state: State): Int = {
      otherActionCalled += 1
      42
    }
  }

  val actions = new TestActions()
  val actionMirror = new ActionMirror(actions)
  val state = new State()

  behavior of "ActionMirror"

  it should "mirror a rawAction" in {
    actions.rawActionCalled should be (0)

    val inMentions = Vector[Mention]()
    val action = actionMirror.reflect("rawAction")
    val outMentions = action(inMentions, state)

    actions.rawActionCalled should be (1)
    outMentions should be theSameInstanceAs inMentions
  }

  it should "mirror a plainAction" in {
    actions.plainActionCalled should be (0)

    val inMentions = Vector[Mention]()
    val action = actionMirror.reflect("plainAction")
    val outMentions = action(inMentions, state)

    actions.plainActionCalled should be (1)
    outMentions should be theSameInstanceAs inMentions
  }

  it should "mirror a scalaAction" in {
    actions.scalaActionCalled should be (0)

    val inMentions = Vector[Mention]()
    val action = actionMirror.reflect("scalaAction")
    val outMentions = action(inMentions, state)

    actions.scalaActionCalled should be (1)
    outMentions should be theSameInstanceAs inMentions
  }

  it should "mirror a javaAction" in {
    actions.javaActionCalled should be (0)

    val inMentions = Vector[Mention]()
    val action = actionMirror.reflect("javaAction")
    val outMentions = action(inMentions, state)

    actions.javaActionCalled should be (1)
    outMentions should not be theSameInstanceAs (inMentions)
  }

  it should "mirror the defaultAction" in {
    val inMentions = Vector[Mention]()
    val action = actionMirror.reflect("default")
    val outMentions = action(inMentions, state)

    outMentions should be theSameInstanceAs inMentions
  }

  it should "not mirror an otherAction" in {
    actions.otherActionCalled should be (0)
    a [RuntimeException] should be thrownBy {
      actionMirror.reflect("otherAction")
    }
    actions.otherActionCalled should be (0)
  }
}
