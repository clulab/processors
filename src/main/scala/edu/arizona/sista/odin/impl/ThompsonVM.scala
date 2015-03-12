package edu.arizona.sista.odin.impl

import edu.arizona.sista.processors.Document
import edu.arizona.sista.odin._

object ThompsonVM {
  type NamedGroups = Map[String, (Int, Int)]
  type NamedMentions = Map[String, Mention]

  trait Thread {
    def isDone: Boolean
    def results: Seq[(NamedGroups, NamedMentions)]
  }

  private case class SingleThread(tok: Int, inst: Inst) extends Thread {
    var groups: NamedGroups = _
    var mentions: NamedMentions = _
    def isDone: Boolean = inst == Done
    def results: Seq[(NamedGroups, NamedMentions)] = Seq((groups, mentions))
  }

  private object SingleThread {
    def apply(tok: Int, inst: Inst, groups: NamedGroups, mentions: NamedMentions): Thread = {
      val t = new SingleThread(tok, inst)
      t.groups = groups
      t.mentions = mentions
      t
    }
  }

  private case class ThreadBundle(bundles: Seq[Seq[Thread]]) extends Thread {
    def isDone: Boolean = bundles exists (_ exists (_.isDone))
    def results: Seq[(NamedGroups, NamedMentions)] = bundles.flatMap(_.find(_.isDone).map(_.results)).flatten
  }

  def evaluate(start: Inst, tok: Int, sent: Int, doc: Document, state: Option[State]): Seq[(NamedGroups, NamedMentions)] = {
    def mkThreads(tok: Int, inst: Inst, groups: NamedGroups, mentions: NamedMentions): Seq[Thread] = inst match {
      case i: Jump => mkThreads(tok, i.next, groups, mentions)
      case i: Split => mkThreads(tok, i.lhs, groups, mentions) ++ mkThreads(tok, i.rhs, groups, mentions)
      case i: SaveStart => mkThreads(tok, i.next, groups + (i.name -> (tok, -1)), mentions)
      case i: SaveEnd => mkThreads(tok, i.next, groups + (i.name -> (groups(i.name)._1, tok)), mentions)
      case _ => Seq(SingleThread(tok, inst, groups, mentions))
    }

    def stepSingleThread(t: SingleThread): Seq[Thread] = t.inst match {
      case i: MatchToken if t.tok < doc.sentences(sent).size && i.c.matches(t.tok, sent, doc, state) =>
        mkThreads(t.tok + 1, i.next, t.groups, t.mentions)  // token matched, return new threads
      case i: MatchSentenceStart if t.tok == 0 =>
        mkThreads(t.tok, i.next, t.groups, t.mentions)
      case i: MatchSentenceEnd if t.tok == doc.sentences(sent).size =>
        mkThreads(t.tok, i.next, t.groups, t.mentions)
      case i: MatchMention => state match {
        case None => Nil  // should we throw an exception or fail silently?
        case Some(s) =>
          val bundles = for {
            mention <- s.mentionsFor(sent, t.tok)
            if mention.start == t.tok && mention.matches(i.m)
          } yield mkThreads(mention.end, i.next, t.groups, mkMentionCapture(t.mentions, i.name, mention))
          if (bundles.nonEmpty) Seq(ThreadBundle(bundles)) else Nil
      }
      case _ => Nil  // thread died with no match
    }

    def mkMentionCapture(mentions: NamedMentions, name: Option[String], mention: Mention): NamedMentions = name match {
      case None => mentions
      case Some(name) => mentions + (name -> mention)
    }

    def stepThreadBundle(t: ThreadBundle): Seq[Thread] = {
      val bundles = t.bundles flatMap { bundle =>
        val ts = stepThreads(bundle)
        if (ts.nonEmpty) Some(ts) else None
      }
      if (bundles.nonEmpty) Seq(ThreadBundle(bundles)) else Nil
    }

    def stepThread(t: Thread): Seq[Thread] = t match {
      case t: SingleThread => stepSingleThread(t)
      case t: ThreadBundle => stepThreadBundle(t)
    }

    def stepThreads(threads: Seq[Thread]): Seq[Thread] =
      (threads flatMap stepThread).distinct

    def handleDone(threads: Seq[Thread]): (Seq[Thread], Option[Thread]) =
      threads find (_.isDone) match {
        // no thread has finished, return them all
        case None => (threads, None)
        // a thread finished, drop all threads to its right but keep the ones to its left
        case Some(t) => (threads.takeWhile(_ != t), Some(t))
      }

    @annotation.tailrec
    def loop(threads: Seq[Thread], result: Option[Thread]): Option[Thread] = {
      if (threads.isEmpty) result
      else {
        val (ts, r) = handleDone(threads)
        loop(stepThreads(ts), r)
      }
    }

    loop(mkThreads(tok, start, Map.empty, Map.empty), None) match {
      case None => Nil
      case Some(t) => t.results
    }
  }
}

sealed trait Inst {
  var next: Inst = null
  def dup: Inst
}

case class Split(lhs: Inst, rhs: Inst) extends Inst {
  def dup: Inst = Split(lhs.dup, rhs.dup)
}

case class MatchToken(c: TokenConstraint) extends Inst {
  def dup: Inst = {
    val inst = copy()
    if (next != null) inst.next = next.dup
    inst
  }
}

case class MatchMention(m: StringMatcher) extends Inst {
  var name: Option[String] = None

  def dup: Inst = {
    val inst = copy()
    if (next != null) inst.next = next.dup
    inst
  }
}

object MatchMention {
  def apply(name: String, matcher: StringMatcher): MatchMention = {
    val inst = MatchMention(matcher)
    inst.name = Some(name)
    inst
  }
}

case class MatchSentenceStart() extends Inst {
  def dup: Inst = {
    val inst = copy()
    if (next != null) inst.next = next.dup
    inst
  }
}

case class MatchSentenceEnd() extends Inst {
  def dup: Inst = {
    val inst = copy()
    if (next != null) inst.next = next.dup
    inst
  }
}

case class Jump() extends Inst {
  def dup: Inst = {
    val inst = copy()
    if (next != null) inst.next = next.dup
    inst
  }
}

case class SaveStart(name: String) extends Inst {
  def dup: Inst = {
    val inst = copy()
    if (next != null) inst.next = next.dup
    inst
  }
}

case class SaveEnd(name: String) extends Inst {
  def dup: Inst = {
    val inst = copy()
    if (next != null) inst.next = next.dup
    inst
  }
}

case object Done extends Inst {
  def dup: Inst = this
}
