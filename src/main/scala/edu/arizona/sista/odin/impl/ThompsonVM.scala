package edu.arizona.sista.odin.impl

import edu.arizona.sista.processors.Document
import edu.arizona.sista.odin._

object ThompsonVM {
  type NamedGroups = Map[String, (Int, Int)]
  type NamedMentions = Map[String, Mention]

  sealed trait Thread {
    def isDone: Boolean
    def isReallyDone: Boolean
    def results: Seq[(NamedGroups, NamedMentions)]
  }

  private case class SingleThread(tok: Int, inst: Inst) extends Thread {
    var groups: NamedGroups = _
    var mentions: NamedMentions = _
    def isDone: Boolean = inst == Done
    def isReallyDone: Boolean = isDone
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
    // at least one thread is done and the threads after the threadbundle can be dropped
    def isDone: Boolean = bundles.exists(_.exists(_.isDone))
    // all bundles are done and we can retrieve the results
    def isReallyDone: Boolean = bundles.forall(_.head.isReallyDone)
    def results: Seq[(NamedGroups, NamedMentions)] = for {
      ts <- bundles
      t = ts.head
      if t.isDone
      r <- t.results
    } yield r
  }

  def evaluate(
      start: Inst,
      tok: Int,
      sent: Int,
      doc: Document,
      state: Option[State]
  ): Seq[(NamedGroups, NamedMentions)] = {

    def mkThreads(
        tok: Int,
        inst: Inst,
        groups: NamedGroups,
        mentions: NamedMentions
    ): Seq[Thread] = inst match {
      case i: Jump =>
        mkThreads(tok, i.next, groups, mentions)
      case i: Split =>
        mkThreads(tok, i.lhs, groups, mentions) ++ mkThreads(tok, i.rhs, groups, mentions)
      case i: SaveStart =>
        mkThreads(tok, i.next, groups + (i.name -> (tok, -1)), mentions)
      case i: SaveEnd =>
        mkThreads(tok, i.next, groups + (i.name -> (groups(i.name)._1, tok)), mentions)
      case _ =>
        Seq(SingleThread(tok, inst, groups, mentions))
    }

    def stepSingleThread(t: SingleThread): Seq[Thread] = t.inst match {
      case i: MatchToken
          if t.tok < doc.sentences(sent).size && i.c.matches(t.tok, sent, doc, state) =>
        mkThreads(t.tok + 1, i.next, t.groups, t.mentions)  // token matched, return new threads
      case i: MatchSentenceStart if t.tok == 0 =>
        mkThreads(t.tok, i.next, t.groups, t.mentions)
      case i: MatchSentenceEnd if t.tok == doc.sentences(sent).size =>
        mkThreads(t.tok, i.next, t.groups, t.mentions)
      case i: MatchMention => state match {
        case None => sys.error("can't match mentions without state")
        case Some(s) =>
          val bundles = for {
            mention <- s.mentionsFor(sent, t.tok)
            if mention.start == t.tok && mention.matches(i.m)
          } yield {
            val captures = mkMentionCapture(t.mentions, i.name, mention)
            mkThreads(mention.end, i.next, t.groups, captures)
          }
          bundles match {
            case Seq() => Nil
            case Seq(bundle) => bundle
            case bundles => Seq(ThreadBundle(bundles))
          }
      }
      case Done => Seq(t)
      case _ => Nil  // thread died with no match
    }

    def mkMentionCapture(
        mentions: NamedMentions,
        name: Option[String],
        mention: Mention
    ): NamedMentions = name match {
      case None => mentions
      case Some(name) => mentions + (name -> mention)
    }

    def stepThreadBundle(t: ThreadBundle): Seq[Thread] = {
      val bundles = for {
        threads <- t.bundles
        newThreads = stepThreads(threads)
        if newThreads.nonEmpty
        (survivingThreads, result) = handleDone(newThreads)
      } yield result match {
        case None => survivingThreads
        case Some(r) => survivingThreads :+ r
      }
      bundles match {
        case Seq() => Nil
        case Seq(bundle) => bundle
        case bundles => Seq(ThreadBundle(bundles))
      }
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
        // a threadbundle is done, but is it really done?
        case Some(t: ThreadBundle) =>
          val survivors = threads.takeWhile(_ != t)
          if (t.isReallyDone) (survivors, Some(t)) else (survivors :+ t, None)
        // a thread finished, drop all threads to its right but keep the ones to its left
        case Some(t) => (threads.takeWhile(_ != t), Some(t))
      }

    @annotation.tailrec
    def loop(threads: Seq[Thread], currentResult: Option[Thread]): Option[Thread] = {
      if (threads.isEmpty) currentResult
      else {
        val (ts, nextResult) = handleDone(threads)
        loop(stepThreads(ts), nextResult orElse currentResult)
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
    inst.name = name
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
