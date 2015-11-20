package edu.arizona.sista.odin.impl

import edu.arizona.sista.processors.Document
import edu.arizona.sista.struct.Interval
import edu.arizona.sista.odin._

object ThompsonVM {

  type NamedGroups = Map[String, Seq[Interval]]
  type NamedMentions = Map[String, Seq[Mention]]
  type PartialGroups = List[(String, Int)]

  sealed trait Thread {
    def isDone: Boolean
    def isReallyDone: Boolean
    def results: Seq[(NamedGroups, NamedMentions)]
  }

  private case class SingleThread(
      tok: Int,
      inst: Inst,
      groups: NamedGroups,
      mentions: NamedMentions,
      partialGroups: List[(String, Int)]
  ) extends Thread {
    def isDone: Boolean = inst == Done
    def isReallyDone: Boolean = isDone
    def results: Seq[(NamedGroups, NamedMentions)] = Seq((groups, mentions))
  }

  private case class ThreadBundle(bundles: Seq[Seq[Thread]]) extends Thread {
    // at least one thread is done and the threads after the threadbundle can be dropped
    def isDone: Boolean = bundles.exists(_.exists(_.isDone))
    // all bundles are done and we can retrieve the results
    def isReallyDone: Boolean = bundles.forall(_.head.isReallyDone)
    def results: Seq[(NamedGroups, NamedMentions)] = for {
      ts <- bundles
      t = ts.head
      if t.isReallyDone
      r <- t.results
    } yield r
  }

  def evaluate(
      start: Inst,
      tok: Int,
      sent: Int,
      doc: Document,
      state: State
  ): Seq[(NamedGroups, NamedMentions)] = {

    // Executes instruction on token and returns the produced threads.
    // Threads are created by following all no-Match instructions.
    def mkThreads(
        tok: Int,
        inst: Inst,
        groups: NamedGroups,
        mentions: NamedMentions,
        partialGroups: PartialGroups
    ): Seq[Thread] = {
      @annotation.tailrec
      def loop(
          is: List[(Inst, NamedGroups, NamedMentions, PartialGroups)],
          ts: Seq[Thread]
      ): Seq[Thread] = is match {
        case Nil => ts
        case (i, gs, ms, pgs) :: rest => i match {
          case i: Jump => loop((i.next, gs, ms, pgs) :: rest, ts)
          case i: Split => loop((i.lhs, gs, ms, pgs) :: (i.rhs, gs, ms, pgs) :: rest, ts)
          case i: SaveStart => loop((i.next, gs, ms, (i.name, tok) :: pgs) :: rest, ts)
          case i: SaveEnd => pgs match {
            case (name, start) :: partials if name == i.name =>
              val updatedGroups = gs.getOrElse(name, Vector.empty) :+ Interval(start, tok)
              loop((i.next, gs + (name -> updatedGroups), ms, partials) :: rest, ts)
            case _ => sys.error("unable to close capture")
          }
          case i => loop(rest, ts :+ SingleThread(tok, i, gs, ms, pgs))
        }
      }
      // return threads produced by `inst`
      loop(List((inst, groups, mentions, partialGroups)), Nil)
    }

    // Advance thread by executing instruction.
    // Instruction is expected to be a Match instruction.
    def stepSingleThread(t: SingleThread): Seq[Thread] = t.inst match {
      case i: MatchToken if t.tok < doc.sentences(sent).size && i.c.matches(t.tok, sent, doc, state) =>
        mkThreads(t.tok + 1, i.next, t.groups, t.mentions, t.partialGroups)  // token matched, return new threads
      case i: MatchSentenceStart if t.tok == 0 =>
        mkThreads(t.tok, i.next, t.groups, t.mentions, t.partialGroups)
      case i: MatchSentenceEnd if t.tok == doc.sentences(sent).size =>
        mkThreads(t.tok, i.next, t.groups, t.mentions, t.partialGroups)
      case i: MatchLookAhead =>
        val results = eval(mkThreads(t.tok, i.start, t.groups, t.mentions, Nil), None)
        if (i.negative == results.isEmpty)
          mkThreads(t.tok, i.next, t.groups, t.mentions, t.partialGroups)
        else Nil
      case i: MatchLookBehind =>
        val startTok = tok - i.size
        if (startTok < 0) {
          if (i.negative) mkThreads(t.tok, i.next, t.groups, t.mentions, t.partialGroups)
          else Nil
        } else {
          val results = eval(mkThreads(startTok, i.start, t.groups, t.mentions, Nil), None)
          if (i.negative == results.isEmpty)
            mkThreads(t.tok, i.next, t.groups, t.mentions, t.partialGroups)
          else Nil
        }
      case i: MatchMention =>
        val bundles = for {
          mention <- state.mentionsFor(sent, t.tok)
          if mention.start == t.tok && mention.matches(i.m)
        } yield {
          val captures = mkMentionCapture(t.mentions, i.name, mention)
          mkThreads(mention.end, i.next, t.groups, captures, t.partialGroups)
        }
        bundles match {
          case Seq() => Nil
          case Seq(bundle) => bundle
          case bundles => Seq(ThreadBundle(bundles))
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
      case Some(name) =>
        val ms = mentions.getOrElse(name, Vector.empty) :+ mention
        mentions + (name -> ms)
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
    def eval(threads: Seq[Thread], currentResult: Option[Thread]): Option[Thread] = {
      if (threads.isEmpty) currentResult
      else {
        val (ts, nextResult) = handleDone(threads)
        eval(stepThreads(ts), nextResult orElse currentResult)
      }
    }

    eval(mkThreads(tok, start, Map.empty, Map.empty, Nil), None) match {
      case None => Nil
      case Some(t) => t.results
    }
  }
}

sealed trait Inst {
  var next: Inst = null
  def dup(): Inst
  def deepcopy(): Inst = {
    val inst = dup()
    if (next != null) inst.next = next.deepcopy()
    inst
  }
}

case class MatchLookAhead(start: Inst, negative: Boolean) extends Inst {
  def dup() = MatchLookAhead(start.deepcopy(), negative)
}

case class MatchLookBehind(start: Inst, size: Int, negative: Boolean) extends Inst {
  def dup() = MatchLookBehind(start.deepcopy(), size, negative)
}

case class Split(lhs: Inst, rhs: Inst) extends Inst {
  def dup() = Split(lhs.deepcopy(), rhs.deepcopy())
}

case class MatchToken(c: TokenConstraint) extends Inst {
  def dup() = copy()
}

case class MatchMention(m: StringMatcher, name: Option[String]) extends Inst {
  def dup() = copy()
}

case class MatchSentenceStart() extends Inst {
  def dup() = copy()
}

case class MatchSentenceEnd() extends Inst {
  def dup() = copy()
}

case class Jump() extends Inst {
  def dup() = copy()
}

case class SaveStart(name: String) extends Inst {
  def dup() = copy()
}

case class SaveEnd(name: String) extends Inst {
  def dup() = copy()
}

case object Done extends Inst {
  def dup() = this
}
