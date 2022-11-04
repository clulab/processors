package org.clulab.odin.impl

import org.clulab.processors.Document
import org.clulab.struct.Interval
import org.clulab.odin._

import scala.collection.immutable.ListMap

object ThompsonVM {

  type NamedGroups = ListMap[String, Seq[Interval]]
  type NamedMentions = ListMap[String, Seq[Mention]]
  // a partial group is the name and the start of a group, without the end
  type PartialGroups = List[(String, Int)]

  // enum
  object Direction extends Enumeration {
    type Direction = Value
    val LeftToRight, RightToLeft = Value
  }
  import Direction._

  case class Evidence(thread: Thread) {

  }

  case class Result(evidence: Evidence, namedGroups: NamedGroups, namedMentions: NamedMentions) {
    println("A result was created!")
  }

  sealed trait Thread {
    val name: String = Namer.name(this)
    def isDone: Boolean
    def isReallyDone: Boolean
    def results: Seq[Result]
  }

  private case class SingleThread(
      tok: Int,
      inst: Inst,
      dir: Direction,
      groups: NamedGroups,
      mentions: NamedMentions,
      partialGroups: PartialGroups
  ) extends Thread {
    println(toString())

    override def toString(): String = {
      val groupNames = groups.keys.mkString("[", ", ", "]")
      val mentionNames = mentions.keys.mkString("[", ", ", "]")
      val partialNames = partialGroups.map(_._1).mkString("[", ", ", "]")
      s"name = $name, ## = ${this.##}, tok = $tok, inst = ${inst.toString}, dir = $dir, groupNames = $groupNames, mentionNames = $mentionNames, partialNames = $partialNames ..."
    }

    def isDone: Boolean = inst == Done
    def isReallyDone: Boolean = isDone
    def results: Seq[Result] = Seq(Result(Evidence(this), groups, mentions))
  }

  private case class ThreadBundle(bundles: Seq[Seq[Thread]]) extends Thread {
    println(s"${this.name}: ${this.hashCode}")
    // at least one thread is done and the threads after the threadbundle can be dropped
    def isDone: Boolean = bundles.exists(_.exists(_.isDone))
    // all bundles are done and we can retrieve the results
    def isReallyDone: Boolean = bundles.forall(_.head.isReallyDone)
    def results: Seq[Result] = for {
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
  ): Seq[Result] = {

    // Executes instruction on token and returns the produced threads.
    // Threads are created by following all no-Match instructions.
    def mkThreads(
        tok: Int,
        inst: Inst,
        dir: Direction = LeftToRight,
        groups: NamedGroups = ListMap.empty,
        mentions: NamedMentions = ListMap.empty,
        partialGroups: PartialGroups = Nil
    ): Seq[Thread] = {
      @annotation.tailrec
      def loop(
          is: List[(Inst, NamedGroups, NamedMentions, PartialGroups)],
          ts: List[Thread]
      ): Seq[Thread] = is match {
        case Nil => ts.reverse
        case (i, gs, ms, pgs) :: rest => i match {
          case i: Pass => loop((i.next, gs, ms, pgs) :: rest, ts)
          case i: Split => loop((i.lhs, gs, ms, pgs) :: (i.rhs, gs, ms, pgs) :: rest, ts)
          case i: SaveStart => loop((i.next, gs, ms, (i.name, tok) :: pgs) :: rest, ts)
          case i: SaveEnd => pgs match {
            case (name, start) :: partials if name == i.name =>
              val updatedGroups = gs.getOrElse(name, Vector.empty) :+ Interval(start, tok)
              loop((i.next, gs + (name -> updatedGroups), ms, partials) :: rest, ts)
            case _ => sys.error("unable to close capture")
          }
          case i => loop(rest, SingleThread(tok, i, dir, gs, ms, pgs) :: ts)
        }
      }
      // return threads produced by `inst`
      loop(List((inst, groups, mentions, partialGroups)), Nil)
    }

    // Advance thread by executing instruction.
    // Instruction is expected to be a Match instruction.
    def stepSingleThread(t: SingleThread): Seq[Thread] = t.inst match {
      case i: MatchToken if doc.sentences(sent).words.isDefinedAt(t.tok) && i.c.matches(t.tok, sent, doc, state) =>
        val nextTok = if (t.dir == LeftToRight) t.tok + 1 else t.tok - 1
        mkThreads(nextTok, i.next, t.dir, t.groups, t.mentions, t.partialGroups)
      case i: MatchSentenceStart if (t.tok == 0) || (t.dir == RightToLeft && t.tok == -1) =>
        mkThreads(t.tok, i.next, t.dir, t.groups, t.mentions, t.partialGroups)
      case i: MatchSentenceEnd if t.tok == doc.sentences(sent).size =>
        mkThreads(t.tok, i.next, t.dir, t.groups, t.mentions, t.partialGroups)
      case i: MatchLookAhead =>
        val startTok = if (t.dir == LeftToRight) t.tok else t.tok + 1
        val results = eval(mkThreads(startTok, i.start, LeftToRight))
        if (i.negative == results.isEmpty) {
          mkThreads(t.tok, i.next, t.dir, t.groups, t.mentions, t.partialGroups)
        } else {
          Nil
        }
      case i: MatchLookBehind =>
        val startTok = if (t.dir == LeftToRight) t.tok - 1 else t.tok
        val results = if (startTok < 0) None else eval(mkThreads(startTok, i.start, RightToLeft))
        if (i.negative == results.isEmpty) {
          mkThreads(t.tok, i.next, t.dir, t.groups, t.mentions, t.partialGroups)
        } else {
          Nil
        }
      case i: MatchMention =>
        val bundles = for {
          m <- retrieveMentions(state, sent, t.tok, i.m, i.arg)
          if (t.dir == LeftToRight && t.tok == m.start) || (t.dir == RightToLeft && t.tok == m.end - 1)
          captures = mkMentionCapture(t.mentions, i.name, m)
          nextTok = if (t.dir == LeftToRight) m.end else m.start - 1
        } yield mkThreads(nextTok, i.next, t.dir, t.groups, captures, t.partialGroups)
        bundles match {
          case Seq() => Nil
          case Seq(bundle) => bundle
          case bundles => Seq(ThreadBundle(bundles))
        }
      case Done => Seq(t)
      case _ => Nil  // thread died with no match
    }

    def retrieveMentions(
        state: State,
        sentence: Int,
        token: Int,
        matcher: StringMatcher,
        argument: Option[String]
    ): Seq[Mention] = {
      val mentions = for {
        mention <- state.mentionsFor(sentence, token)
        if mention matches matcher
        result <- argument match {
          case None => Seq(mention)
          case Some(name) if name equalsIgnoreCase "trigger" =>
            mention match {
              case event: EventMention => Seq(event.trigger)
              case _ => Nil
            }
          case Some(name) => mention.arguments.getOrElse(name, Nil)
        }
      } yield result
      // the same mention may be the argument of many mentions
      // so we may encounter it many times
      val distinctMentions = mentions.distinct
      if (mentions.length != distinctMentions.length)
        println("Is this correct?")
      else
        println("Yes, it is.")
      distinctMentions
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

    def stepThreads(threads: Seq[Thread]): Seq[Thread] = {
      val indistincts = threads flatMap stepThread
      val distincts = indistincts.distinct

      indistincts.foreach { indistinct =>
        if (indistinct.hashCode != indistinct.##)
          println("This shouldn't happen.")
      }

      if (indistincts.length != distincts.length) {
        println("Some threads were removed.")
        val distincts = indistincts.distinct
        // Find out which ones.
        import java.util.{IdentityHashMap => JIdentityHashMap}
        val distinctMap = new JIdentityHashMap[Thread, Int]()
        distincts.foreach { thread =>
          distinctMap.put(thread, distinctMap.size)
        }
        indistincts.foreach { thread =>
          print(s"${thread.name}: ${thread.##},")
        }
        println()
        indistincts.foreach { indistinct =>
          if (!distinctMap.containsKey(indistinct)) {
            val name = indistinct.name
            println(s"Thread $name: ${indistinct.##} has been removed!")
            // Check to see if hash code matches anything in the distinct ones.  It should!

            var matches = false
            distincts.foreach { case distinct =>
              if (distinct.## == indistinct.##)
                matches = true
            }
            if (!matches)
              println("Why was it removed?")

//            if (indistinct.isInstanceOf[SingleThread]) {
              // So there should be one among the distincts with the same posId.
//              val indistinctThread = indistinct.asInstanceOf[SingleThread]
//              if (!distincts.exists { distinct =>
//                distinct.isInstanceOf[SingleThread] && distinct.asInstanceOf[SingleThread].posId == indistinctThread..posId
//              })
//                println("Why was it deleted?")
//            }
          }
        }
      }
      distincts
    }

    def handleDone(threads: Seq[Thread]): (Seq[Thread], Option[Thread]) = {
      val doneThreads = threads.filter(_.isDone)
      if (doneThreads.length > 1) {
        println("More than one thread is done.")
        val distincts = doneThreads.distinct

        if (distincts.length != doneThreads.length)
          println("This could go wrong!")
      }

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
    }

    @annotation.tailrec
    def eval(threads: Seq[Thread], currentResult: Option[Thread] = None): Option[Thread] = {
      if (threads.isEmpty) currentResult
      else {
        val (ts, nextResult) = handleDone(threads)
        eval(stepThreads(ts), nextResult orElse currentResult)
      }
    }

    // evaluate pattern and return results
    eval(mkThreads(tok, start)).map(_.results).getOrElse(Nil)
  }
}
