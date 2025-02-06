package org.clulab.odin.debugger.debugging

import org.clulab.odin.State
import org.clulab.odin.debugger.Debugger
import org.clulab.odin.impl.{Done, Inst, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, Pass, SaveEnd, SaveStart, Split, ThompsonVM}
import org.clulab.odin.impl.ThompsonVM.{Direction, NamedGroups, NamedMentions, PartialGroups, SingleThread, Thread, ThreadBundle}
import org.clulab.processors.Document
import org.clulab.struct.Interval

import scala.annotation.tailrec

object DebuggingThompsonVM {
  import Direction._

  class DebuggingEvaluator(
    debugger: Debugger,
    start: Inst,
    tok: Int,
    sent: Int,
    doc: Document,
    state: State
  ) extends ThompsonVM.Evaluator(start, tok, sent, doc, state) {

    // Executes instruction on token and returns the produced threads.
    // Threads are created by following all no-Match instructions.
    override def mkThreads(
      tok: Int,
      inst: Inst,
      dir: Direction = LeftToRight,
      groups: NamedGroups = Map.empty,
      mentions: NamedMentions = Map.empty,
      partialGroups: PartialGroups = Nil,
      prevThreadOpt: Option[Thread] // TODO: Should this be a single thread?
    ): Seq[Thread] = debugger.debugTok(tok) {

      // @annotation.tailrec
      def loop(
        internals: List[(Inst, NamedGroups, NamedMentions, PartialGroups)],
        threads: List[Thread]
      ): Seq[Thread] = {
        // This changes the Inst, but keeps the same Tok.
        internals match {
          case Nil => threads.reverse
          case (headInst, headGroups, headMentions, headPartialGroups) :: rest => debugger.debugInst(headInst) {
            headInst match {
              case inst: Pass =>
                debugger.debugMatches(true, tok, inst)
                prevThreadOpt.foreach(printThread) // It was on that thread and inst passed, and now trying what pass leads to.
                // So, pass works at this token and should see the inst in table.
                // Do I want to add the thread there as well?
                // For which threads did it pass and for which did it fail?
                // Add inst.getNext to front of internals and leave everything else the same.
                loop((inst.getNext, headGroups, headMentions, headPartialGroups) :: rest, threads)
              case inst: Split =>
                debugger.debugMatches(true, tok, inst)
                // Add inst.lhs and inst.rhs to the front of internals and leave everything else the same.
                loop((inst.lhs, headGroups, headMentions, headPartialGroups) :: (inst.rhs, headGroups, headMentions, headPartialGroups) :: rest, threads)
              case inst: SaveStart =>
                debugger.debugMatches(true, tok, inst)
                // Add inst.getNext to the front of internals and also (inst.name, tok) to the headPartialGroups and leave everything else the same.
                loop((inst.getNext, headGroups, headMentions, (inst.name, tok) :: headPartialGroups) :: rest, threads)
              case inst: SaveEnd =>
                headPartialGroups match {
                  // See if what was stored in headPartialGroups matches the name of the inst.
                  case (name, start) :: partials if name == inst.name =>
                    debugger.debugMatches(true, tok, inst)
                    val updatedGroups = headGroups.getOrElse(name, Vector.empty) :+ Interval(start, tok)
                    // Add inst.getNext to the front of the internals and also (name -> updatedGroups) to the headGroups and leave everything else the same.
                    // Replace the partials with the remaining ones now that the head has been matched.
                    loop((inst.getNext, headGroups + (name -> updatedGroups), headMentions, partials) :: rest, threads)
                  case _ =>
                    sys.error("unable to close capture")
                }
              // For any other kind of instance, add it to the threads that will in the end be the result.
              case inst: Inst =>
                //                if (inst == Done)
                //                  debugger.debugMatches(true, tok, inst) // Maybe wait on this.  Turn each thread into a finished thread with result and reason why.
                //                else
                //                  println("What is this?")

                val singleThread = SingleThread(tok, inst, dir, headGroups, headMentions, headPartialGroups, prevThreadOpt)
                // Turn whatever else is at the head into a SingleThread and prepend it to existing threads.
                // If the inst is Done, then it is sitting there waiting to be recognized as is anything else not in the match above.
                loop(rest, singleThread :: threads)
            }
          }
        }
      }

      // Return the Threads produced by inst.
      // Notice that tok and dir are not in the list.  They always come from method arguments.
      val threads = loop(List((inst, groups, mentions, partialGroups)), Nil)

      // TODO: Record demise of prevThreadOpt that doesn't continue?
      //      if (threads.isEmpty)
      //        debugger.debugMatches(false) // This would be for the thread then?
      threads.foreach { thread =>
        printThread(thread)
      }
      threads
    }

    // Add some kind of 5(tok) <- 4(tok) <-
    // Showing what the prev thread was and for each the token where it matched.
    def printThread(thread: Thread): Unit = {
      thread match {
        case thread: SingleThread =>
          println(s"Debugging single thread with tok = ${thread.tok} and inst.posId = ${thread.inst.getPosId}")

          thread.prevThreadOpt.foreach(printThread)
        case threadBundle: ThreadBundle =>
          println(s"Debugging thread bundle: $threadBundle")
          threadBundle.bundles.foreach { bundle =>
            bundle.foreach { thread =>
              printThread(thread)
            }
          }
      }
    }

    // Advance the Thread by executing its instruction (Inst).
    // The Inst is expected to be a Match_ instruction.
    override def stepSingleThread(t: SingleThread): Seq[Thread] = debugger.debugTok(t.tok) {
      debugger.debugInst(t.inst) {
        printThread(t)
        val prevThreadOpt = Some(t)

        t.inst match {
          case i: MatchToken =>
            val matches = doc.sentences(sent).words.isDefinedAt(t.tok) && i.c.matches(t.tok, sent, doc, state)

            // TODO: Add thread to this and turn into done thread with reason being not matches.
            debugger.debugMatches(matches, t.tok, i)
            if (matches) {
              val nextTok = if (t.dir == LeftToRight) t.tok + 1 else t.tok - 1

              mkThreads(nextTok, i.getNext, t.dir, t.groups, t.mentions, t.partialGroups, prevThreadOpt)
            }
            else Nil
          case i: MatchSentenceStart =>
            val matches = (t.tok == 0) || (t.dir == RightToLeft && t.tok == -1)

            debugger.debugMatches(matches, t.tok, i) // TODO: Account for false
            if (matches) {
              mkThreads(t.tok, i.getNext, t.dir, t.groups, t.mentions, t.partialGroups, prevThreadOpt)
            }
            else Nil
          case i: MatchSentenceEnd => // TODO: Account for false
            val matches = t.tok == doc.sentences(sent).size

            debugger.debugMatches(matches, t.tok, i)
            if (matches) {
              mkThreads(t.tok, i.getNext, t.dir, t.groups, t.mentions, t.partialGroups, prevThreadOpt)
            }
            else Nil
          case i: MatchLookAhead => // TODO: Account for false
            val startTok = if (t.dir == LeftToRight) t.tok else t.tok + 1
            // So this bunch of threads has to match first.
            val results = evalThreads(mkThreads(startTok, i.start, LeftToRight, prevThreadOpt = None)) // TODO: Record as dependency?
            println("Eval threads first, so side rail.")
            val matches = i.negative == results.isEmpty

            debugger.debugMatches(matches, t.tok, i)
            if (matches) {
              mkThreads(t.tok, i.getNext, t.dir, t.groups, t.mentions, t.partialGroups, prevThreadOpt)
            }
            else Nil
          case i: MatchLookBehind =>
            val startTok = if (t.dir == LeftToRight) t.tok - 1 else t.tok
            // So this bunch of threads has to match first.
            val results =
              if (startTok < 0) None
              else evalThreads(mkThreads(startTok, i.start, RightToLeft, prevThreadOpt = None)) // TODO: Record as dependency?
            println("Eval threads first, so side rail.")
            val matches = i.negative == results.isEmpty

            debugger.debugMatches(matches, t.tok, i) // Record reason as lookbehind was unsuccessful.
            if (matches) {
              mkThreads(t.tok, i.getNext, t.dir, t.groups, t.mentions, t.partialGroups, prevThreadOpt)
            }
            else Nil
          case i: MatchMention =>
            val mentions = retrieveMentions(state, sent, t.tok, i.m, i.arg)
            val bundles: Seq[Seq[Thread]] = mentions
              .filter { m =>
                val matches = (t.dir == LeftToRight && t.tok == m.start) || (t.dir == RightToLeft && t.tok == m.end - 1)

                matches
              }
              .map { m =>
                val captures = mkMentionCapture(t.mentions, i.name, m)
                val nextTok = if (t.dir == LeftToRight) m.end else m.start - 1

                // Everything that gets bundled later will already have a prevThreadOpt.
                mkThreads(nextTok, i.getNext, t.dir, t.groups, captures, t.partialGroups, prevThreadOpt)
              }
            val matches = bundles.nonEmpty

            debugger.debugMatches(matches, t.tok, i) // TODO. If mkThreads is empty, then prevThreadOpt essentially dies, so need to note.
            bundles match {
              case Seq() => Nil
              case Seq(bundle) => bundle
              case bundles => Seq(ThreadBundle(bundles))
            }
          case Done =>
            debugger.debugMatches(true, t.tok, t.inst) // Don't think this ever gets called.
            Seq(t)
          case _ =>
            debugger.debugMatches(false, t.tok, t.inst) // TODO: Kill the thread as well // Other inst
            debugger.debugThread(t, false, Some("Died with no match"))
            Nil // The Thread died with no match.
        }
      }
    }

    def debugThread(thread: Thread, matches: Boolean, reasonOpt: Option[String]): Unit = {
      thread match {
        case singleThread: SingleThread =>
          debugger.debug(singleThread, matches, reasonOpt)
        case threadBundle: ThreadBundle =>
          threadBundle.bundles.foreach { bundle =>
            bundle.foreach { thread =>
              debugThread(thread, matches, reasonOpt)
            }
          }
      }
    }

    def debugDoneThread(thread: Thread): Unit = {
      thread match {
        case singleThread: SingleThread =>
          if (singleThread.inst == Done) {
            // This will at least be good for the table.
            // Should it really by the prevThread thing instead?
            debugger.debugMatches(true, singleThread.tok, singleThread.inst)
          }
        case threadBundle: ThreadBundle =>
          threadBundle.bundles.foreach { bundle =>
            bundle.foreach { thread =>
              debugDoneThread(thread)
            }
          }
      }
    }

    override def handleDone(threads: Seq[Thread]): (Seq[Thread], Option[Thread]) = {
      // This finds the first one.  Any subsequent duplicates will not survive.
      val doneThreadIndex = threads.indexWhere(_.isDone)
      val doneThreadIndexOpt = if (doneThreadIndex < 0) None else Some(doneThreadIndex)
      val doneThreadOpt = doneThreadIndexOpt.map(threads(_))

      // Not all of these will actually be considered, but that is tracked with the threads.
      threads.foreach(debugDoneThread)
      doneThreadOpt match {
        // No thread has finished; return them all.
        case None => (threads, None)
        // A ThreadBundle is done, but is it really done?
        case Some(thread: ThreadBundle) =>
          val survivors = threads.slice(0, doneThreadIndex) // Exclude doneThreadIndex.
          val victims = threads.slice(doneThreadIndex + 1, threads.length)
          // If it isReallyDone, then the idea of survivors is clear.
          // If it isn't really done, should they all survive or should
          // one continue the search for one that isReallyDone?
          val result =
              if (thread.isReallyDone) {
                debugThread(thread, true, Some("It was really done."))
                (survivors, Some(thread))
              } // TODO: Mark this last one complete with the debugger.
              else
                (survivors :+ thread, None) // This sends it to the end of the line.

          victims.foreach(debugThread(_, false, Some("It was located after the surviving thread.")))
          result
        // A Thread finished.  Drop all Threads to its right but keep the ones to its left.
        case Some(thread: SingleThread) =>
          val survivors = threads.slice(0, doneThreadIndex) // Exclude doneThreadIndex.
          val victims = threads.slice(doneThreadIndex + 1, threads.length)

          debugger.debugThread(thread, true, Some("It was the first found that was done."))
          victims.foreach(debugThread(_, false, Some("It was located after the surviving thread.")))
          (survivors, Some(thread)) // This last thread finished, the ones after takeWhile failed.  Didn't get there first.
      }
    }
  }

  object DebuggingEvaluator {

    def apply(
      debugger: Debugger,
      start: Inst,
      tok: Int,
      sent: Int,
      doc: Document,
      state: State
    ): DebuggingEvaluator = new DebuggingEvaluator(debugger, start, tok, sent, doc, state)
  }

  def evaluate(
    debugger: Debugger,
    start: Inst,
    tok: Int,
    sent: Int,
    doc: Document,
    state: State
  ): Seq[(NamedGroups, NamedMentions)] = {
    val evaluator = DebuggingEvaluator(debugger, start, tok, sent, doc, state)
    val result = evaluator
        .eval(tok, start)
        .map(_.results)
        .getOrElse(Nil)

    result
  }
}
