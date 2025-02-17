package org.clulab.odin.debugger.debugging

import org.clulab.odin.State
import org.clulab.odin.debugger.{Debugger, ThreadMatch}
import org.clulab.odin.impl.{Done, Inst, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, Pass, SaveEnd, SaveStart, Split, ThompsonVM}
import org.clulab.odin.impl.ThompsonVM.{Direction, NamedGroups, NamedMentions, PartialGroups, SingleThread, Thread, ThreadBundle}
import org.clulab.processors.Document
import org.clulab.struct.Interval

import scala.annotation.tailrec

object DebuggingThompsonVM {
  import Direction._

  val noThreads = Seq.empty[SingleThread]

  object DebuggingThread {

    def withSingleThreads[T](thread: Thread)(f: SingleThread => Unit): Unit = {

      // TODO: Make this tail recursive!
      def loop(thread: Thread): Unit = {
        thread match {
          case singleThread: SingleThread => f(singleThread)
          case threadBundle: ThreadBundle =>
            threadBundle.bundles.foreach { bundle =>
              bundle.foreach { thread =>
                loop(thread)
              }
            }
        }
      }

      loop(thread)
    }
  }

  class DebuggingEvaluator(
    debugger: Debugger,
    start: Inst,
    tok: Int,
    sent: Int,
    doc: Document,
    state: State
  ) extends ThompsonVM.Evaluator(start, tok, sent, doc, state) {

    // Executes instruction on token and returns the produced SingleThreads.
    // Threads are created by following all no-Match instructions.
    override def mkThreads(
      tok: Int,
      inst: Inst,
      dir: Direction = LeftToRight,
      groups: NamedGroups = Map.empty,
      mentions: NamedMentions = Map.empty,
      partialGroups: PartialGroups = Nil,
      prevThreadOpt: Option[SingleThread]
    ): Seq[SingleThread] = debugger.debugTok(tok) {

      @tailrec
      def loop(
        internals: List[(Inst, NamedGroups, NamedMentions, PartialGroups)],
        singleThreads: List[SingleThread]
      ): Seq[SingleThread] = {
        // This changes the Inst, but keeps the same Tok.
        internals match {
          case Nil => singleThreads.reverse
          case (headInst, headGroups, headMentions, headPartialGroups) :: rest => /*debugger.debugInst(headInst)*/ {
            headInst match {
              case inst: Pass =>
                debugger.debugInstMatches(true, tok, inst)
                // So, pass works at this token and should see the Inst in table.
                // Add inst.getNext to front of internals and leave everything else the same.
                loop((inst.getNext, headGroups, headMentions, headPartialGroups) :: rest, singleThreads)
              case inst: Split =>
                debugger.debugInstMatches(true, tok, inst)
                // Add inst.lhs and inst.rhs to the front of internals and leave everything else the same.
                loop((inst.lhs, headGroups, headMentions, headPartialGroups) :: (inst.rhs, headGroups, headMentions, headPartialGroups) :: rest, singleThreads)
              case inst: SaveStart =>
                debugger.debugInstMatches(true, tok, inst)
                // Add inst.getNext to the front of internals and also (inst.name, tok) to the headPartialGroups and leave everything else the same.
                loop((inst.getNext, headGroups, headMentions, (inst.name, tok) :: headPartialGroups) :: rest, singleThreads)
              case inst: SaveEnd =>
                headPartialGroups match {
                  // See if what was stored in headPartialGroups matches the name of the inst.
                  case (name, start) :: partials if name == inst.name =>
                    debugger.debugInstMatches(true, tok, inst)
                    val updatedGroups = headGroups.getOrElse(name, Vector.empty) :+ Interval(start, tok)
                    // Add inst.getNext to the front of the internals and also (name -> updatedGroups) to the headGroups and leave everything else the same.
                    // Replace the partials with the remaining ones now that the head has been matched.
                    loop((inst.getNext, headGroups + (name -> updatedGroups), headMentions, partials) :: rest, singleThreads)
                  case _ =>
                    sys.error("unable to close capture")
                }
              // For any other kind of instance, add it to the threads that will in the end be the result.
              case inst: Inst =>
                // This inst is passed up for now but will be recorded later.
                val singleThread = SingleThread(tok, inst, dir, headGroups, headMentions, headPartialGroups, prevThreadOpt)
                // Turn whatever else is at the head into a SingleThread and prepend it to existing threads.
                // If the inst is Done, then it is sitting there waiting to be recognized as is anything else not in the match above.
                loop(rest, singleThread :: singleThreads)
            }
          }
        }
      }

      // Return the Threads produced by inst.
      // Notice that tok and dir are not in the list.  They always come from method arguments.
      val threads = loop(List((inst, groups, mentions, partialGroups)), Nil)

      threads
    }

    // Advance the Thread by executing its instruction (Inst).
    // The Inst is expected to be a Match_ instruction.
    override def stepSingleThread(t: SingleThread): Seq[Thread] = debugger.debugTok(t.tok) {
      val prevThreadOpt = Some(t)
      val newThreads = t.inst match {
        case i: MatchToken =>
          val matches = doc.sentences(sent).words.isDefinedAt(t.tok) && i.c.matches(t.tok, sent, doc, state)

          // TODO: Add thread to this and turn into done thread with reason being not matches.
          debugger.debugInstMatches(matches, t.tok, i)
          if (matches) {
            val nextTok = if (t.dir == LeftToRight) t.tok + 1 else t.tok - 1

            mkThreads(nextTok, i.getNext, t.dir, t.groups, t.mentions, t.partialGroups, prevThreadOpt)
          }
          else {
            debugger.debugThreadMatches(t, false, ThreadMatch.instMismatch)
            noThreads
          }
        case i: MatchSentenceStart =>
          val matches = (t.tok == 0) || (t.dir == RightToLeft && t.tok == -1)

          debugger.debugInstMatches(matches, t.tok, i) // TODO: Account for false
          if (matches) {
            mkThreads(t.tok, i.getNext, t.dir, t.groups, t.mentions, t.partialGroups, prevThreadOpt)
          }
          else {
            debugger.debugThreadMatches(t, false, ThreadMatch.instMismatch)
            noThreads
          }
        case i: MatchSentenceEnd => // TODO: Account for false
          val matches = t.tok == doc.sentences(sent).size

          debugger.debugInstMatches(matches, t.tok, i)
          if (matches) {
            mkThreads(t.tok, i.getNext, t.dir, t.groups, t.mentions, t.partialGroups, prevThreadOpt)
          }
          else {
            debugger.debugThreadMatches(t, false, ThreadMatch.instMismatch)
            noThreads
          }
        case i: MatchLookAhead =>
          val startTok = if (t.dir == LeftToRight) t.tok else t.tok + 1
          // So this bunch of threads has to match first.
          val results = evalThreads(mkThreads(startTok, i.start, LeftToRight, prevThreadOpt = None)) // TODO: Record as dependency?
          val matches = i.negative == results.isEmpty

          debugger.debugInstMatches(matches, t.tok, i)
          if (matches) {
            mkThreads(t.tok, i.getNext, t.dir, t.groups, t.mentions, t.partialGroups, prevThreadOpt)
          }
          else {
            debugger.debugThreadMatches(t, false, ThreadMatch.instMismatch)
            noThreads
          }
        case i: MatchLookBehind =>
          val startTok = if (t.dir == LeftToRight) t.tok - 1 else t.tok
          // So this bunch of threads has to match first.
          val results =
            if (startTok < 0) None
            else evalThreads(mkThreads(startTok, i.start, RightToLeft, prevThreadOpt = None)) // TODO: Record as dependency?
          // println("Eval threads first, so side rail.")
          val matches = i.negative == results.isEmpty

          debugger.debugInstMatches(matches, t.tok, i) // Record reason as lookbehind was unsuccessful.
          if (matches) {
            mkThreads(t.tok, i.getNext, t.dir, t.groups, t.mentions, t.partialGroups, prevThreadOpt)
          }
          else {
            debugger.debugThreadMatches(t, false, ThreadMatch.instMismatch)
            noThreads
          }
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

          debugger.debugInstMatches(matches, t.tok, i)
          bundles match {
            case Seq() =>
              // TODO. If mkThreads is empty, then prevThreadOpt essentially dies, so need to note.
              noThreads
            case Seq(bundle) => bundle
            case bundles => Seq(ThreadBundle(bundles))
          }
        case Done =>
          debugger.debugInstMatches(true, t.tok, t.inst) // Don't think this ever gets called.
          Seq(t)
        case _ =>
          debugger.debugInstMatches(false, t.tok, t.inst)
          debugger.debugThreadMatches(t, false, ThreadMatch.empty)
          noThreads // The Thread died with no match.
      }

      newThreads
    }

    def debugThread(thread: Thread, threadMatch: ThreadMatch): Unit = {
      DebuggingThread.withSingleThreads(thread) { singleThread =>
        debugger.debugThreadMatches(singleThread, singleThread.isDone, threadMatch)
      }
    }

    def debugMatchesDone(thread: Thread): Unit = {
      DebuggingThread.withSingleThreads(thread) { singleThread =>
        if (singleThread.inst == Done) {
          // This will at least be good for the table.
          // Should it really use the prevThreadOpt instead so that
          // the recorded tok is one behind, making an inclusive range?
          // TODO: Experiment with this.
          debugger.debugInstMatches(true, singleThread.tok, singleThread.inst)
        }
      }
    }

    override def handleDone(threads: Seq[Thread]): (Seq[Thread], Option[Thread]) = {
      // This finds the first one.  Any subsequent duplicates will not survive.
      val doneThreadIndex = threads.indexWhere(_.isDone)
      val doneThreadIndexOpt = if (doneThreadIndex < 0) None else Some(doneThreadIndex)
      val doneThreadOpt = doneThreadIndexOpt.map(threads(_))

      // Not all of these will actually be considered, but that is tracked with the threads.
      threads.foreach(debugMatchesDone)
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
                // TODO: Again, this might not be used, so don't debug it yet.
                // debugThread(thread, true, Some("It was really done."))
                (survivors, Some(thread))
              } // TODO: Mark this last one complete with the debugger.
              else
                (survivors :+ thread, None) // This sends it to the end of the line.

          victims.foreach(debugThread(_, ThreadMatch.lowerPriority))
          result
        // A Thread finished.  Drop all Threads to its right but keep the ones to its left.
        case Some(thread: SingleThread) =>
          val survivors = threads.slice(0, doneThreadIndex) // Exclude doneThreadIndex.
          val victims = threads.slice(doneThreadIndex + 1, threads.length)

          // Even though the thread is being returned, it may not be used.
          // debugger.debugThread(thread, true, Some("It was the first found that was done."))
          victims.foreach(debugThread(_, ThreadMatch.lowerPriority))
          (survivors, Some(thread)) // This last thread finished, the ones after takeWhile failed.  Didn't get there first.
      }
    }

    @annotation.tailrec
    private def localInnerEvalThreads(threads: Seq[Thread], resultOpt: Option[Thread] = None): Option[Thread] = {
      // TODO: This is a good place to record the threads that are being evaluated and maybe depth.
      if (threads.isEmpty) {
        resultOpt.foreach { thread =>
          debugThread(thread, ThreadMatch.survivor)
        }
        resultOpt
      }
      else {
        // Threads that move to nextThreads will eventually be counted.
        // The nextResultOpt
        val (nextThreads, nextResultOpt) = handleDone(threads)
        val steppedThreads = stepThreads(nextThreads)

//        nextThreads.foreach { thread =>
//          debugThread(thread, false, Some("It got stepped."))
//        }
        if (nextResultOpt.isDefined) {
          resultOpt.foreach { thread =>
            debugThread(thread, ThreadMatch.superseded)
          }
        }
        localInnerEvalThreads(steppedThreads, nextResultOpt.orElse(resultOpt))
      }
    }

    override def evalThreads(threads: Seq[Thread], result: Option[Thread] = None): Option[Thread] = {
      localInnerEvalThreads(threads, result)
    }

    override def eval(tok: Int, start: Inst): Option[Thread] = {
      val singleThreads = mkThreads(tok, start, prevThreadOpt = None)
      val threadOpt = evalThreads(singleThreads)

      threadOpt
    }
  }

  def evaluate(
    debugger: Debugger,
    start: Inst,
    tok: Int,
    sent: Int,
    doc: Document,
    state: State
  ): Seq[(NamedGroups, NamedMentions)] = {
    val evaluator = new DebuggingEvaluator(debugger, start, tok, sent, doc, state)
    val result = evaluator
        .eval(tok, start)
        .map(_.results)
        .getOrElse(Nil)

    result
  }
}
