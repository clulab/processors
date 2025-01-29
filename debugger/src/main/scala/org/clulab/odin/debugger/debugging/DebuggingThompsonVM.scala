package org.clulab.odin.debugger.debugging

import org.clulab.odin.State
import org.clulab.odin.debugger.Debugger
import org.clulab.odin.impl.{Done, Inst, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, Pass, SaveEnd, SaveStart, Split, ThompsonVM}
import org.clulab.odin.impl.ThompsonVM.{Direction, NamedGroups, NamedMentions, PartialGroups, SingleThread, Thread, ThreadBundle}
import org.clulab.processors.Document
import org.clulab.struct.Interval

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
      partialGroups: PartialGroups = Nil
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
                debugger.debugMatches(true)
                // Add inst.getNext to front of internals and leave everything else the same.
                loop((inst.getNext, headGroups, headMentions, headPartialGroups) :: rest, threads)
              case inst: Split =>
                debugger.debugMatches(true)
                // Add inst.lhs and inst.rhs to the front of internals and leave everything else the same.
                loop((inst.lhs, headGroups, headMentions, headPartialGroups) :: (inst.rhs, headGroups, headMentions, headPartialGroups) :: rest, threads)
              case inst: SaveStart =>
                debugger.debugMatches(true)
                // Add inst.getNext to the front of internals and also (inst.name, tok) to the headPartialGroups and leave everything else the same.
                loop((inst.getNext, headGroups, headMentions, (inst.name, tok) :: headPartialGroups) :: rest, threads)
              case inst: SaveEnd =>
                headPartialGroups match {
                  // See if what was stored in headPartialGroups matches the name of the inst.
                  case (name, start) :: partials if name == inst.name =>
                    debugger.debugMatches(true)
                    val updatedGroups = headGroups.getOrElse(name, Vector.empty) :+ Interval(start, tok)
                    // Add inst.getNext to the front of the internals and also (name -> updatedGroups) to the headGroups and leave everything else the same.
                    // Replace the partials with the remaining ones now that the head has been matched.
                    loop((inst.getNext, headGroups + (name -> updatedGroups), headMentions, partials) :: rest, threads)
                  case _ =>
                    debugger.debugMatches(false)
                    sys.error("unable to close capture")
                }
              // For any other kind of instance, add it to the threads that will in the end be the result.
              case inst =>
                // Turn whatever else is at the head into a SingleThread and prepend it to existing threads.
                loop(rest, SingleThread(tok, inst, dir, headGroups, headMentions, headPartialGroups) :: threads)
            }
          }
        }
      }

      // Return the Threads produced by inst.
      // Notice that tok and dir are not in the list.  They always come from method arguments.
      loop(List((inst, groups, mentions, partialGroups)), Nil)
    }

    // Advance the Thread by executing its instruction (Inst).
    // The Inst is expected to be a Match_ instruction.
    override def stepSingleThread(t: SingleThread): Seq[Thread] = debugger.debugTok(t.tok) { debugger.debugInst(t.inst) {
      t.inst match {
        case i: MatchToken =>
          val matches = doc.sentences(sent).words.isDefinedAt(t.tok) && i.c.matches(t.tok, sent, doc, state)

          debugger.debugMatches(matches)
          if (matches) {
            val nextTok = if (t.dir == LeftToRight) t.tok + 1 else t.tok - 1

            mkThreads(nextTok, i.getNext, t.dir, t.groups, t.mentions, t.partialGroups)
          }
          else Nil
        case i: MatchSentenceStart =>
          val matches = (t.tok == 0) || (t.dir == RightToLeft && t.tok == -1)

          debugger.debugMatches(matches)
          if (matches) {
            mkThreads(t.tok, i.getNext, t.dir, t.groups, t.mentions, t.partialGroups)
          }
          else Nil
        case i: MatchSentenceEnd  =>
          val matches = t.tok == doc.sentences(sent).size

          debugger.debugMatches(matches)
          if (matches) {
            mkThreads(t.tok, i.getNext, t.dir, t.groups, t.mentions, t.partialGroups)
          }
          else Nil
        case i: MatchLookAhead =>
          val startTok = if (t.dir == LeftToRight) t.tok else t.tok + 1
          // So this bunch of threads has to match first.
          val results = evalThreads(mkThreads(startTok, i.start, LeftToRight))
          val matches = i.negative == results.isEmpty

          debugger.debugMatches(matches)
          if (matches) {
            mkThreads(t.tok, i.getNext, t.dir, t.groups, t.mentions, t.partialGroups)
          }
          else Nil
        case i: MatchLookBehind =>
          val startTok = if (t.dir == LeftToRight) t.tok - 1 else t.tok
          // So this bunch of threads has to match first.
          val results = if (startTok < 0) None else evalThreads(mkThreads(startTok, i.start, RightToLeft))
          val matches = i.negative == results.isEmpty

          debugger.debugMatches(matches)
          if (matches) {
            mkThreads(t.tok, i.getNext, t.dir, t.groups, t.mentions, t.partialGroups)
          }
          else Nil
        case i: MatchMention =>
          val mentions = retrieveMentions(state, sent, t.tok, i.m, i.arg)
          val bundles = mentions
              .filter { m =>
                val matches = (t.dir == LeftToRight && t.tok == m.start) || (t.dir == RightToLeft && t.tok == m.end - 1)

                matches
              }
              .map { m =>
                val captures = mkMentionCapture(t.mentions, i.name, m)
                val nextTok = if (t.dir == LeftToRight) m.end else m.start - 1

                mkThreads(nextTok, i.getNext, t.dir, t.groups, captures, t.partialGroups)
              }
          val matches = bundles.nonEmpty

          debugger.debugMatches(matches)
          bundles match {
            case Seq() => Nil
            case Seq(bundle) => bundle
            case bundles => Seq(ThreadBundle(bundles))
          }
        case Done =>
          debugger.debugMatches(true)
          Seq(t)
        case _ =>
          debugger.debugMatches(false)
          Nil // The Thread died with no match.
      }
    }}
  }

  object DebuggingEvaluator {

    def apply( debugger: Debugger,
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

    // evaluate pattern and return results
    val result = evaluator.eval(tok, start).map(_.results).getOrElse(Nil)
    result
  }
}
