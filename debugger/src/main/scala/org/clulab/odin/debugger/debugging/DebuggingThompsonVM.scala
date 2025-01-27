package org.clulab.odin.debugger.debugging

import org.clulab.odin.State
import org.clulab.odin.debugger.Debugger
import org.clulab.odin.impl.{Done, Inst, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, Pass, SaveEnd, SaveStart, Split, ThompsonVM}
import org.clulab.odin.impl.ThompsonVM.Direction.{Direction, LeftToRight, RightToLeft}
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

      // TODO: Why is this List while I see Seq and even Vector elsewhere?
      //@annotation.tailrec
      def loop(
        internals: List[(Inst, NamedGroups, NamedMentions, PartialGroups)],
        ts: List[Thread]
      ): Seq[Thread] = {
        // This changes the Inst, but keeps the same Tok.

        internals match {
          case Nil => ts.reverse
          // TODO: Rename these headInst, headGroups, headMentions, headPartialGroups
          case (i, gs, ms, pgs) :: rest => debugger.debugInst(i) {
            i match {
              case i: Pass =>
                debugger.debugMatches(true)
                loop((i.getNext, gs, ms, pgs) :: rest, ts)
              case i: Split =>
                debugger.debugMatches(true)
                loop((i.lhs, gs, ms, pgs) :: (i.rhs, gs, ms, pgs) :: rest, ts)
              case i: SaveStart =>
                debugger.debugMatches(true)
                loop((i.getNext, gs, ms, (i.name, tok) :: pgs) :: rest, ts)
              case i: SaveEnd => pgs match {
                case (name, start) :: partials if name == i.name =>
                  debugger.debugMatches(true)
                  val updatedGroups = gs.getOrElse(name, Vector.empty) :+ Interval(start, tok)
                  loop((i.getNext, gs + (name -> updatedGroups), ms, partials) :: rest, ts)
                case _ =>
                  debugger.debugMatches(false)
                  sys.error("unable to close capture")
              }
              // Here we loop on rest.  Could that have different ms?
              case i =>
                if (i == Done)
                  debugger.debugMatches(true)
                loop(rest, SingleThread(tok, i, dir, gs, ms, pgs) :: ts)
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
            val token = t.tok
            val const = i.c
//            matchTokens(const.toString) = token
//            println(s"Match mentions in singlestepthread is $matchTokens")
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
          val results = evalThreads(mkThreads(startTok, i.start, LeftToRight))
          val matches = i.negative == results.isEmpty

          debugger.debugMatches(matches)
          if (matches) {
            mkThreads(t.tok, i.getNext, t.dir, t.groups, t.mentions, t.partialGroups)
          }
          else Nil
        case i: MatchLookBehind =>
          val startTok = if (t.dir == LeftToRight) t.tok - 1 else t.tok
          val results = if (startTok < 0) None else evalThreads(mkThreads(startTok, i.start, RightToLeft))
          val matches = i.negative == results.isEmpty

          debugger.debugMatches(matches)
          if (matches) {
            mkThreads(t.tok, i.getNext, t.dir, t.groups, t.mentions, t.partialGroups)
          }
          else Nil
        case i: MatchMention =>
          val bundles = retrieveMentions(state, sent, t.tok, i.m, i.arg).flatMap { m =>
            val matches = (t.dir == LeftToRight && t.tok == m.start) || (t.dir == RightToLeft && t.tok == m.end - 1)

            debugger.debugMatches(matches)
            if (matches) {
              val captures = mkMentionCapture(t.mentions, i.name, m)
              val nextTok = if (t.dir == LeftToRight) m.end else m.start - 1
              Some(mkThreads(nextTok, i.getNext, t.dir, t.groups, captures, t.partialGroups))
            }
            else None
          }
  //        val bundlesOld = for {
  //          m <- retrieveMentions(state, sent, t.tok, i.m, i.arg)
  //          if (t.dir == LeftToRight && t.tok == m.start) || (t.dir == RightToLeft && t.tok == m.end - 1)
  //          captures = mkMentionCapture(t.mentions, i.name, m)
  //          nextTok = if (t.dir == LeftToRight) m.end else m.start - 1
  //        } yield mkThreads(nextTok, i.getNext, t.dir, t.groups, captures, t.partialGroups)
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
