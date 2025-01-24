package org.clulab.odin

import org.clulab.odin.debugger.{Debugger, RealDebugger}
import org.clulab.odin.impl.ThompsonVM.{Direction, NamedGroups, NamedMentions, PartialGroups, PartialMatch, SingleThread, Thread, ThreadBundle}
import org.clulab.odin.impl.TokenPattern.{GlobalCapture, Result}
import org.clulab.odin.impl.{CrossSentenceExtractor, Done, Extractor, GraphExtractor, GraphPattern, Inst, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, OdinConfig, Pass, Priority, SaveEnd, SaveStart, Split, ThompsonVM, TokenExtractor, TokenPattern}
import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.Interval

import scala.reflect.ClassTag

class LocalDebugger {
  def debugDoc[ResultType](document: Document)(block: => ResultType): ResultType = ???

  def debugLoop[ResultType](loop: Int)(block: => ResultType): ResultType = ???

  def debugSentence[ResultType](index: Int, sentence: Sentence)(block: => ResultType): ResultType = ???

  def debugExtractor[ResultType](extractor: Extractor)(block: => ResultType): ResultType = ???

  def debugStart[ResultType](start: Int)(block: => ResultType): ResultType = ???

  def debugInst[ResultType](inst: Inst)(block: => ResultType): ResultType = ???

  def debugMatches(matches: Boolean) = ???

  def debugTok[ResultType](tok: Int)(block: => ResultType): ResultType = ???
}


object DebuggingThompsonVM {
  import Direction._

  class DebuggingEvaluator(
    debugger: LocalDebugger,
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
    ): Seq[Thread] = Debugger.debugTok(tok) {

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
                loop(rest, SingleThread(tok, i, dir, gs, ms, pgs, List.empty[PartialMatch]) :: ts)
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
            matchTokens(const.toString) = token
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

    def apply( debugger: LocalDebugger,
       start: Inst,
       tok: Int,
       sent: Int,
       doc: Document,
       state: State
     ): DebuggingEvaluator = new DebuggingEvaluator(debugger, start, tok, sent, doc, state)
  }

  def evaluate(
    debugger: LocalDebugger,
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



class DebuggingTokenPattern(debugger: LocalDebugger, start: Inst) extends TokenPattern(start) {

  override def findPrefixOf(tok: Int, sent: Int, doc: Document, state: State): Seq[Result] = {
    DebuggingThompsonVM.evaluate(debugger, start, tok, sent, doc, state) map {
      case (groups, mentions) =>
        // there must be one GlobalCapture only
        val globalCapture = groups(GlobalCapture).head
        Result(globalCapture, groups - GlobalCapture, mentions)
    }
  }

  override def findFirstIn(tok: Int, sent: Int, doc: Document, state: State): Seq[Result] = {
    val n = doc.sentences(sent).size

    @annotation.tailrec
    def loop(i: Int): Seq[Result] = {
      if (i < n) {
        val r = debugger.debugStart(i) {
          findPrefixOf(i, sent, doc, state)
        }
        if (r.nonEmpty) r
        else loop(i + 1)
      }
      else Nil
    }

    loop(tok)
  }
}

object DebuggingTokenPattern {

  def apply(debugger: LocalDebugger, tokenPattern: TokenPattern): DebuggingTokenPattern = {
    new DebuggingTokenPattern(
      debugger,
      tokenPattern.start
    )
  }
}

class DebuggingTokenExtractor(
   val debugger: LocalDebugger,
   name: String,
   labels: Seq[String],
   priority: Priority,
   keep: Boolean,
   action: Action,
   pattern: TokenPattern // This may need to be copied
 ) extends TokenExtractor(name, labels, priority, keep, action, pattern) {

  // This comes indirectly through Extractor.
  override def findAllIn(doc: Document, state: State): Seq[Mention] = debugger.debugExtractor(this) {
    super.findAllIn(doc, state)
  }

  override def findAllIn(sent: Int, doc: Document, state: State): Seq[Mention] = debugger.debugSentence(sent, doc.sentences(sent)) {
    super.findAllIn(sent, doc, state)
  }
}

object DebuggingTokenExtractor {

  def apply(debugger: LocalDebugger, tokenExtractor: TokenExtractor): DebuggingTokenExtractor = {
    new DebuggingTokenExtractor(
      debugger,
      tokenExtractor.name,
      tokenExtractor.labels,
      tokenExtractor.priority,
      tokenExtractor.keep,
      tokenExtractor.action,
      tokenExtractor.pattern
    )
  }
}

class DebuggingGraphExtractor(
  val debugger: LocalDebugger,
  name: String,
  labels: Seq[String],
  priority: Priority,
  keep: Boolean,
  action: Action,
  pattern: GraphPattern,
  config: OdinConfig
) extends GraphExtractor(name, labels, priority, keep, action, pattern, config) {

  // This comes indirectly through Extractor.
  override def findAllIn(doc: Document, state: State): Seq[Mention] = debugger.debugExtractor(this) {
    super.findAllIn(doc, state)
  }

  override def findAllIn(sent: Int, doc: Document, state: State): Seq[Mention] = debugger.debugSentence(sent, doc.sentences(sent)) {
    super.findAllIn(sent, doc, state)
  }
}

object DebuggingGraphExtractor {

  def apply(debugger: LocalDebugger, graphExtractor: GraphExtractor): DebuggingGraphExtractor = {
    new DebuggingGraphExtractor(
      debugger,
      graphExtractor.name,
      graphExtractor.labels,
      graphExtractor.priority,
      graphExtractor.keep,
      graphExtractor.action,
      graphExtractor.pattern,
      graphExtractor.config
    )
  }
}

class DebuggingCrossSentenceExtractor(
  val debugger: LocalDebugger,
  name: String,
  labels: Seq[String],
  priority: Priority,
  keep: Boolean,
  action: Action,
  leftWindow: Int,
  rightWindow: Int,
  anchorPattern: TokenExtractor,
  neighborPattern: TokenExtractor,
  anchorRole: String,
  neighborRole: String
) extends CrossSentenceExtractor(name, labels, priority, keep, action, leftWindow, rightWindow, anchorPattern,
    neighborPattern, anchorRole, neighborRole) {

}

object DebuggingCrossSentenceExtractor {

  def apply(debugger: LocalDebugger, crossSentenceExtractor: CrossSentenceExtractor): DebuggingCrossSentenceExtractor = {
    new DebuggingCrossSentenceExtractor(
      debugger,
      crossSentenceExtractor.name,
      crossSentenceExtractor.labels,
      crossSentenceExtractor.priority,
      crossSentenceExtractor.keep,
      crossSentenceExtractor.action,
      crossSentenceExtractor.leftWindow,
      crossSentenceExtractor.rightWindow,
      crossSentenceExtractor.anchorPattern,
      crossSentenceExtractor.neighborPattern,
      crossSentenceExtractor.anchorRole,
      crossSentenceExtractor.neighborRole
    )
  }
}

class InnerDebuggingExtractorEngine(val debugger: LocalDebugger, extractors: Vector[Extractor], globalAction: Action)
    extends ExtractorEngine(extractors, globalAction) {

  override protected def extract(document: Document, i: Int, state: State): Seq[Mention] = debugger.debugLoop(i) {
    super.extract(document, i, state)
  }

  override def extractFrom(document: Document, initialState: State): Seq[Mention] = debugger.debugDoc(document) {
    super.extractFrom(document, initialState)
  }
}

object InnerDebuggingExtractorEngine {

  def apply(debugger: LocalDebugger, extractorEngine: ExtractorEngine): InnerDebuggingExtractorEngine = {
    val extractors = extractorEngine.extractors
    val debuggingExtractors = extractors.map { extractor =>
      extractor match {
        case extractor: TokenExtractor => DebuggingTokenExtractor(debugger, extractor)
        case extractor: GraphExtractor => DebuggingGraphExtractor(debugger, extractor)
        case extractor: CrossSentenceExtractor => ???
      }
    }
    val globalAction = extractorEngine.globalAction

    new InnerDebuggingExtractorEngine(debugger, debuggingExtractors, globalAction)
  }
}

class DebuggingExtractorEngine protected (extractors: Vector[Extractor], globalAction: Action)
    extends ExtractorEngine(extractors, globalAction) {

  // We do have a list of extractors here and they are indexed.
  // Maybe in the log we need to store the index rather than extractor itself.
  // The debugging extractor needs to have an index then.
  // What if an extractor is present twice?  getIndex could return multiple values
  // and the user would have to decide?
  // Get records for particular extractor
  // By sentence
  // Get the trace and get any extractor details
  // Send those to the debugger
  // Call this Tracer rather than Debugger?
  // May depend on what all it needs do to.

  override def extractFrom(doc: Document): Seq[Mention] = {
    val debugger =  new LocalDebugger()
    val inner = InnerDebuggingExtractorEngine(debugger, this)
    val result = inner.extractFrom(doc)
    // Get stuff out of the debugger
    // Save it in a synchronized way to the transcript

    result
  }

  override def extractFrom(document: Document, initialState: State): Seq[Mention] = {
    val debugger = new LocalDebugger()
    val inner = InnerDebuggingExtractorEngine(debugger, this)
    val result = inner.extractFrom(document, initialState)
    // Get stuff out of the debugger
    // Save it in a synchronized way to the transcript

    result
  }

  override def extractByType[M <: Mention : ClassTag](document: Document, initialState: State): Seq[M] = {
    val debugger = new LocalDebugger()
    val inner = InnerDebuggingExtractorEngine(debugger, this)
    val result = inner.extractByType[M](document, initialState)
    // Get stuff out of the debugger
    // Save it in a synchronized way to the transcript

    result
  }

  // Need extra stuff to fish out the right extractor
  // Need to get out the right extractor, how?
  // Maybe get out the right inst of it by number?
  // Then need document and sentence.
  // Write various kinds of queries.
}

object DebuggingExtractorEngine {

  def apply(extractorEngine: ExtractorEngine): DebuggingExtractorEngine = {
    val extractors = extractorEngine.extractors
    val debuggingExtractors = extractors.map { extractor =>
      extractor match {
        case extractor: TokenExtractor => ??? // Make one with en empty debugger spot to be filled in when another copy made
        case extractor: GraphExtractor => ???
        case extractor: CrossSentenceExtractor => ???
      }
    }
    val globalAction = extractorEngine.globalAction

    new DebuggingExtractorEngine(debuggingExtractors, globalAction)

  }
}