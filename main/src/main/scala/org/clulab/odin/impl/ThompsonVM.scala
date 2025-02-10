package org.clulab.odin.impl

import org.clulab.processors.Document
import org.clulab.struct.Interval
import org.clulab.odin._

object ThompsonVM {
  type NamedGroups = Map[String, Seq[Interval]]
  type NamedMentions = Map[String, Seq[Mention]]
  // A PartialGroup is the name and the start of a group, without the end.
  type PartialGroups = List[(String, Int)]

  // enum
  object Direction extends Enumeration {
    type Direction = Value
    val LeftToRight, RightToLeft = Value
  }
  import Direction._

  sealed trait Thread {
    def isDone: Boolean
    def isReallyDone: Boolean
    def results: Seq[(NamedGroups, NamedMentions)]
  }

  case class SingleThread(
    tok: Int,
    inst: Inst,
    dir: Direction,
    groups: NamedGroups,
    mentions: NamedMentions,
    partialGroups: PartialGroups,
    prevThreadOpt: Option[Thread] = None
  ) extends Thread {

    def isDone: Boolean = inst == Done

    def isReallyDone: Boolean = isDone

    def results: Seq[(NamedGroups, NamedMentions)] = Seq((groups, mentions))
  }

  case class ThreadBundle(bundles: Seq[Seq[Thread]]) extends Thread {
    // At least one Thread is done and the Threads after the ThreadBundle can be dropped.
    def isDone: Boolean = bundles.exists(_.exists(_.isDone))

    // all bundles are done and we can retrieve the results
    def isReallyDone: Boolean = bundles.forall(_.head.isReallyDone)

    def results: Seq[(NamedGroups, NamedMentions)] = for {
      threads <- bundles
      thread = threads.head if thread.isReallyDone
      result <- thread.results
    } yield result
  }

  case class Evaluator(start: Inst, tok: Int, sent: Int, doc: Document, state: State) {

    // Executes instruction on token and returns the produced threads.
    // Threads are created by following all no-Match instructions.
    def mkThreads(
      tok: Int,
      inst: Inst,
      dir: Direction = LeftToRight,
      groups: NamedGroups = Map.empty,
      mentions: NamedMentions = Map.empty,
      partialGroups: PartialGroups = Nil,
      prevThreadOpt: Option[Thread] // TODO: Should this be a single thread?
    ): Seq[Thread] = {

      // TODO: Why is this List while I see Seq and even Vector elsewhere?
      @annotation.tailrec
      def loop(
        internals: List[(Inst, NamedGroups, NamedMentions, PartialGroups)],
        ts: List[Thread]
      ): Seq[Thread] = {
        // This changes the Inst, but keeps the same Tok.

        internals match {
          case Nil => ts.reverse
          // TODO: Rename these headInst, headGroups, headMentions, headPartialGroups
          case (i, gs, ms, pgs) :: rest => {
            i match {
              case i: Pass =>
                loop((i.getNext, gs, ms, pgs) :: rest, ts)
              case i: Split =>
                loop((i.lhs, gs, ms, pgs) :: (i.rhs, gs, ms, pgs) :: rest, ts)
              case i: SaveStart =>
                loop((i.getNext, gs, ms, (i.name, tok) :: pgs) :: rest, ts)
              case i: SaveEnd => pgs match {
                case (name, start) :: partials if name == i.name =>
                  val updatedGroups = gs.getOrElse(name, Vector.empty) :+ Interval(start, tok)
                  loop((i.getNext, gs + (name -> updatedGroups), ms, partials) :: rest, ts)
                case _ =>
                  sys.error("unable to close capture")
              }
              // Here we loop on rest.  Could that have different ms?
              case i =>
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
    def stepSingleThread(t: SingleThread): Seq[Thread] = {
      val prevThreadOpt = Some(t)

      t.inst match {
        case i: MatchToken =>
          val matches = doc.sentences(sent).words.isDefinedAt(t.tok) && i.c.matches(t.tok, sent, doc, state)

          if (matches) {
            val nextTok = if (t.dir == LeftToRight) t.tok + 1 else t.tok - 1

            mkThreads(nextTok, i.getNext, t.dir, t.groups, t.mentions, t.partialGroups, prevThreadOpt)
          }
          else Nil
        case i: MatchSentenceStart =>
          val matches = (t.tok == 0) || (t.dir == RightToLeft && t.tok == -1)

          if (matches) {
            mkThreads(t.tok, i.getNext, t.dir, t.groups, t.mentions, t.partialGroups, prevThreadOpt)
          }
          else Nil
        case i: MatchSentenceEnd =>
          val matches = t.tok == doc.sentences(sent).size

          if (matches) {
            mkThreads(t.tok, i.getNext, t.dir, t.groups, t.mentions, t.partialGroups, prevThreadOpt)
          }
          else Nil
        case i: MatchLookAhead =>
          val startTok = if (t.dir == LeftToRight) t.tok else t.tok + 1
          // This is a side track.
          val threads = mkThreads(startTok, i.start, LeftToRight, prevThreadOpt = prevThreadOpt)
          val results = evalThreads(threads)
          val matches = i.negative == results.isEmpty

          if (matches) {
            mkThreads(t.tok, i.getNext, t.dir, t.groups, t.mentions, t.partialGroups, prevThreadOpt)
          }
          else Nil
        case i: MatchLookBehind =>
          val startTok = if (t.dir == LeftToRight) t.tok - 1 else t.tok
          val results = if (startTok < 0) None else evalThreads(mkThreads(startTok, i.start, RightToLeft, prevThreadOpt = prevThreadOpt))
          val matches = i.negative == results.isEmpty

          if (matches) {
            mkThreads(t.tok, i.getNext, t.dir, t.groups, t.mentions, t.partialGroups, prevThreadOpt)
          }
          else Nil
        case i: MatchMention =>
          val mentions = retrieveMentions(state, sent, t.tok, i.m, i.arg)
          val bundles = mentions.flatMap { m =>
            val matches = (t.dir == LeftToRight && t.tok == m.start) || (t.dir == RightToLeft && t.tok == m.end - 1)

            if (matches) {
              val captures = mkMentionCapture(t.mentions, i.name, m)
              val nextTok = if (t.dir == LeftToRight) m.end else m.start - 1

              // Everything that gets bundled later will already have a prevThreadOpt.
              Some(mkThreads(nextTok, i.getNext, t.dir, t.groups, captures, t.partialGroups, prevThreadOpt))
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
          Seq(t)
        case _ =>
          Nil // The Thread died with no match.
      }
    }

    def retrieveMentions(
      state: State,
      sentence: Int,
      token: Int,
      matcher: StringMatcher,
      argument: Option[String]
    ): Seq[Mention] = {
      val mentions = for {
        mention <- state.mentionsFor(sentence, token) if mention matches matcher
        result <- argument match {
          case None => Seq(mention)
          case Some(name) if name.equalsIgnoreCase("trigger") =>
            mention match {
              case event: EventMention => Seq(event.trigger)
              case _ => Nil
            }
          case Some(name) => mention.arguments.getOrElse(name, Nil)
        }
      } yield result

      // The same mention may be the argument of many mentions,
      // so we may encounter it many times.
      mentions.distinct
    }

    def mkMentionCapture(
      mentions: NamedMentions,
      name: Option[String],
      mention: Mention
    ): NamedMentions = name match {
      case None => mentions
      case Some(name) =>
        val updateMentions = mentions.getOrElse(name, Vector.empty) :+ mention
        mentions + (name -> updateMentions)
    }

    def stepThreadBundle(threadBundle: ThreadBundle): Seq[Thread] = {
      val threadBundles = for {
        threads <- threadBundle.bundles
        nextThreads = stepThreads(threads)
        if nextThreads.nonEmpty
        (survivors, resultOpt) = handleDone(nextThreads)
      } yield survivors ++ resultOpt.toSeq

      threadBundles match {
        case Seq() => Nil // empty sequence
        case Seq(bundle) => bundle // sequence of one
        case bundles => Seq(ThreadBundle(bundles)) // sequence of more than one
      }
    }

    def stepThread(thread: Thread): Seq[Thread] = thread match {
      case singleThread: SingleThread => stepSingleThread(singleThread)
      case threadBundle: ThreadBundle => stepThreadBundle(threadBundle)
    }

    def stepThreads(threads: Seq[Thread]): Seq[Thread] =
        threads.flatMap(stepThread).distinct

    def handleDone(threads: Seq[Thread]): (Seq[Thread], Option[Thread]) = {
      // TODO: Would indexWhere work better?  The test for equality is quite expensive.
      val doneThreadOpt = threads.find(_.isDone)

      // TODO: Keith was here!

      doneThreadOpt match {
        // No thread has finished; return them all.
        case None => (threads, None)
        // A ThreadBundle is done, but is it really done?
        case Some(thread: ThreadBundle) =>
          val survivors = threads.takeWhile(_ != thread)
          if (thread.isReallyDone) (survivors, Some(thread)) // TODO: Mark this last one complete with the debugger.
          else (survivors :+ thread, None) // This sends it to the end of the line.
        // A Thread finished.  Drop all Threads to its right but keep the ones to its left.
        case Some(thread: SingleThread) =>
          val survivors = threads.takeWhile(_ != thread)
          (survivors, Some(thread)) // This last thread finished, the ones after takeWhile failed.  Didn't get there first.
      }
    }

    @annotation.tailrec
    final def innerEvalThreads(threads: Seq[Thread], result: Option[Thread] = None): Option[Thread] = {
      if (threads.isEmpty) result
      else {
        val (nextThreads, nextResult) = handleDone(threads)
        val steppedThreads = stepThreads(nextThreads)

        innerEvalThreads(steppedThreads, nextResult orElse result)
      }
    }

    def evalThreads(threads: Seq[Thread], result: Option[Thread] = None): Option[Thread] = {
      innerEvalThreads(threads, result)
    }

    def eval(tok: Int, start: Inst): Option[Thread] = {
      val threads = mkThreads(tok, start, prevThreadOpt = None)
      val threadOpt = evalThreads(threads)

      threadOpt
    }
  }

  def evaluate(
    start: Inst,
    tok: Int,
    sent: Int,
    doc: Document,
    state: State
  ): Seq[(NamedGroups, NamedMentions)] = {
    val evaluator = Evaluator(start, tok, sent, doc, state)
    val result = evaluator
        .eval(tok, start)
        .map(_.results)
        .getOrElse(Nil)

    result
  }
}

// instruction
sealed trait Inst {
  protected var posId: Int = 0 // These indeed need to be mutable in TokenPattern.assignIds
  def setPosId(newPosId: Int): Unit = posId = newPosId
  def getPosId: Int = posId
  protected var next: Inst = null
  // See deepcopy, ProgramFragment.capture, and ProgramFragment.setOut for the writes.
  def setNext(newNext: Inst): Unit = next = newNext
  def getNext: Inst = next
  def dup(): Inst
  def deepcopy(): Inst = {
    val inst = dup()
    if (next != null) inst.next = next.deepcopy()
    inst
  }
  override def toString(): String = {
    val nextString = Option(next).map(_.toString)

    s"${getClass.getName}: posId = $posId, next = $nextString"
  }

  override def hashCode: Int = posId

  def canEqual(other: Any): Boolean = {
    // Since both are from the same class, we shouldn't need to check if other.canEqual(this).
    this.getClass == other.getClass && this.hashCode == other.hashCode
  }

  override def equals(other: Any): Boolean = {
    other match {
      case that: Inst => this.eq(that) ||
        this.canEqual(that) &&
        shortEquals(that) // &&
        // this.nextOpt == that.nextOpt // Do not go all the way down!
      case _ => false
    }
  }

  def shortEquals(that: Inst): Boolean = this.posId == that.posId
}

// the pattern matched successfully
case object Done extends Inst {
  def dup() = this
}

// no operation
case class Pass() extends Inst {
  def dup() = copy()
}

// split execution
case class Split(lhs: Inst, rhs: Inst) extends Inst {
  def dup() = Split(lhs.deepcopy(), rhs.deepcopy())
  override def hashCode: Int = (lhs, rhs, super.hashCode).##

  override def equals(other: Any): Boolean = {
    other match {
      case that: Split => this.eq(that) ||
        this.canEqual(that) &&
        super.shortEquals(that) &&
        this.lhs == that.lhs &&
        this.rhs == that.rhs
      case _ => false
    }
  }
}

// start capturing tokens
case class SaveStart(name: String, newNext: Inst) extends Inst {
  next = newNext

  def dup() = copy()
  override def hashCode: Int = (name, super.hashCode).##

  override def equals(other: Any): Boolean = {
    other match {
      case that: SaveStart => this.eq(that) ||
        this.canEqual(that) &&
        super.shortEquals(that) &&
        this.name == that.name
      case _ => false
    }
  }
}

// end capturing tokens
case class SaveEnd(name: String) extends Inst {
  def dup() = copy()
  override def hashCode: Int = (name, super.hashCode).##

  override def equals(other: Any): Boolean = {
    other match {
      case that: SaveEnd => this.eq(that) ||
        this.canEqual(that) &&
        super.shortEquals(that) &&
        this.name == that.name
      case _ => false
    }
  }
}

// matches token using token constraint
case class MatchToken(c: TokenConstraint) extends Inst {
  def dup() = copy()
  override def hashCode: Int = (c, super.hashCode).##

  override def equals(other: Any): Boolean = {
    other match {
      case that: MatchToken => this.eq(that) ||
        this.canEqual(that) &&
        super.shortEquals(that) &&
        this.c == that.c
      case _ => false
    }
  }
}

// matches mention by label using string matcher
case class MatchMention(
    m: StringMatcher,
    name: Option[String],
    arg: Option[String]
) extends Inst {
  def dup() = copy()
  override def hashCode: Int = (m, name, arg, super.hashCode).##

  override def equals(other: Any): Boolean = {
    other match {
      case that: MatchMention => this.eq(that) ||
        this.canEqual(that) &&
        super.shortEquals(that) &&
        this.m == that.m &&
        this.name == that.name &&
        this.arg == that.arg
      case _ => false
    }
  }
}

// matches sentence start
case class MatchSentenceStart() extends Inst {
  def dup() = copy()
}

// matches sentence end
case class MatchSentenceEnd() extends Inst {
  def dup() = copy()
}

// zero-width look-ahead assertion
case class MatchLookAhead(start: Inst, negative: Boolean) extends Inst {
  def dup() = MatchLookAhead(start.deepcopy(), negative)
  override def hashCode: Int = (start, negative, super.hashCode).##

  override def equals(other: Any): Boolean = {
    other match {
      case that: MatchLookAhead => this.eq(that) ||
        this.canEqual(that) &&
        super.shortEquals(that) &&
        this.start == that.start &&
        this.negative == that.negative
      case _ => false
    }
  }
}

// zero-width look-behind assertion
case class MatchLookBehind(start: Inst, negative: Boolean) extends Inst {
  def dup() = MatchLookBehind(start.deepcopy(), negative)
  override def hashCode: Int = (start, negative, super.hashCode).##

  override def equals(other: Any): Boolean = {
    other match {
      case that: MatchLookBehind => this.eq(that) ||
        this.canEqual(that) &&
        super.shortEquals(that) &&
        this.start == that.start &&
        this.negative == that.negative
      case _ => false
    }
  }
}
