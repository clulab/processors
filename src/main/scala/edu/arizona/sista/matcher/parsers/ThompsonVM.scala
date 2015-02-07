package edu.arizona.sista.matcher

import edu.arizona.sista.processors.Document

object ThompsonVM {
  type Sub = Map[String, (Int, Int)]

  private case class Thread(tok: Int, inst: Inst) {
    var sub: Sub = _
  }

  private object Thread {
    def apply(tok: Int, inst: Inst, sub: Sub): Thread = {
      val t = Thread(tok, inst)
      t.sub = sub
      t
    }
  }

  def evaluate(start: Inst, tok: Int, sent: Int, doc: Document, state: Option[State]): Option[Sub] = {
    def mkThreads(tok: Int, inst: Inst, sub: Sub): Seq[Thread] = inst match {
      case i: Jump => mkThreads(tok, i.next, sub)
      case i: Split => mkThreads(tok, i.lhs, sub) ++ mkThreads(tok, i.rhs, sub)
      case i: SaveStart => mkThreads(tok, i.next, sub + (i.name -> (tok, -1)))
      case i: SaveEnd => mkThreads(tok, i.next, sub + (i.name -> (sub(i.name)._1, tok)))
      case _ => Seq(Thread(tok, inst, sub))
    }

    def stepThreads(threads: Seq[Thread]): Seq[Thread] =
      threads.flatMap(t => t.inst match {
        case i: MatchToken if t.tok < doc.sentences(sent).size && i.c.matches(t.tok, sent, doc, state) =>
          mkThreads(t.tok + 1, i.next, t.sub)  // token matched, return new threads
        case i: MatchSentenceStart if t.tok == 0 =>
          mkThreads(t.tok, i.next, t.sub)
        case i: MatchSentenceEnd if t.tok == doc.sentences(sent).size =>
          mkThreads(t.tok, i.next, t.sub)
        case i: MatchMention => state match {
          case None => Nil  // should we throw an exception or fail silently?
          case Some(s) => for {
            mention <- s.mentionsFor(sent, t.tok)
            if mention.start == t.tok && i.m.matches(mention.label)
            thread <- mkThreads(mention.end, i.next, t.sub)
          } yield thread
        }
        case _ => Nil  // thread died with no match
      }).distinct

    def handleDone(threads: Seq[Thread]): (Seq[Thread], Option[Sub]) =
      threads find (_.inst == Done) match {
        // no thread has finished, return them all
        case None => (threads, None)
        // a thread finished, drop all threads to its right but keep the ones to its left
        case Some(t) => (threads.takeWhile(_ != t), Some(t.sub))
      }

    @annotation.tailrec
    def loop(threads: Seq[Thread], result: Option[Sub]): Option[Sub] = {
      if (threads.isEmpty) result
      else {
        val (ts, r) = handleDone(threads)
        loop(stepThreads(ts), r)
      }
    }

    loop(mkThreads(tok, start, Map.empty), None)
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
  def dup: Inst = {
    val inst = copy()
    if (next != null) inst.next = next.dup
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
