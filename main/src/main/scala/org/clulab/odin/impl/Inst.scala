package org.clulab.odin.impl

// instruction
sealed trait Inst {
  var posId: Int = 0 // These indeed need to be mutable in TokenPattern.assignIds
  var nextOpt: Option[Inst] = None // See deepcopy for the write.
  def dup(): Inst
  def deepcopy(): Inst = {
    val inst = dup()
    inst.nextOpt = nextOpt.map(_.deepcopy())
    inst
  }
  override def toString(): String = {
    val nextString = Option(nextOpt).map(_.toString)

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
        this.posId == that.posId &&
        this.nextOpt == that.nextOpt // TODO. Go all the way down?
      case _ => false
    }
  }
}

// the pattern matched succesfully
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
  override def hashCode: Int = {
    val tuple = (lhs, rhs, super.hashCode)
    val hash = tuple.##

    hash
  }

  override def equals(other: Any): Boolean = {
    other match {
      case that: Split => this.eq(that) ||
        this.canEqual(that) &&
        this.lhs == that.lhs &&
        this.rhs == that.rhs &&
        super.equals(that) // Save the recursion for the end.
      case _ => false
    }
  }
}

// start capturing tokens
case class SaveStart(name: String) extends Inst {
  def dup() = copy()
  override def hashCode: Int = (name, super.hashCode).##

  override def equals(other: Any): Boolean = {
    other match {
      case that: SaveStart => this.eq(that) ||
        this.canEqual(that) &&
        this.name == that.name &&
        super.equals(that)
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
        this.name == that.name &&
        super.equals(that)
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
        this.c == that.c &&
        super.equals(that)
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
        this.m == that.m &&
        this.name == that.name &&
        this.arg == that.arg &&
        super.equals(that)
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
    if (posId != 0)
      println("I guess this happens")
    other match {
      case that: MatchLookAhead => this.eq(that) ||
        this.canEqual(that) &&
        this.start == that.start &&
        this.negative == that.negative &&
        super.equals(that)
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
        this.start == that.start &&
        this.negative == that.negative &&
        super.equals(that)
      case _ => false
    }
  }
}
