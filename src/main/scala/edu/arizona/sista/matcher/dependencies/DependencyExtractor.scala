package edu.arizona.sista.matcher.dependencies

import scala.util.matching.Regex
import scala.util.parsing.combinator._
import edu.arizona.sista.matcher.{Extractor, State, TriggerMention}
import edu.arizona.sista.processors.Sentence



sealed trait Values {
  def values(tokens: Seq[Int], strings: Seq[String]): Seq[(Int, String)] =
    tokens map (i => (i, strings(i)))

  def values(tokens: Seq[Int], strings: Option[Array[String]], msg: String): Seq[(Int, String)] =
    strings match {
      case None => sys.error(msg)
      case Some(strings) => values(tokens, strings)
    }
}



sealed trait Dependencies {
  def dependencies(sentence: Sentence) = sentence.dependencies match {
    case None => sys.error("sentence has no dependencies")
    case Some(deps) => deps
  }

  def incomingEdges(sentence: Sentence) = dependencies(sentence).incomingEdges
  def outgoingEdges(sentence: Sentence) = dependencies(sentence).outgoingEdges
}



sealed trait MatcherNode {
  def matches(strings: Seq[(Int, String)]): Seq[Int]
}

class ExactMatcher(dep: String) extends MatcherNode {
  def matches(strings: Seq[(Int, String)]): Seq[Int] =
    strings filter (_._2 == dep) map (_._1)
}

class RegexMatcher(rx: Regex) extends MatcherNode {
  def matches(strings: Seq[(Int, String)]): Seq[Int] =
    strings filter (e => rx.findFirstIn(e._2).nonEmpty) map (_._1)
}



sealed trait ExtractorNode {
  def findAllIn(sentence: Sentence, state: State, start: Int): Seq[Int]
}

class OutgoingExtractor(matcher: MatcherNode)
extends ExtractorNode with Dependencies {
  def findAllIn(sentence: Sentence, state: State, start: Int): Seq[Int] = {
    val edges = outgoingEdges(sentence)
    if (edges isDefinedAt start) matcher.matches(edges(start)) else Nil
  }
}

class IncomingExtractor(matcher: MatcherNode)
extends ExtractorNode with Dependencies {
  def findAllIn(sentence: Sentence, state: State, start: Int): Seq[Int] = {
    val edges = incomingEdges(sentence)
    if (edges isDefinedAt start) matcher.matches(edges(start)) else Nil
  }
}

class ConcatExtractor(lhs: ExtractorNode, rhs: ExtractorNode)
extends ExtractorNode {
  def findAllIn(sentence: Sentence, state: State, start: Int): Seq[Int] =
    (lhs.findAllIn(sentence, state, start) flatMap (i => rhs.findAllIn(sentence, state, i))).distinct
}

class OrExtractor(lhs: ExtractorNode, rhs: ExtractorNode)
extends ExtractorNode {
  def findAllIn(sentence: Sentence, state: State, start: Int): Seq[Int] =
    (lhs.findAllIn(sentence, state, start) ++ rhs.findAllIn(sentence, state, start)).distinct
}

class FilteredExtractor(extractor: ExtractorNode, filter: FilterNode)
extends ExtractorNode {
  def findAllIn(sentence: Sentence, state: State, start: Int): Seq[Int] =
    filter.filter(sentence, state, extractor.findAllIn(sentence, state, start))
}

class OptionalExtractor(extractor: ExtractorNode)
extends ExtractorNode {
  def findAllIn(sentence: Sentence, state: State, start: Int): Seq[Int] =
    (start +: extractor.findAllIn(sentence, state, start)).distinct
}

class KleeneExtractor(extractor: ExtractorNode)
extends ExtractorNode {
  def findAllIn(sentence: Sentence, state: State, start: Int): Seq[Int] = {
    def collect(results: Set[Int], remaining: Seq[Int]): Seq[Int] = remaining match {
      case Nil => results.toSeq
      case t :: ts if results contains t => collect(results, ts)
      case t :: ts => collect(results + t, ts ++ extractor.findAllIn(sentence, state, t))
    }
    collect(Set.empty, Seq(start))
  }
}

class ArgumentExtractor(val name: String, val required: Boolean, extractor: ExtractorNode)
extends ExtractorNode {
  def findAllIn(sentence: Sentence, state: State, start: Int): Seq[Int] =
    extractor.findAllIn(sentence, state, start)
}



sealed trait FilterNode {
  def filter(sentence: Sentence, state: State, tokens: Seq[Int]): Seq[Int]
}

class WordFilter(matcher: MatcherNode) extends FilterNode with Values {
  def filter(sentence: Sentence, state: State, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.words)
}

class LemmaFilter(matcher: MatcherNode) extends FilterNode with Values {
  def filter(sentence: Sentence, state: State, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.lemmas, "sentence has no lemmas")
}

class TagFilter(matcher: MatcherNode) extends FilterNode with Values {
  def filter(sentence: Sentence, state: State, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.tags, "sentence has no tags")
}

class EntityFilter(matcher: MatcherNode) extends FilterNode with Values {
  def filter(sentence: Sentence, state: State, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.entities, "sentence has no entities")
}

class ChunkFilter(matcher: MatcherNode) extends FilterNode with Values {
  def filter(sentence: Sentence, state: State, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.chunks, "sentence has no chunks")
}

class MentionFilter(matcher: MatcherNode) extends FilterNode {
  def filter(sentence: Sentence, state: State, tokens: Seq[Int]): Seq[Int] = {
    val s = state.sentenceIndex(sentence)
    tokens filter { t =>
      state.mentionsFor(s, t) exists { m =>
        val indexsAndValues = m.allLabels.zipWithIndex map (li => (li._2, li._1))
        matcher.matches(indexsAndValues).nonEmpty
      }
    }
  }
}

class IncomingFilter(matcher: MatcherNode) extends FilterNode with Dependencies {
  def filter(sentence: Sentence, state: State, tokens: Seq[Int]): Seq[Int] = {
    val edges = incomingEdges(sentence)
    tokens filter (tok => (edges.isDefinedAt(tok) && matcher.matches(edges(tok)).nonEmpty))
  }
}

class OutgoingFilter(matcher: MatcherNode) extends FilterNode with Dependencies {
  def filter(sentence: Sentence, state: State, tokens: Seq[Int]): Seq[Int] = {
    val edges = outgoingEdges(sentence)
    tokens filter (tok => (edges.isDefinedAt(tok) && matcher.matches(edges(tok)).nonEmpty))
  }
}

class AndFilter(lhs: FilterNode, rhs: FilterNode) extends FilterNode {
  def filter(sentence: Sentence, state: State, tokens: Seq[Int]): Seq[Int] =
    lhs.filter(sentence, state, tokens) intersect rhs.filter(sentence, state, tokens)
}

class OrFilter(lhs: FilterNode, rhs: FilterNode) extends FilterNode {
  def filter(sentence: Sentence, state: State, tokens: Seq[Int]): Seq[Int] =
    (lhs.filter(sentence, state, tokens) ++ rhs.filter(sentence, state, tokens)).distinct
}

class NotFilter(filter: FilterNode) extends FilterNode {
  def filter(sentence: Sentence, state: State, tokens: Seq[Int]): Seq[Int] =
    tokens diff filter.filter(sentence, state, tokens)
}



class TriggerFinder(filter: FilterNode) {
  def findAllIn(sentence: Sentence, state: State, ruleName: String): Seq[Int] = {
    val s = state.sentenceIndex(sentence)
    filter.filter(sentence, state, 0 until sentence.size) filter {
      t => state.mentionsForRule(s, t, ruleName).isEmpty
    }
  }
}



class DependencyExtractor(trigger: TriggerFinder, arguments: Seq[ArgumentExtractor])
extends Extractor {
  private val required = arguments filter (_.required == true)
  private val optional = arguments filter (_.required == false)

  def findAllIn(sentence: Sentence, state: State, ruleName: String): Seq[Map[String, Seq[Int]]] =
    trigger.findAllIn(sentence, state, ruleName) flatMap (t => extractArgs(sentence, state, t))

  private def extractArgs(sent: Sentence, state: State, tok: Int) = {
    val req = extract(required, sent, state, tok)
    if (req.exists(_._2.isEmpty)) None
    else Some(req ++ extract(optional, sent, state, tok) + ("trigger" -> Seq(tok)))
  }

  private def extract(args: Seq[ArgumentExtractor], sent: Sentence, state: State, i: Int) =
    args.map(a => (a.name -> a.findAllIn(sent, state, i))).toMap
}

object DependencyExtractor {
  def apply(input: String): DependencyExtractor = Parser.parse(input)
}



object Parser extends RegexParsers {

  // remove comment lines and trim text
  def cleanInput(input:String): String = input.replaceAll("""(?m)^\s*#.*$""", "").trim

  def parse(input: String): DependencyExtractor = parseAll(depExtractor, cleanInput(input)) match {
    case Success(result, _) => result
    case failure: NoSuccess => sys.error(failure.msg)
  }

  override val whiteSpace = """[ \t]+""".r
  val eol = "\n"

  def ident: Parser[String] =
    """\p{javaJavaIdentifierStart}\p{javaJavaIdentifierPart}*""".r

  // single- or double-quote delimited string literal
  def stringLiteral: Parser[String] =
    """"[^\\"]*(?:\\.[^\\"]*)*"|'[^\\']*(?:\\.[^\\']*)*'""".r ^^ {
      case s => """\\(.)""".r.replaceAllIn(s.drop(1).dropRight(1), m => m.group(1))
    }

  def exactLiteral: Parser[String] = ident | stringLiteral

  // match a perl style "/" delimited regular expression
  // "\" is the escape character, so "\/" becomes "/"
  def regexLiteral: Parser[Regex] = """/[^\\/]*(?:\\.[^\\/]*)*/""".r ^^ {
    case s => s.drop(1).dropRight(1).replaceAll("""\\/""", "/").r
  }

  def exactMatcher: Parser[MatcherNode] = exactLiteral ^^ {
    case string => new ExactMatcher(string)
  }

  def regexMatcher: Parser[MatcherNode] = regexLiteral ^^ {
    case regex => new RegexMatcher(regex)
  }

  def stringMatcher: Parser[MatcherNode] = exactMatcher | regexMatcher

  def outgoingExtractor: Parser[ExtractorNode] = opt(">") ~> stringMatcher ^^ {
    case matcher => new OutgoingExtractor(matcher)
  }

  def incomingExtractor: Parser[ExtractorNode] = "<" ~> stringMatcher ^^ {
    case matcher => new IncomingExtractor(matcher)
  }

  def atomExtractor: Parser[ExtractorNode] =
    outgoingExtractor | incomingExtractor | "(" ~> orExtractor <~ ")"

  def quantifiedExtractor: Parser[ExtractorNode] = atomExtractor ~ opt("?"|"*"|"+") ^^ {
    case extractor ~ None => extractor
    case extractor ~ Some("?") => new OptionalExtractor(extractor)
    case extractor ~ Some("*") => new KleeneExtractor(extractor)
    case extractor ~ Some("+") => new ConcatExtractor(extractor, new KleeneExtractor(extractor))
  }

  def filteredExtractor: Parser[ExtractorNode] = quantifiedExtractor ~ opt(tokenFilter) ^^ {
    case extractor ~ None => extractor
    case extractor ~ Some(filter) => new FilteredExtractor(extractor, filter)
  }

  def concatExtractor: Parser[ExtractorNode] = filteredExtractor ~ rep(filteredExtractor) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) => new ConcatExtractor(lhs, rhs)
    }
  }

  def orExtractor: Parser[ExtractorNode] = concatExtractor ~ rep("|" ~> concatExtractor) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) => new OrExtractor(lhs, rhs)
    }
  }

  def filterValue: Parser[FilterNode] = ident ~ "=" ~ stringMatcher ^^ {
    case "word" ~ _ ~ matcher => new WordFilter(matcher)
    case "lemma" ~ _ ~ matcher => new LemmaFilter(matcher)
    case "tag" ~ _ ~ matcher => new TagFilter(matcher)
    case "entity" ~ _ ~ matcher => new EntityFilter(matcher)
    case "chunk" ~ _ ~ matcher => new ChunkFilter(matcher)
    case "mention" ~ _ ~ matcher => new MentionFilter(matcher)
    case "incoming" ~ _ ~ matcher => new IncomingFilter(matcher)
    case "outgoing" ~ _ ~ matcher => new OutgoingFilter(matcher)
    case _ => sys.error("unrecognized filter")
  }

  def filterAtom: Parser[FilterNode] = filterValue | "(" ~> orFilter <~ ")"

  def notFilter: Parser[FilterNode] = opt("!") ~ filterAtom ^^ {
    case None ~ filter => filter
    case Some(_) ~ filter => new NotFilter(filter)
  }

  def andFilter: Parser[FilterNode] = notFilter ~ rep("&" ~> notFilter) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) => new AndFilter(lhs, rhs)
    }
  }

  def orFilter: Parser[FilterNode] = andFilter ~ rep("|" ~> andFilter) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) => new OrFilter(lhs, rhs)
    }
  }

  def tokenFilter: Parser[FilterNode] = "[" ~> orFilter <~ "]"

  def comment: Parser[String] = "#.*".r

  def triggerFinder: Parser[TriggerFinder] = "trigger" ~> ":" ~> tokenFilter <~ opt(comment) ^^ {
    case filter => new TriggerFinder(filter)
  }

  def argExtractor: Parser[ArgumentExtractor] = ident ~ opt("?") ~ ":" ~ orExtractor <~ opt(comment) ^^ {
    case "trigger" ~ _ ~ _ ~ _ => sys.error("`trigger` is not a valid argument name")
    case name ~ None ~ _ ~ extractor => new ArgumentExtractor(name, true, extractor)
    case name ~ Some(_) ~ _ ~ extractor => new ArgumentExtractor(name, false, extractor)
  }

  def depExtractor: Parser[DependencyExtractor] =
    triggerFinder ~ rep1(eol) ~ repsep(argExtractor, rep1(eol)) ^^ {
      case trigger ~ _ ~ arguments  => new DependencyExtractor(trigger, arguments)
    }
}
