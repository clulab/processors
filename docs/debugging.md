---
title: Debugging
parent: Odin
has_children: false
nav_order: 1
---

# Debugging

## Introduction

Odin rules are great when they work as expected, but when the unexpected happens, it can be difficult to sort out what has gone wrong.  This can occur when rules encounter text with unexpected vocabulary or syntax, when the tokenizer or parser makes a mistake resulting in non-word tokens or part-of-speech errors, when your rule doesn't really say what you think it does, when rules start to interact in strange ways, etc.  These are times when a debugger can come in handy.

The most familiar and powerful debugger at your disposal is probably the one built into your IDE (Integrated Development Environment) like IntelliJ or Eclipse.  These can be very helpful and might even be the tool of choice.  They require no additional programming and tell you almost everything there is to know about a running program.  However, it can often be more than you want to know, so much in fact that it is difficult to filter through all the information to find what you need.  The information is also fleeting, changing with every newly executed line of code.

Alternatives to the built-in debugger include "printf" debugging where one inserts code at strategic places to record what is happening.  The information collected is then more pertinent and persistent.  This is basically the strategy that the Odin debugger takes.  Critical parts of rule processing code have been "instrumented" with virtual printfs in advance.  They not only output text, but can also output HTML to produce reports that provide insight into the workings of the rules.  The debugger also keeps track of context so that it can printf about issues not obvious from a single line of code. 

Other similar debugging possibilities include the processors [webapp](https://processors.clulab.org), which will generate an HTML visualization of sentences and mentions with the currently preinstalled grammars and named entities or can be rebuilt locally to support your own.  There is also the command line interface (CLI) provided by [CommandLineInterface](https://github.com/clulab/processors/blob/master/apps/src/main/scala/org/clulab/processors/apps/CommandLineInterface.scala) that will processes sentences and show the results.  The Odin debugger borrows presentations from these other tools so that you will hopefully not need to use more than one.

Unlike the webapp and CLI program, the debugger is not a stand-alone application and needs to be integrated into a programming project and activated (similar to printf debugging).  Afterwards, the output requires some interpretation and familiarity with Odin concepts and terminology.  Nevertheless, it should be easier to deal with than the IDE's debugger or printf.  The very bottom of this document includes sample [output](Debugging#Output) so that you have a preview before installing.

## Installation

The [debugger](https://github.com/clulab/processors/tree/master/debugger) is implemented as a relatively independent subproject of processors.  It is published separately from other processors projects and needs its own line in `build.sbt` or the maven equivalent.

`libraryDependencies += "org.clulab" %% "processors-debugger" % "x.x.x"`

The version number should match whatever you are using for processors.  Changes to Odin's rule processing code will in many cases need to be tracked in the debugger code and it is surely a mistake to mix the version numbers.

## Usage

The debugger is designed in a somewhat aspect-oriented way, attempting to add behavior to Odin without affecting the existing code.  Regular Odin usage involves creating an [ExtractorEngine](TODO) with some rules.  In order to debug, one needs to take that extractor engine and use it to construct a [DebuggingExtractorEngine](TODO), which will take the original one and duplicate it with debugging versions of applicable parts.  With that debugging extractor engine, one should have the program do something very similar to what it does to the original extractor engine.  While that is being done, the debugging version will collect information about program execution that can be used to generate reports (the printfs).  For example, if the original program says

```scala
import org.clulab.odin.ExtractorEngine
import org.clulab.processors.clu.CluProcessor
...

  val sentence = "..."
  val processor = new CluProcessor()
  val document = processor.annotate(sentence)

  val rules = "..."
  val extractorEngine = ExtractorEngine(rules)
  val mentions = extractorEngine.extractFrom(document)
```

then the debugging version would need the following lines either added

```scala
import org.clulab.odin.debugger.odin.DebuggingExtractorEngine
...

  val debuggingExtractorEngine = DebuggingExtractorEngine(extractorEngine)
  val debuggingMentions = debuggingExtractorEngine.extractFrom(document)
```

or replaced with some of the others like this:

```scala
import org.clulab.odin.ExtractorEngine
import org.clulab.odin.debugger.odin.DebuggingExtractorEngine
import org.clulab.processors.clu.CluProcessor
...

  val sentence = "..."
  val processor = new CluProcessor()
  val document = processor.annotate(sentence)

  val rules = "..."
  val extractorEngine = DebuggingExtractorEngine(ExtractorEngine(rules))
  val mentions = extractorEngine.extractFrom(document)
```

There are examples of this strategy in several source files including

* main/apps
  * [DebuggingOdinStarterApp](TODO)
* test/extractor
  * [DebugTokenExtractor](TODO)
  * [DebugCrossSentenceExtractor](TODO)
  * graph
    * [DebugTriggerPatternGraphExtractor](TODO)
    * [DebugTriggerMentionGraphExtractor](TODO)
    * [DebugRelationGraphExtractor](TODO)

Instructions above have described the debugger with little d.  There is also a [Debugger](TODO) with big D.  The DebuggingExtractorEngine and other [debugging](TODO) classes talk to it behind the scenes. In order to find out what information the Debugger has collected, one makes use of an [Inspector](TODO), the second major component of the debugger:

* debugger
  * [Debugger](TODO)
  * [Inspector](TODO)

The Inspector is created with the help of the DebuggingExtractorEngine and can thereafter be used to inspect the operations that the Debugger has noted:

```scala
import org.clulab.odin.debugger.Inspector
...

  val debuggingExtractorEngine = ...

  Inspector(debuggingExtractorEngine)
      .inspectStaticAsHtml("../debug-static.html")
      .inspectDynamicAsHtml("../debug-dynamic.html")
```
## Views

As hinted in the code above, there are two basic kinds of information collected by the debugger: static and dynamic.  The static deals with Odin rules without them yet being applied to any particular sentence.  Rule parsers and compilers have converted the rule texts into static data structures to be used later (dynamically).  The rules have not been applied or "executed", but the data structures can be inspected.  Three different static views are available in the HTML output and there are some plain text options as well:

* Static views
  * HTML
    * [Rule View](TODO) - shows in a simple table format a visualization of rules that otherwise usually appear in yaml format.  Studying this view may clear up problems related to indentation, special characters, or formatting of the yaml file and explain unexpected behavior.
    * [Textual Extractor View](TODO) - shows the data structures of parsed and compiled rules which have been converted into [Extractors](TODO) of several types.  With this information, a somewhat experienced rule writer can verify that a rule is properly understood by Odin and not somehow misunderstood because of an issue with Odin syntax, a typo, a misnamed identifier, etc.  Extractor types are
      * [TokenExtractor](TODO) for "token" rules,
      * [GraphExtractor](TODO) for "dependency" rules, and
      * [CrossSentenceExtractor](TODO) for "cross-sentence" rules.
    * [Graphical Extractor View](TODO) - turns some of the internal structures of Extractors into a directed graph showing possible states that something called a [Thread](TODO) can be in as an extractor eventually executes.  Key things to notice here are loops and branches related to `*`, `+`, and `|` operations in [TokenPatterns](TODO).  For rules to match, the outermost [Done](TODO) [Inst](TODO) needs to be reached.  [Mermaid](https://mermaid.js.org/) is used to produce the graphic and access to its content delivery network needs to be available when the HTML output is observed in the browser. 
  * Text
    * [Rule View](TODO) - offers a version of the HTML Rule View that can simply be printed to the console and observed by a human without assistance of a web browser.
    * [Extractor View](TODO) - as with the HTML version, shows the data structure, but this time in a text-only format.

Next there is the dynamic information that gets collected as extractors process rules against particular sentences, resulting in matches that are turned into [Mentions](TODO).  In this case the sentences themselves also need to be diagramed so that one can understand what the Extractors are matching against.  The different kinds of Extractors match in different ways and in different steps, so there are several views involved:

* Dynamic views
  * HTML
    * [Global Action View](TODO) - is usually located at the top of an HTML report because it summarizes the Mentions found across an entire document by all the rules and one can check here first before looking into more details.  On the left are listed the Mentions that enter the global action and on the right are those that exit.  Mentions that are either filtered out or transformed into different mentions (they are compared with `eq()` rather than `equals()` or `==`, so that means reference comparison) appear only on the left.  New Mentions appear only on the right.  If your rules are actually working, but something is wrong with the action, clues can be found here.
    * [Parse View](TODO) - applies to sentences and resembles the [webapp]() output.  This shows the tokenization, part-of-speech tags, named entities, dependencies, etc. of sentences that are being processed.  The parse for each sentence will be followed by views for all the Extractors being applied to the sentence.  It is possible for the parse to be wrong and that to be the cause of unexpected rule behavior.
    * [Inst View](TODO) - shows how [Insts](TODO) match at consecutive sentence tokens.  [TokenPatterns](TODO) are composed of a graph of Insts which either invoke some comparison with the sentence (e.g., [MatchToken](TODO), [MatchSentenceStart](TODO)) or regulate [Threads]() that the [ThompsonVM](TODO) uses to negotiate possible rule matches (e.g., [Split](TODO), [MatchLookAhead](TODO)).  Insts are numbered as shown in the Extractor View.  If the Inst matches each time at a particular token, its number is displayed in green.  If it always fails at the token, its number is displayed in red.  If it sometimes passes and sometimes fails, the display is gray.  A green 0 indicates that [Done](TODO) matches, that the TokenPattern is complete, and that a rule matched the tokens to the left of the zero.  A careful study of the numbers and colors can explain unexpected behavior.
    * [Thread View](TODO) - organizes sequences of Insts that have formed at least a portion of a match of a rule across tokens.  They usually start with a couple of green Inst numbers and then end with a red number because of a mismatch or a green 0 indicating a complete match.  Complete matches are not always used because we also want the longest complete match starting at any token.  Used Threads are marked with a green &#9745; and unused with a red &#9746;.  A reason ([ThreadMatch]()) is always given for the outcome and it may be useful to track down where these are assigned in the code:
      * &#9746; Inst mismatch
      * &#9746; Thread empty
      * &#9746; Thread not the best
      * &#9746; Thread superseded
      * &#9745; Thread survived
    * [Mention View](TODO) - for some kinds of Extractors, details the comparisons made between a reference [Mention](TODO) and Mentions in the [State](TODO).  The former is a model of something being looked for and the latter are the possible matches, everything being a Mention.  This view is pertinent to the 
      * [CrossSentenceExtractor](TODO) for a [CrossSentenceRule](TODO) indicated with the type "cross-sentence",
      * [GraphExtractor](TODO) for either a [TriggerMentionGraphPattern](TODO) or a [RelationGraphPattern](TODO) that is part of a [GraphRule](TODO) indicated with the type "dependency".

      As with the Thread View, a reason for the match or mismatch is always given in the form of a [MentionMatch](TODO) and strange values can be clues:
      * &#9746; State mismatch
      * &#9746; Interval mismatch
      * &#9746; Label mismatch
      * &#9746; Type mismatch
      * &#9745; Mention match
    * [Local Action View](TODO) - is the same as the Global Action View, but pertains not to the entire document, but just a sentence and the Mentions found by a particular Extractor.

There is one additional view which should be classified as dynamic but does not act the same as the others.  It does not appear in the HTML output.  In fact, the values it tracks are not stored because they would be too many.  The [Stack View](TODO) is printed to `System.out` directly by the Debugger during the execution of the DebuggingExtractorEngine if its `verbose` has been set to `true`.  It shows the pertinent stack frames that the Odin algorithm uses while searching for text that matches the rules.  This visualizes the looping through of Documents, rule priorities, Sentences, Extractors, starting tokens, Insts, etc.  The output is formatted so that links appear in the IntelliJ Debug tab and you can click on them to go directly to the code and, for example, set a breakpoint to catch unexpected behavior in the act:

```
beg loop org.clulab.odin.debugger.odin.InnerDebuggingExtractorEngine#extract(DebuggingExtractorEngine.scala:17)[](loop = 1)
    beg extractor org.clulab.odin.debugger.odin.DebuggingTokenExtractor#findAllIn(DebuggingTokenExtractor.scala:23)["four-As"]()
        beg tokenPattern org.clulab.odin.debugger.odin.DebuggingTokenExtractor#findAllIn(DebuggingTokenExtractor.scala:24)["tokenPattern"]()
            beg sentence org.clulab.odin.debugger.odin.DebuggingTokenExtractor#findAllIn(DebuggingTokenExtractor.scala:29)[](index = 0, sentence = "John Doe eats cake .")
                beg start org.clulab.odin.debugger.odin.DebuggingTokenPattern#findFirstIn loop r(DebuggingTokenPattern.scala:26)[](start = 0)
                    beg tok org.clulab.odin.debugger.odin.DebuggingThompsonVM.DebuggingEvaluator#mkThreads(DebuggingThompsonVM.scala:56)[](tok = 0)
                        instMatches org.clulab.odin.debugger.odin.DebuggingThompsonVM.DebuggingEvaluator#mkThreads loop(DebuggingThompsonVM.scala:78)[](matches = true, ...)
                    end tok org.clulab.odin.debugger.odin.DebuggingThompsonVM.DebuggingEvaluator#mkThreads(DebuggingThompsonVM.scala:56)[](tok = 0)
                    beg tok org.clulab.odin.debugger.odin.DebuggingThompsonVM.DebuggingEvaluator#stepSingleThread(DebuggingThompsonVM.scala:114)[](tok = 0)
                        ...
```

## Filters

It works best to use the debugger with only a small number of rules and sentences.  However, it may be difficult to manipulate an existing, complex program into one that runs on just a small subset of rules and sentences.  Also, one might not know in advance where to look and need to spread a wide net.  In these situations, [Filters](TODO) might help.  There are four kinds:

| | Static | Dynamic |
| --- | --- | --- |
| Debugger | [StaticDebuggerFilter](TODO) | [DynamicDebuggerFilter](TODO) |
| Inspector | [StaticInspectorFilter](TODO) | [DynamicInspectorFilter](TODO) |

The static and dynamic match the idea from the views above with static having to do with Rules and Extractors before they are applied to sentences and dynamic while they are.  The Debugger works dynamically and simply passes any StaticDebuggerFilter it knows about to the Inspector when it gets that far.  Filters can be created by subclassing from the classes in the table above or by having a filter generated by their companion objects, for example, like this:
```scala
import org.clulab.odin.debugger.debug.filter.{DynamicDebuggerFilter, StaticDebuggerFilter}
import org.clulab.odin.debugger.odin.DebuggingExtractorEngine
...

  val extractorEngine = ...
  val extractor = ...
  val sentence = "..."
  
  val dynamicDebuggerFilter = DynamicDebuggerFilter.extractorFilter(extractor).sentenceFilter(sentence)
  val staticDebuggerFilter = StaticDebuggerFilter.extractorFilter(extractor)

  val debuggingExtractorEngine = DebuggingExtractorEngine(extractorEngine,
      dynamicDebuggerFilter = dynamicDebuggerFilter, staticDebuggerFilter = staticDebuggerFilter,
      active = true, verbose = true)

```

In this example, the Debugger will only collect information related to the particular extractor and sentence in its [Transcripts](TODO).  When the Inspector gets created, the DebuggingExtractorEngine will also, in this case redundantly, tell the Inspector to only concern itself with information about the one extractor as it takes over the Transcripts.  Filters can be applied at other times.  If the `dynamicDebuggerFilter` is not added to the `debuggingExtractorEngine` in advance, the filtering can take place after the fact, at the Inspector, by calling `inspector.filter(dynamicDebuggerFilter)`.  There is a similar `inspector.filter(staticDebuggerFilter)` allowed, both returning new Inspectors.

The Inspector filters tell the Inspector which kinds of views to show.  StaticInspectorFilters control the static views and DynamicInspectorFilters include both the static and dynamic ones (so that the two kinds of views can be mixed and matched in a single HTML page).  If you don't care about local actions, for instance, all of those views can be suppressed.  These filters are added to the calls to `inspectStaticAsHtm()` and `inspectDynamicAsHtm()`.  Here are examples:

```scala
import org.clulab.odin.debugger.Inspector
import org.clulab.odin.debugger.debug.Transcript
import org.clulab.odin.debugger.debug.filter.DynamicInspectorFilter
import org.clulab.odin.debugger.debug.finished.FinishedLocalAction
import org.clulab.odin.impl.Extractor
import org.clulab.processors.Sentence
...

  val debuggingExtractorEngine = ...
  val verboseFilter = DynamicInspectorFilter.verbose
  val noLocalActionFilter = new DynamicInspectorFilter {
    def showLocalActionView(extractor: Extractor, sentence: Sentence, transcript: Transcript[FinishedLocalAction]): Boolean = false
  }

  Inspector(debuggingExtractorEngine)
      .inspectDynamicAsHtml("../debug-verbose-dynamic.html", filter = verboseFilter)
      .inspectDynamicAsHtml("../debug-localactionless-dynamic.html", filter = noLocalActionFilter)
```
 
 Finally, if too much information is displayed in the HTML output, one can also click on the gray headers to collapse sections of the output in order to highlight the remaining, relevant information.

## Verbosity

As mentioned earlier, display of the Stack View can be turned on and off by controlling the verbose argument to the constructor of the DebuggingExtractorEngine.  There are also filters
  * [StaticInspectorFilter.verbose](TODO) - shows all static views, so the Rule, Textual, and Graphical Views.  It can be used directly or be activated when `verbose = true` in the call to `inspectStaticAsHtml()`.
  * [StaticInspectorFilter.concise](TODO) - skips the Graphical View and is used when `verbose = false` in the call to `inspectStaticAsHtml()`
  * [DynamicInspectorFilter.verbose](TODO) - shows all static and dynamic views.  It can be used explicitly in the call to `inspectDynamicAsHtml()` or implicitly by setting `verbose = true`.
  * [DynamicInspectorFilter.concise](TODO) - skips the static views in the dynamic output and is used if `verbose = false` in the call.

## Context

As the debugger runs, it keeps track of what Odin is working on, like an instance of Sentence, Extractor, or Pattern, in a [Context](TODO).  Presently, the context is separated into lists/stacks differentiated by the instance type.  Some ordering information is lost with this scheme and an alternative design would store the instances in a single, mixed stack that would be more accurate but more complex to query.  If the Odin algorithm changes, or the ordering of the Context becomes important for some other reason, an updated design might be warranted.

## Output

Here are examples of both static and dynamic output in HTML format:
  * Static
    ![debug-static.png](./images/debug-static.png?raw=True")
  * Dynamic
    ![debug-dynamic.png](./images/debug-dynamic.png?raw=True")
