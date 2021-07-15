package org.clulab.processors

import java.io.PrintWriter
import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.fastnlp.FastNLPProcessorWithSemanticRoles
import org.clulab.utils.CliReader
import org.clulab.utils.DefaultMenuItem
import org.clulab.utils.ExitMenuItem
import org.clulab.utils.HelpMenuItem
import org.clulab.utils.MainMenuItem
import org.clulab.utils.Menu

/**
  * A simple interactive shell
  * User: mihais
  * Date: 3/13/14
  * Last Modified: Fix compiler warning: remove redundant match case clause.
 */
object ProcessorShell extends App {
  lazy val core: Processor = new CoreNLPProcessor() // this uses the slower constituent parser
  lazy val fast: Processor = new FastNLPProcessorWithSemanticRoles() // this uses the faster dependency parser
  lazy val clu: Processor = new CluProcessor()

  Utils.initializeDyNet()

  var proc = clu // The initial proc does not get initialized.
  val cluCorePrompt = "(clu)>>> "

  val lineReader = new CliReader(cluCorePrompt, "user.home", ".processorshellhistory")
  val printWriter = new PrintWriter(System.out)

  def prepareProcessor(prompt: String, message: String, processor: Processor): Boolean = {
    lineReader.setPrompt(prompt)
    println(message)
    proc = processor
    proc.annotate("initialize me!")
    true
  }

  def prepareCore(menu: Menu, text: String): Boolean =
    prepareProcessor("(core)>>>", "Preparing CoreNLPProcessor...", core)

  def prepareFast(menu: Menu, text: String): Boolean =
    prepareProcessor("(fast)>>> ", "Preparing FastNLPProcessor...", fast)

  def prepareClu(menu: Menu, text: String): Boolean = {
    prepareProcessor("(clu)>>> ", "Preparing CluProcessor...", clu)
  }

  def annotate(menu: Menu, text: String): Boolean = {
    val doc = proc.annotate(text)
    doc.prettyPrint(printWriter)
    printWriter.flush()
    true
  }

  val mainMenuItems = Seq(
    new HelpMenuItem(":help", "show commands"),
    new MainMenuItem(":core", "use CoreNLPProcessor", prepareCore),
    new MainMenuItem(":fast", "use FastNLPProcessor", prepareFast),
    new MainMenuItem(":clu", "use CluProcessor", prepareClu),
    new ExitMenuItem(":exit", "exit system")
  )
  val defaultMenuItem = new DefaultMenuItem(annotate)
  val menu = new Menu("Welcome to the ProcessorShell!", lineReader, mainMenuItems, defaultMenuItem)

  menu.run()
}
