package org.clulab.processors

import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.fastnlp.FastNLPProcessorWithSemanticRoles
import org.clulab.utils.CliReader
import org.clulab.utils.ExitMenuItem
import org.clulab.utils.HelpMenuItem
import org.clulab.utils.Menu
import org.clulab.utils.ReloadableProcessor
import org.clulab.utils.ReloadableShell
import org.clulab.utils.SafeDefaultMenuItem
import org.clulab.utils.SafeMainMenuItem
import org.clulab.utils.Shell

import java.io.PrintWriter

/**
  * A simple interactive shell
  * User: mihais
  * Date: 3/13/14
  * Last Modified: Fix compiler warning: remove redundant match case clause.
  */
class ProcessorShell extends Shell {
  Utils.initializeDyNet()

  val core = new PromptedReloadableProcessor("(core)>>> ", () => new CoreNLPProcessor()) // this uses the slower constituent parser
  val fast = new PromptedReloadableProcessor("(fast)>>> ", () => new FastNLPProcessorWithSemanticRoles()) // this uses the faster dependency parser
  val clu = new PromptedReloadableProcessor("(clu)>>> ", () => new CluProcessor(), true)

  var proc = clu // Note that the initial proc does not get initialized.

  val lineReader = new CliReader(proc.prompt, "user.home", ".processorshellhistory")
  val printWriter = new PrintWriter(System.out)

  def prepareProcessor(message: String, promptedReloadableProcessor: PromptedReloadableProcessor): Unit = {
    lineReader.setPrompt(promptedReloadableProcessor.prompt)
    println(message)
    proc = promptedReloadableProcessor
    proc.reload()
    proc.get.annotate("initialize me!")
  }

  def prepareCore(): Unit = prepareProcessor("Preparing CoreNLPProcessor...", core)

  def prepareFast(): Unit = prepareProcessor("Preparing FastNLPProcessor...", fast)

  def prepareClu(): Unit = prepareProcessor("Preparing CluProcessor...", clu)

  override def work(text: String): Unit = {
    val doc = proc.get.annotate(text)
    doc.prettyPrint(printWriter)
    printWriter.flush()
  }

  // We inherit now just from Shell, so no reloading is performed.
  def reload(): Unit = {
    println("The processor is reloading...")
    proc.reload()
  }

  override def mkMenu(): Menu = {
    val mainMenuItems = Seq(
      new HelpMenuItem(":help", "show commands"),
      new SafeMainMenuItem(":core", "use CoreNLPProcessor", prepareCore),
      new SafeMainMenuItem(":fast", "use FastNLPProcessor", prepareFast),
      new SafeMainMenuItem(":clu", "use CluProcessor", prepareClu),
      // new SafeMainMenuItem(":reload", "reload rules for current processor from filesystem", reload),
      new ExitMenuItem(":exit", "exit system")
    )
    val defaultMenuItem = new SafeDefaultMenuItem(work)

    new Menu("Welcome to the ProcessorShell!", lineReader, mainMenuItems, defaultMenuItem)
  }
}

object ProcessorShell extends App {
  new ProcessorShell().shell()
}

class PromptedReloadableProcessor(val prompt: String, constructor: () => Processor, impatient: Boolean = true)
    extends ReloadableProcessor(constructor, impatient)
