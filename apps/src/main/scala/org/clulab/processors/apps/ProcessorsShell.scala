package org.clulab.processors.apps

import org.clulab.processors.Processor
import org.clulab.processors.clu.BalaurProcessor
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
class ProcessorsShell extends Shell {
  val balaur = new PromptedReloadableProcessor("(balaur)>>> ", () => new BalaurProcessor(), true)

  var proc = balaur // Note that the initial proc does not get initialized.

  val lineReader = new CliReader(proc.prompt, "user.home", ".processorshellhistory")
  val printWriter = new PrintWriter(System.out)

  def prepareProcessor(message: String, promptedReloadableProcessor: PromptedReloadableProcessor): Unit = {
    lineReader.setPrompt(promptedReloadableProcessor.prompt)
    println(message)
    proc = promptedReloadableProcessor
    proc.reload()
    proc.get.annotate("initialize me!")
  }

  def prepareBalaur(): Unit = prepareProcessor("Preparing BalaurProcessor...", balaur)

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
      new SafeMainMenuItem(":balaur", "use BalaurProcessor", prepareBalaur),
      // new SafeMainMenuItem(":reload", "reload rules for current processor from filesystem", reload),
      new ExitMenuItem(":exit", "exit system")
    )
    val defaultMenuItem = new SafeDefaultMenuItem(work)

    new Menu("Welcome to the ProcessorShell!", lineReader, mainMenuItems, defaultMenuItem)
  }
}

object ProcessorsShell extends App {
  new ProcessorsShell().shell()
}

class PromptedReloadableProcessor(val prompt: String, constructor: () => Processor, impatient: Boolean = true)
    extends ReloadableProcessor(constructor, impatient)
