package org.clulab.utils

/**
  * An interactive shell to be used to test various NLP components
  */
abstract class Shell {
  /** Initialize the NLP component needed for the work method
    * if that needs to happen after shell() is called.  Otherwise,
    * such initialization can happen in the subclass constructor. */
  def initialize(): Unit = ()
  /** The actual work, including printing out the output */
  def work(text: String): Unit

  def shell() {

    def workSafely(menu: Menu, text: String): Boolean = {
      if (text.trim.nonEmpty)
        try {
          work(text)
        }
        catch {
          case exception: Throwable =>
            println("Processing failed with the following error:")
            exception.printStackTrace()
        }
      true
    }

    initialize()

    val lineReader = new CliReader("(shell)>>> ", "user.home", ".shellhistory")
    val mainMenuItems = Seq(
      new HelpMenuItem(":help", "show commands"),
      new ExitMenuItem(":exit", "exit system")
    )
    val defaultMenuItem = new DefaultMenuItem(workSafely)
    val menu = new Menu("Welcome to the shell!", lineReader, mainMenuItems, defaultMenuItem)

    menu.run()
  }
}
