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

  /** If your subclass's menu is different, override this method. */
  def mkMenu(): Menu = {
    val lineReader = new CliReader("(shell)>>> ", "user.home", ".shellhistory")
    val mainMenuItems = Seq(
      new HelpMenuItem(":help", "show commands"),
      new ExitMenuItem(":exit", "exit system")
    )
    val defaultMenuItem = new SafeDefaultMenuItem(work)

    new Menu("Welcome to the shell!", lineReader, mainMenuItems, defaultMenuItem)
  }

  def shell(): Unit = {
    initialize()
    mkMenu().run()
  }
}

abstract class ReloadableShell extends Shell {
  def reload(): Unit

  override def mkMenu(): Menu = {
    // This IdeReader needed for debugging in IntelliJ for Windows.
    // val lineReader = new IdeReader("(shell)>>> ")
    val lineReader = new CliReader("(shell)>>> ", "user.home", ".shellhistory")
    val mainMenuItems = Seq(
      new HelpMenuItem(":help", "show commands"),
      new SafeMainMenuItem(":reload", "reload rules from filesystem", reload),
      new ExitMenuItem(":exit", "exit system")
    )
    val defaultMenuItem = new SafeDefaultMenuItem(work)

    new Menu("Welcome to the shell!", lineReader, mainMenuItems, defaultMenuItem)
  }
}