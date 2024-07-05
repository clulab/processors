package org.clulab.utils

class MenuItem(val command: MenuItem.MenuCommand) {

  def run(menu: Menu, key: String): Boolean = command(menu, key)
}

object MenuItem {
  type Command = () => Unit // a command that might be in the menu, but without the Menu or String parameters
  type DefaultCommand = String => Unit // the command that should process text
  type MenuCommand = (Menu, String) => Boolean
  val helpCommand: MenuCommand = (menu: Menu, _: String) => { menu.printCommands(); true }
  val exitCommand: MenuCommand = (_: Menu, _: String) => false
  val eofCommand: MenuCommand = (_: Menu, _: String) => { println(); println(); false }
  val defaultCommand: DefaultCommand = (input: String) => println(s"Unknown command: $input")
  val menuCommand: MenuCommand = newMenuCommand(defaultCommand)
  val defaultMenuItem: DefaultMenuItem = new DefaultMenuItem(menuCommand)

  def toDefaultCommand(command: Command): DefaultCommand = (_: String) => command()

  def newMenuCommand(defaultCommand: DefaultCommand): MenuCommand = (_: Menu, input: String) => {
    defaultCommand(input)
    true
  }

  def newMenuCommand(command: Command): MenuCommand = newMenuCommand(toDefaultCommand(command))

  def newSafeMenuCommand(defaultCommand: DefaultCommand): MenuCommand = (_: Menu, input: String) => {
    if (input.trim.nonEmpty)
      try {
        defaultCommand(input)
      }
      catch {
        case exception: Throwable =>
          println("Processing failed with the following error:")
          exception.printStackTrace()
      }
    true
  }

  def newSafeMenuCommand(command: Command): MenuCommand = newSafeMenuCommand(toDefaultCommand(command))
}

class MainMenuItem(val key: String, val hint: String, menuCommand: MenuItem.MenuCommand) extends MenuItem(menuCommand)

class SimpleMainMenuItem(key: String, hint: String, command: MenuItem.Command)
    extends MainMenuItem(key, hint, MenuItem.newMenuCommand(command))

class SafeMainMenuItem(key: String, hint: String, command: MenuItem.Command)
    extends MainMenuItem(key, hint, MenuItem.newSafeMenuCommand(command))

class HelpMenuItem(key: String, hint: String, menuCommand: MenuItem.MenuCommand = MenuItem.helpCommand) extends MainMenuItem(key, hint, menuCommand)

class ExitMenuItem(key: String, hint: String, menuCommand: MenuItem.MenuCommand = MenuItem.exitCommand) extends MainMenuItem(key, hint, menuCommand)

class DefaultMenuItem(menuCommand: MenuItem.MenuCommand = MenuItem.menuCommand) extends MenuItem(menuCommand)

class SimpleDefaultMenuItem(command: MenuItem.DefaultCommand = MenuItem.defaultCommand)
    extends DefaultMenuItem(MenuItem.newMenuCommand(command))

class SafeDefaultMenuItem(command: MenuItem.DefaultCommand = MenuItem.defaultCommand)
    extends DefaultMenuItem(MenuItem.newSafeMenuCommand(command))

class Menu(greeting: String, lineReader: LineReader, mainMenuItems: Seq[MainMenuItem], defaultMenuItem: DefaultMenuItem = MenuItem.defaultMenuItem) {
  val maxKeyLength: Int = mainMenuItems.foldLeft(0) { (maxKeyLength: Int, mainMenuItem: MainMenuItem) => Math.max(maxKeyLength, mainMenuItem.key.length) }
  val indent: String = "    "

  def withGreeting(greeting: String): Menu = new Menu(greeting, lineReader, mainMenuItems, defaultMenuItem)

  def printCommands(): Unit = {
    println("COMMANDS:")
    mainMenuItems.foreach { mainMenuItem =>
      val padding = " " * (maxKeyLength - mainMenuItem.key.length)
      println(s"$indent${mainMenuItem.key}$padding => ${mainMenuItem.hint}")
    }
  }

  def printEmpty(): Unit = {
    println("Enter a command or some text to process.")
    println()
    printCommands()
  }

  def processMenu: Boolean = {
    println()
    lineReader.readLineOpt().map { line =>
      println()
      if (line.trim.isEmpty) {
        printEmpty()
        true
      }
      else
        mainMenuItems
            .find(_.key == line)
            .map(_.command(this, line))
            .getOrElse(defaultMenuItem.command(this, line))
    }.getOrElse(MenuItem.eofCommand(this, ""))
  }

  def run(): Unit = {
    println(s"\n$greeting\n")
    printCommands()
    while (processMenu) { }
    println(s"Bye!\n")
  }
}
