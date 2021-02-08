package org.clulab.utils

class MenuItem(val command: MenuItem.Command) {

  def run(menu: Menu, key: String): Boolean = command(menu, key)
}

object MenuItem {
  type Command = (Menu, String) => Boolean
  val helpCommand: Command = (menu: Menu, _: String) => { menu.printCommands(); true }
  val exitCommand: Command = (_: Menu, _: String) => false
  val defaultCommand: Command = (_: Menu, input: String) => { println(s"Unknown command: $input"); true }
  val eofCommand: Command = (_: Menu, _: String) => { println(); println(); false }
  val defaultMenuItem: DefaultMenuItem = new DefaultMenuItem(defaultCommand)
}

class MainMenuItem(val key: String, val hint: String, command: MenuItem.Command) extends MenuItem(command)

class HelpMenuItem(key: String, hint: String, command: MenuItem.Command = MenuItem.helpCommand) extends MainMenuItem(key, hint, command)

class ExitMenuItem(key: String, hint: String, command: MenuItem.Command = MenuItem.exitCommand) extends MainMenuItem(key, hint, command)

class DefaultMenuItem(command: MenuItem.Command = MenuItem.defaultCommand) extends MenuItem(command)

class Menu(greeting: String, lineReader: LineReader, mainMenuItems: Seq[MainMenuItem], defaultMenuItem: DefaultMenuItem = MenuItem.defaultMenuItem) {
  val maxKeyLength: Int = mainMenuItems.foldLeft(0) { (maxKeyLength: Int, mainMenuItem: MainMenuItem) => Math.max(maxKeyLength, mainMenuItem.key.length) }
  val indent: String = "    "

  def printCommands(): Unit = {
    println("COMMANDS:")
    mainMenuItems.foreach { mainMenuItem =>
      val padding = " " * (maxKeyLength - mainMenuItem.key.length)
      println(s"$indent${mainMenuItem.key}$padding => ${mainMenuItem.hint}")
    }
  }

  def processMenu: Boolean = {
    println()
    lineReader.readLineOpt().map { line =>
      println()
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
