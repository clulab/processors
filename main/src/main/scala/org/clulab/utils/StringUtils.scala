package org.clulab.utils

import java.io.{ FileInputStream, BufferedInputStream, PrintWriter, StringWriter }
import java.util.Properties
import java.util.regex.Pattern

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

/**
 * Converts a command line to properties; and other useful String utils
 * User: mihais
 * Date: 2/12/13
 * Last Modified: Update for Scala 2.12: java converters.
 */
object StringUtils {
  val PROPS = "props"
  val PROPERTIES = "properties"
  val VARIABLE = Pattern.compile("\\$\\{[\\w\\d\\_\\-]+\\}", Pattern.CASE_INSENSITIVE)

  def argsToProperties(args: Array[String], verbose:Boolean = true): Properties = {
    val result = new Properties()
    val otherArgs = new ListBuffer[String]
    var i = 0
    while (i < args.length) {
      var key = args(i)
      var value:Option[String] = None
      if (key.length > 0 && key.startsWith("-")) {
        if (key.length > 1 && key.charAt(1) == '-') key = key.substring(2)
        else key = key.substring(1)
        if (i < args.length - 1 && (args(i + 1).length == 0 || (args(i + 1).length > 0 && ! args(i + 1).startsWith("-")))) {
          value = Some(args(i + 1))
          i = i + 1
        }

        if ((key == PROPERTIES || key == PROPS) && ! value.isEmpty) {
          // a props file was specified. read props from there
          println(s"loading props from file ${value.get}")
          val is = new BufferedInputStream(new FileInputStream(value.get))
          val propsFromFile = new Properties()
          propsFromFile.load(is)
          // trim all values, they may have trailing spaces
          for (k <- propsFromFile.keySet().asScala) {
            val v = propsFromFile.getProperty(k.asInstanceOf[String]).trim
            result.setProperty(k.asInstanceOf[String], v)
          }
          is.close()
        } else {
          result.setProperty(key, value.getOrElse("true"))
        }
        i = i + 1
      } else {
        otherArgs += key
      }
    }

    // otherArgs contains all args that do not match the "-key value" pattern
    // add them as a separate property
    if (! otherArgs.isEmpty) {
      result.setProperty("", otherArgs.toList.mkString(" "))
    }

    val normedProps = instantiateVariables(result)

    if (verbose) {
      println("Using the following properties:")
      for (k <- normedProps.keySet().asScala) {
        println("\t" + k + " = " + normedProps.getProperty(k.asInstanceOf[String]))
      }
    }

    normedProps
  }

  def argsToMap(args: Array[String], verbose: Boolean = true): Map[String, String] =
      argsToProperties(args, verbose).asScala.toMap

  private def instantiateVariables(props:Properties):Properties = {
    val newProps = new Properties()

    for (key <- props.keySet().asScala) {
      val value = props.getProperty(key.asInstanceOf[String])
      val m = VARIABLE.matcher(value)
      var offset = 0
      val norm = new StringBuilder
      while(m.find()) {
        val name = value.substring(m.start() + 2, m.end() - 1)
        // println("Found variable: " + name)
        norm.append(value.substring(offset, m.start()))
        norm.append(varValue(name, props))
        offset = m.end()
      }
      norm.append(value.substring(offset))
      newProps.put(key, norm.toString())
    }

    newProps
  }

  private def varValue(name:String, props:Properties):String = {
    // check if the Properties contain this variable
    if (props.containsKey(name))
      return props.getProperty(name)

    // check if the environment contains this variable
    val env = System.getenv(name)
    if (env != null) return env

    throw new RuntimeException("ERROR: cannot instantiate variable \"" + name + "\" in properties!")
  }

  def getStringOption(props: Properties, name: String): Option[String] = {
    val s = props.getProperty(name)
    if (s == null) None else Some(s)
  }

  def getInt(props:Properties, name:String, default:Int):Int = getIntOption(props,name).getOrElse(default)

  def getIntOption(props:Properties, name: String):Option[Int] = getStringOption(props, name).map(_.toInt)

  def getBool(props:Properties, name:String, default:Boolean):Boolean = getBoolOption(props, name).getOrElse(default)

  def getBoolOption(props: Properties, name:String) = getStringOption(props, name).map(_.toBoolean)

  def getDoubleOption(props:Properties, name:String) = getStringOption(props, name).map(_.toDouble)

  def getDouble(props: Properties, name:String, default:Double) = getDoubleOption(props, name).getOrElse(default)

  def toIntArray(v:String, sep:String = " "):Array[Int] = {
    val bits = v.split(sep)
    val ints = new Array[Int](bits.length)
    var i = 0
    while(i < bits.length) {
      ints(i) = bits(i).toInt
      i += 1
    }
    ints
  }

  /** Format the given exception as a string and return the string. */
  def exceptionToString (ex: Exception): String = {
    val sw = new StringWriter
    ex.printStackTrace(new PrintWriter(sw))
    sw.toString
  }

  /** Generates the stem of a word, according to the Porter algorithm */
  def porterStem(s:String): String = {
    val ps = new PorterStemmer
    val lowerS = s.toLowerCase()
    ps.add(lowerS.toCharArray, lowerS.length)
    ps.stem()
    ps.toString
  }

  def before(string: String, index: Int, all: Boolean, keep: Boolean): String = {
    if (index < 0)
      if (all) string
      else ""
    else string.substring(0, index + (if (keep) 1 else 0))
  }

  def beforeLast(string: String, char: Char, all: Boolean = true, keep: Boolean = false): String =
    before(string, string.lastIndexOf(char), all, keep)

  def beforeFirst(string: String, char: Char, all: Boolean = true, keep: Boolean = false): String =
    before(string, string.indexOf(char), all, keep)

  def after(string: String, index: Int, all: Boolean, keep: Boolean): String = {
    if (index < 0)
      if (all) string
      else ""
    else string.substring(index + (if (keep) 0 else 1))
  }

  def afterLast(string: String, char: Char, all: Boolean = true, keep: Boolean = false): String =
    after(string, string.lastIndexOf(char), all, keep)

  def afterFirst(string: String, char: Char, all: Boolean = true, keep: Boolean = false): String =
    after(string, string.indexOf(char), all, keep)

  def med(source: String, target: String): Int = MED(source, target).getDistance
}
