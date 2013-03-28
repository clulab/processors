package edu.arizona.sista.utils

import java.util.Properties
import collection.mutable.ListBuffer
import java.io.{FileInputStream, BufferedInputStream}
import scala.collection.JavaConversions._

/**
 * Converts a command line to properties; and many other useful String utils
 * User: mihais
 * Date: 2/12/13
 */
object StringUtils {
  val PROPS = "props"
  val PROPERTIES = "properties"

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
          val is = new BufferedInputStream(new FileInputStream(value.get))
          val propsFromFile = new Properties()
          propsFromFile.load(is)
          // trim all values, they may have trailing spaces
          for (k <- propsFromFile.keySet()) {
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

    if (verbose) {
      println("Using the following properties:")
      for (k <- result.keySet()) {
        println("\t" + k + " = " + result.getProperty(k.asInstanceOf[String]))
      }
    }

    result
  }

  def getInt(props:Properties, name:String, default:Int):Int = {
    val s = props.getProperty(name)
    if (s == null) return default
    s.toInt
  }

  def getBool(props:Properties, name:String, default:Boolean):Boolean = {
    val s = props.getProperty(name)
    if (s == null) return default
    s.toBoolean
  }

  def getDouble(props:Properties, name:String, default:Double):Double = {
    val s = props.getProperty(name)
    if (s == null) return default
    s.toDouble
  }

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
}
