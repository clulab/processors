package org.clulab.utils

import com.typesafe.config.Config

import scala.collection.JavaConversions._

/**
  * Classes that are configured with com.typesafe.config.Config
  * User: mihais
  * Date: 9/10/17
  */
trait Configured {
  def getConf:Config

  protected def getArgBoolean (argPath: String, defaultValue: Option[Boolean]): Boolean =
    if (getConf.hasPath(argPath)) getConf.getBoolean(argPath)
    else if(defaultValue.nonEmpty) defaultValue.get
    else throw new RuntimeException(s"ERROR: parameter $argPath must be defined!")

  protected def getArgInt (argPath: String, defaultValue: Option[Int]): Int =
    if (getConf.hasPath(argPath)) getConf.getInt(argPath)
    else if(defaultValue.nonEmpty) defaultValue.get
    else throw new RuntimeException(s"ERROR: parameter $argPath must be defined!")

  protected def getArgString (argPath: String, defaultValue: Option[String]): String =
    if (getConf.hasPath(argPath)) getConf.getString(argPath)
    else if(defaultValue.nonEmpty) defaultValue.get
    else throw new RuntimeException(s"ERROR: parameter $argPath must be defined!")

  protected def getArgStrings (argPath: String, defaultValue: Option[Seq[String]]): Seq[String] =
    if(getConf.hasPath(argPath)) getConf.getStringList(argPath) // this is a Java List that gets converted to Seq thru JavaConversions
    else if(defaultValue.nonEmpty) defaultValue.get
    else throw new RuntimeException(s"ERROR: parameter $argPath must be defined!")
}
