package org.clulab.utils

import com.typesafe.config.{Config, ConfigFactory}

import scala.collection.JavaConverters._

/**
  * Classes that are configured with com.typesafe.config.Config
  * User: mihais
  * Date: 9/10/17
  * Last Modified: Update for Scala 2.12: java converters.
  */
trait Configured {
  def getConf:Config

  def getArgBoolean (argPath: String, defaultValue: Option[Boolean]): Boolean =
    if (getConf.hasPath(argPath)) getConf.getBoolean(argPath)
    else if(defaultValue.nonEmpty) defaultValue.get
    else throw new RuntimeException(s"ERROR: parameter $argPath must be defined!")

  def getArgInt (argPath: String, defaultValue: Option[Int]): Int =
    if (getConf.hasPath(argPath)) getConf.getInt(argPath)
    else if(defaultValue.nonEmpty) defaultValue.get
    else throw new RuntimeException(s"ERROR: parameter $argPath must be defined!")

  def getArgFloat (argPath: String, defaultValue: Option[Float]): Float =
    if (getConf.hasPath(argPath)) getConf.getDouble(argPath).toFloat
    else if(defaultValue.nonEmpty) defaultValue.get
    else throw new RuntimeException(s"ERROR: parameter $argPath must be defined!")

  def getArgString (argPath: String, defaultValue: Option[String]): String =
    if (getConf.hasPath(argPath)) getConf.getString(argPath)
    else if(defaultValue.nonEmpty) defaultValue.get
    else throw new RuntimeException(s"ERROR: parameter $argPath must be defined!")

  def getArgStrings (argPath: String, defaultValue: Option[Seq[String]]): Seq[String] =
    if (getConf.hasPath(argPath)) getConf.getStringList(argPath).asScala
    else if(defaultValue.nonEmpty) defaultValue.get
    else throw new RuntimeException(s"ERROR: parameter $argPath must be defined!")

  def contains(argPath:String):Boolean = getConf.hasPath(argPath)
}

class ConfigWithDefaults(config:Config) extends Configured {
  override def getConf: Config = config
}

object ConfigWithDefaults {
  def apply(config:Config): ConfigWithDefaults = {
    new ConfigWithDefaults(config)
  }

  def apply(configName:String): ConfigWithDefaults = {
    new ConfigWithDefaults(ConfigFactory.load(configName))
  }
}
