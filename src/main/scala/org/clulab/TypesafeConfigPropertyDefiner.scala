package org.clulab

import ch.qos.logback.core.PropertyDefinerBase
import com.typesafe.config.ConfigFactory

// http://stackoverflow.com/questions/15097967/how-can-i-configure-system-properties-or-logback-configuration-variables-from-ty
class TypesafeConfigPropertyDefiner extends PropertyDefinerBase {

  private var propertyName: String = ""

  def getPropertyValue = ConfigFactory.load.getString(propertyName)

  def setPropertyName(propertyName: String): Unit = {
    this.propertyName = propertyName
  }
}
