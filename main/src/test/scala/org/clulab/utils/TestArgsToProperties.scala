package org.clulab.utils

import org.scalatest._

/**
 * 
 * User: mihais
 * Date: 4/3/13
 */
class TestArgsToProperties extends FlatSpec with Matchers {
  // NOTE we are reading from the filesystem on purpose, that is the test
  val props = StringUtils.argsToProperties(List("-props", "main/src/test/resources/org/clulab/utils/test.properties").toArray)

  "properties" should "contain p1 with value /some/path" in {
    val p1 = props.getProperty("p1")
    p1 should be ("/some/path")
  }

  it should "contain p2 with the value /some/path/subdir/123" in {
    val p2 = props.getProperty("p2")
    p2 should be ("/some/path/subdir/123")
  }

  it should "contain p4 set to a valid shell" in {
    val p4 = props.getProperty("p4")
    p4 should fullyMatch regex """shell is /bin/bash|shell is /bin/sh|shell is /bin/csh|shell is /bin/tcsh|shell is /bin/zsh"""
  }
}
