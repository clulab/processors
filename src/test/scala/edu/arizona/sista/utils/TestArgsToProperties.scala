package edu.arizona.sista.utils

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import junit.framework.Assert

/**
 * 
 * User: mihais
 * Date: 4/3/13
 */
class TestArgsToProperties extends AssertionsForJUnit {
  @Test def testArgsToProperties() {
    val props = StringUtils.argsToProperties(List("-props", "src/test/resources/edu/arizona/sista/utils/test.properties").toArray)

    val p1 = props.getProperty("p1")
    val p2 = props.getProperty("p2")

    Assert.assertTrue(p1 == "/some/path")
    Assert.assertTrue(p2 == "/some/path/subdir/123")

    val p4 = props.getProperty("p4")
    Assert.assertTrue(
      p4 == "shell is /bin/bash" ||
      p4 == "shell is /bin/sh" ||
      p4 == "shell is /bin/csh" ||
      p4 == "shell is /bin/tcsh" ||
      p4 == "shell is /bin/zsh")
  }
}
