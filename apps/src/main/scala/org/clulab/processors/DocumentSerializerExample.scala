package org.clulab.processors

import org.clulab.serialization.DocumentSerializer

import java.io.{BufferedReader, FileReader}
import scala.util.Using

/**
 *
 * User: mihais
 * Date: 10/1/14
 */
object DocumentSerializerExample {
  def main(args:Array[String]): Unit = {
    var count = 0
    Using.resource(new BufferedReader(new FileReader(args(0)))) { r =>
      val ds = new DocumentSerializer
      var done = false
      while (!done) {
        val d = ds.load(r)
        if (d == null) {
          done = true
        } else {
          count += 1
          if (count % 10 == 0)
            println(s"Loaded $count documents...")
        }
      }
    }
    println(s"Done! Loaded $count documents.")
  }
}
