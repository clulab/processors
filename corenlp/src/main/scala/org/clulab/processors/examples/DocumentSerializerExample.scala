package org.clulab.processors.examples

import java.io.{BufferedReader, FileReader}

import org.clulab.serialization.DocumentSerializer

/**
 *
 * User: mihais
 * Date: 10/1/14
 */
object DocumentSerializerExample {
  def main(args:Array[String]): Unit = {
    val ds = new DocumentSerializer
    val r = new BufferedReader(new FileReader(args(0)))
    var done = false
    var count = 0
    while(! done) {
      val d = ds.load(r)
      if(d == null) {
        done = true
      } else {
        count += 1
        if(count % 10 == 0)
          println(s"Loaded $count documents...")
      }
    }
    r.close()
    println(s"Done! Loaded $count documents.")
  }
}
