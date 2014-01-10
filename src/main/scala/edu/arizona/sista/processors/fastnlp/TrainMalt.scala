package edu.arizona.sista.processors.fastnlp

import org.maltparserx.MaltConsoleEngine
import scala.collection.mutable.ArrayBuffer

/**
 * Trains maltparser with basic Stanford dependencies, using LR from liblinear
 * User: mihais
 * Date: 1/4/14
 */
object TrainMalt {

  def main(args:Array[String]) {
    if(args.length != 2) {
      println("Usage: edu.arizona.sista.processor.fastnlp.TrainMalt <training file> <model name>")
      System.exit(1)
    }
    val trainFile = args(0)
    val modelName = args(1)

    val engine = new MaltConsoleEngine
    engine.startEngine(mkArgs(trainFile, modelName))
    println(s"Training completed. Model file saved in $modelName")
  }

  def mkArgs(trainFile:String, modelName:String):Array[String] = {
    val args = new ArrayBuffer[String]()

    args += "-m"
    args += "learn"

    args += "-c"
    args += modelName

    // Crammer's SVM implementation performs well and is fast
    args += "-l"
    args += "liblinear"
    args += "-lo"
    args += "-s_4_-e_0.1_-c_0.2_-B_1.0"
    args += "-lv"
    args += "all"

    args += "-a"
    args += "nivreeager"

    args += "-i"
    args += trainFile

    println("Using command line: " + args.toList)
    args.toArray
  }
}
