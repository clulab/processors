package org.clulab.sequences

import com.typesafe.config.{Config, ConfigFactory}

/**
  * Implements a multi-task learning (MTL) framework around the biLSTM-CRF of Lample et al. (2016)
  * We use GloVe embeddings and learned character embeddings
  * @author Mihai
  */
class LstmCrfMtl() {

}

object LstmCrfMtl {
  def main(args: Array[String]): Unit = {
    //
    // to set a custom config file add -Dconfig.file=/path/to/conf/file to the cmd line for sbt
    //
    val config = ConfigFactory.load()

    val taskManager = new TaskManager(config)
  }
}