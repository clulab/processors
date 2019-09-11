package org.clulab.utils

import edu.cmu.dynet.Initialize
import org.clulab.sequences.LstmCrfMtlParameters
import org.scalatest._

class TestZipModelLoader extends FlatSpec with Matchers {

  behavior of "ZipModelLoader"

  // Optionally place similar model files in test/resources
  // and in the main project directory to test those locations.
  // It was done manually, but just once.
  it should "load" in {
    // See LstmCrfMtl.
    Initialize.initialize()
    val modelName = "mtl-en"
    LstmCrfMtlParameters.load(modelName)
    // If something was wrong, it should have crashed by now.
  }
}
