package org.clulab.fatdynet.apps

import edu.cmu.dynet._
import org.clulab.fatdynet.utils.Closer.AutoCloser
import org.clulab.fatdynet.utils.Loader
import org.clulab.fatdynet.utils.Loader.ClosableModelSaver
import org.clulab.fatdynet.utils.Transducer

import scala.util.Random

case class PairModel(w: Parameter, b: Parameter, v: Parameter, a: Parameter, model: ParameterCollection)

case class PairTransformation(inputs: Array[Int], output: Int) {

  override def toString(): String = getClass.getSimpleName + "(" + inputs.mkString("(", ", ", ")") + " -> " + output.toString() + ")"

  // Testing
  def transform(inputValues: Array[Float]): Unit = {
    inputs.indices.foreach { index =>
      inputValues(index) = inputs(index)
    }
  }

  // Training
  def transform(inputValues: Array[Float], outputValue: FloatPointer): Unit = {
    transform(inputValues)
    outputValue.set(output)
  }
}

object PairExampleApp {
  protected val random: Random = new Random(1234L)

  val LAYERS_SIZE = 1

  val  INPUT_SIZE = 1
  val HIDDEN_SIZE = 4
  val OUTPUT_SIZE = 1

  val ITERATIONS = 2000

  val transformations: Seq[PairTransformation] = Seq(
    // For pairs of ones anywhere in sequence
    PairTransformation(Array(0, 0), 0),
    PairTransformation(Array(0, 1), 0),
    PairTransformation(Array(1, 0), 0),
    PairTransformation(Array(1, 1), 1),

    PairTransformation(Array(0, 0, 0), 0),
    PairTransformation(Array(0, 0, 1), 0),
    PairTransformation(Array(0, 1, 0), 0),
    PairTransformation(Array(0, 1, 1), 1),
    PairTransformation(Array(1, 0, 0), 0),
    PairTransformation(Array(1, 0, 1), 0),
    PairTransformation(Array(1, 1, 0), 1),
    PairTransformation(Array(1, 1, 1), 1),

    PairTransformation(Array(0, 0, 0, 0), 0),
    PairTransformation(Array(0, 0, 0, 1), 0),
    PairTransformation(Array(0, 0, 1, 0), 0),
    PairTransformation(Array(0, 0, 1, 1), 1),
    PairTransformation(Array(0, 1, 0, 0), 0),
    PairTransformation(Array(0, 1, 0, 1), 0),
    PairTransformation(Array(0, 1, 1, 0), 1),
    PairTransformation(Array(0, 1, 1, 1), 1),
    PairTransformation(Array(1, 0, 0, 0), 0),
    PairTransformation(Array(1, 0, 0, 1), 0),
    PairTransformation(Array(1, 0, 1, 0), 0),
    PairTransformation(Array(1, 0, 1, 1), 1),
    PairTransformation(Array(1, 1, 0, 0), 1),
    PairTransformation(Array(1, 1, 0, 1), 1),
    PairTransformation(Array(1, 1, 1, 0), 1),
    PairTransformation(Array(1, 1, 1, 1), 1),

    PairTransformation(Array(0, 0, 0, 0, 0), 0),
    PairTransformation(Array(0, 0, 0, 0, 1), 0),
    PairTransformation(Array(0, 0, 0, 1, 0), 0),
    PairTransformation(Array(0, 0, 0, 1, 1), 1),
    PairTransformation(Array(0, 0, 1, 0, 0), 0),
    PairTransformation(Array(0, 0, 1, 0, 1), 0),
    PairTransformation(Array(0, 0, 1, 1, 0), 1),
    PairTransformation(Array(0, 0, 1, 1, 1), 1),
    PairTransformation(Array(0, 1, 0, 0, 0), 0),
    PairTransformation(Array(0, 1, 0, 0, 1), 0),
    PairTransformation(Array(0, 1, 0, 1, 0), 0),
    PairTransformation(Array(0, 1, 0, 1, 1), 1),
    PairTransformation(Array(0, 1, 1, 0, 0), 1),
    PairTransformation(Array(0, 1, 1, 0, 1), 1),
    PairTransformation(Array(0, 1, 1, 1, 0), 1),
    PairTransformation(Array(0, 1, 1, 1, 1), 1),

    PairTransformation(Array(1, 0, 0, 0, 0), 0),
    PairTransformation(Array(1, 0, 0, 0, 1), 0),
    PairTransformation(Array(1, 0, 0, 1, 0), 0),
    PairTransformation(Array(1, 0, 0, 1, 1), 1),
    PairTransformation(Array(1, 0, 1, 0, 0), 0),
    PairTransformation(Array(1, 0, 1, 0, 1), 0),
    PairTransformation(Array(1, 0, 1, 1, 0), 1),
    PairTransformation(Array(1, 0, 1, 1, 1), 1),
    PairTransformation(Array(1, 1, 0, 0, 0), 1),
    PairTransformation(Array(1, 1, 0, 0, 1), 1),
    PairTransformation(Array(1, 1, 0, 1, 0), 1),
    PairTransformation(Array(1, 1, 0, 1, 1), 1),
    PairTransformation(Array(1, 1, 1, 0, 0), 1),
    PairTransformation(Array(1, 1, 1, 0, 1), 1),
    PairTransformation(Array(1, 1, 1, 1, 0), 1),
    PairTransformation(Array(1, 1, 1, 1, 1), 1)
  )

  protected def mkPredictionGraph(pairModel: PairModel, xValues: Seq[Float], builder: RnnBuilder): Expression = {
    // The graph will grow and grow without this next line.
    ComputationGraph.renew()
    // Use the new graph.
    builder.newGraph()

    val xs = xValues.map(Expression.input)
    val builderOutputs = Transducer.transduce(builder, xs)
    val builderOutput = builderOutputs.last

    val W = Expression.parameter(pairModel.w)
    val b = Expression.parameter(pairModel.b)
    val V = Expression.parameter(pairModel.v)
    val a = Expression.parameter(pairModel.a)
    val y = V * Expression.tanh(W * builderOutput + b) + a

    y
  }

  def train: (PairModel, Seq[Float], RnnBuilder) = {
    val model = new ParameterCollection
    val trainer = new SimpleSGDTrainer(model) // i.e., stochastic gradient descent trainer

    val WParameter = model.addParameters(Dim(HIDDEN_SIZE, HIDDEN_SIZE))
    val bParameter = model.addParameters(Dim(HIDDEN_SIZE))
    val VParameter = model.addParameters(Dim(OUTPUT_SIZE, HIDDEN_SIZE))
    val aParameter = model.addParameters(Dim(OUTPUT_SIZE))

    val rnnModel = new ParameterCollection
    val builder = new LstmBuilder(LAYERS_SIZE, INPUT_SIZE, HIDDEN_SIZE, rnnModel)
    val pairModel = PairModel(WParameter, bParameter, VParameter, aParameter, rnnModel)

    val yValue = new FloatPointer // because OUTPUT_SIZE is 1

    for (iteration <- 0 until ITERATIONS) {
      val lossValue = random.shuffle(transformations).map { transformation =>
      val xValues = new Array[Float](transformation.inputs.length)

        transformation.transform(xValues, yValue)

        val yPrediction = mkPredictionGraph(pairModel, xValues, builder)
        val y = Expression.input(transformation.output)

        val loss = Expression.squaredDistance(yPrediction, y)
        val lossValue = loss.value().toFloat()

//        println()
//        println("Computation graphviz structure:")
//        ComputationGraph.printGraphViz()

        ComputationGraph.backward(loss)
        trainer.update()
        lossValue
      }.sum

      println(s"index = $iteration, loss = $lossValue")
      trainer.learningRate *= 0.999f
    }

    val results = predict(pairModel, builder)

    (pairModel, results, builder)
  }

  def predict(pairModel: PairModel, builder: RnnBuilder): Seq[Float] = {
    var count = 0

    println
    val result = transformations.map { transformation =>
      val xValues = new Array[Float](transformation.inputs.length)

      transformation.transform(xValues)

      val yPrediction = mkPredictionGraph(pairModel, xValues, builder)
      val yValue = yPrediction.value().toFloat()
      val correct = transformation.output == yValue.round

      if (correct)
        count += 1
      println(s"TRANSFORMATION = $transformation, PREDICTION = $yValue, CORRECT = $correct")
      yValue
    }
    val accuracy = count / transformations.size.toFloat

    println(s"Accuracy: $count / ${transformations.size} = $accuracy")
    result
  }

  def save(filename: String, pairModel: PairModel): Unit = {
    new ClosableModelSaver(filename).autoClose { saver =>
      saver.addParameter(pairModel.w, "/W")
      saver.addParameter(pairModel.b, "/b")
      saver.addParameter(pairModel.v, "/V")
      saver.addParameter(pairModel.a, "/a")
      saver.addModel(pairModel.model, "/model")
    }
  }

  def load(filename: String): (PairModel, RnnBuilder) = {
    val (optionBuilder, optionModel, parameters, _) = Loader.loadLstm(filename)
    val WParameters = parameters("/W")
    val bParameters = parameters("/b")
    val VParameters = parameters("/V")
    val aParameters = parameters("/a")

    (PairModel(WParameters, bParameters, VParameters, aParameters, optionModel.get), optionBuilder.get)
  }

  def main(args: Array[String]) {
    val filename = "PairModel.dat"

    Initialize.initialize(Map("random-seed" -> 2522620396L))

    val (pairModel1, initialResults, builder1) = train
    val expectedResults = predict(pairModel1, builder1)
    save(filename, pairModel1)

    val (pairModel2, builder2) = load(filename)
    val actualResults = predict(pairModel2, builder2)

    assert(initialResults == expectedResults)
    assert(expectedResults == actualResults)
  }
}
