package org.clulab.learning

import java.util.zip.GZIPInputStream
import java.io.{BufferedInputStream, FileInputStream, FileOutputStream, FileWriter, ObjectInputStream, ObjectOutputStream, PrintWriter}

import org.slf4j.LoggerFactory

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.{BufferedSource, Source}
import org.clulab.struct.Counter
import org.clulab.struct.Lexicon
import org.clulab.utils.Files
import org.clulab.utils.Serializer

/**
 * Parent class for all datasets used for ranking problems
 * User: mihais
 * Date: 4/23/13
 * Last Modified: Fix compiler issue: import scala.io.Source.
 */
trait RankingDataset[F] {
  var featureLexicon = new Lexicon[F]

  /**
   * We follow the same convention for labels as svm_rank:
   * - Labels take positive integers as values
   * - Higher values indicate better rank, i.e., closer to the top
   * - You can have equal values in a group. This means that those datums have equal rank
   */
  val labels = new ArrayBuffer[Array[Int]]

  def += (queryDatums:Iterable[Datum[Int, F]]): Unit

  def numFeatures = featureLexicon.size
  def size = labels.size
  def datumSize = {
    var count = 0
    labels.foreach(q => count += q.length)
    count
  }

  def querySize(queryOffset:Int) = labels(queryOffset).length
  def featuresCounter(queryOffset:Int, datumOffset:Int):Counter[Int]

  def mkQueryDatums(queryOffset:Int):Array[Datum[Int, F]]

  def shuffle():(RankingDataset[F], Array[Int])
  def bootstrapSample(n:Int):(RankingDataset[F])

}

class BVFRankingDataset[F] extends RankingDataset[F] {
  val features = new ArrayBuffer[Array[Array[Int]]]

  def += (queryDatums:Iterable[Datum[Int, F]]): Unit = {
    val fvs = queryToArray(queryDatums)
    labels += fvs.map(fv => fv._1)
    features += fvs.map(fv => fv._2)
  }

  private def queryToArray(queryDatums:Iterable[Datum[Int, F]]):Array[(Int, Array[Int])] = {
    val b = new ListBuffer[(Int, Array[Int])]
    for(d <- queryDatums) {
      d match {
        case bd:BVFDatum[Int, F] => {
          b += new Tuple2[Int, Array[Int]](bd.label, featuresToArray(bd.features))
        }
        case _ => throw new RuntimeException("ERROR: you cannot add a non BVFDatum to a BVFRankingDataset!")
      }
    }
    b.toArray
  }

  private def featuresToArray(fs:Iterable[F]):Array[Int] = {
    val fb = new ListBuffer[Int]
    for(f <- fs) fb += featureLexicon.add(f)
    fb.toList.sorted.toArray
  }

  def featuresCounter(queryOffset:Int, datumOffset:Int):Counter[Int] = {
    val c = new Counter[Int]
    features(queryOffset)(datumOffset).foreach(f => c.incrementCount(f))
    c
  }

  def mkQueryDatums(queryOffset:Int):Array[Datum[Int, F]] = {
    val datums = new ArrayBuffer[Datum[Int, F]]
    for(i <- 0 until querySize(queryOffset)) {
      val feats = new ListBuffer[F]
      features(queryOffset)(i).foreach(f => feats += featureLexicon.get(f))
      datums += new BVFDatum[Int, F](labels(queryOffset)(i), feats.toList)
    }
    datums.toArray
  }

  def shuffle():(RankingDataset[F], Array[Int]) = {
    val datasetShuffled = new BVFRankingDataset[F]

    // Step 1: Create new order map
    var indicies = new ArrayBuffer[Int]()
    for (i <- 0 until labels.size) indicies.append(i)
    val orderMap = util.Random.shuffle(indicies).toArray

    // Step 2: Copy member variables
    // Step 2A: Copy over feature lexicon
    datasetShuffled.featureLexicon = this.featureLexicon

    // Step 2B: Copy over labels and features, in shuffled order
    for (i <- 0 until orderMap.size) {
      datasetShuffled.labels.append( this.labels(orderMap(i)) )
      datasetShuffled.features.append ( this.features(orderMap(i)) )
    }

    // Return shuffled dataset, and reordering information
    (datasetShuffled, orderMap)
  }

  override def bootstrapSample(n:Int):RankingDataset[F] = {
    // n - number of boostrap samples to draw from original dataset
    val datasetBootstrapped = new BVFRankingDataset[F]
    val datasetSize:Int = this.size

    // Step 1: Create new order map
    var orderMap = new Array[Int](n)
    for (i <- 0 until n) orderMap(i) = util.Random.nextInt(datasetSize)

    // Step 2: Copy member variables
    // Step 2A: Copy over feature lexicon, to maintain feature indicies from parent dataset
    datasetBootstrapped.featureLexicon = this.featureLexicon

    // Step 2B: Copy over labels, features, and values, in shuffled order
    for (i <- 0 until n) {
      datasetBootstrapped.labels.append( this.labels(orderMap(i)) )
      datasetBootstrapped.features.append ( this.features(orderMap(i)) )
    }

    // Return bootstrapped dataset
    datasetBootstrapped
  }
}

class RVFRankingDataset[F] extends BVFRankingDataset[F] with FeatureTraversable[F, Double] {
  val values = new ArrayBuffer[Array[Array[Double]]]

  override def += (queryDatums:Iterable[Datum[Int, F]]): Unit = {
    val fvs = queryToArray(queryDatums)
    labels += fvs.map(fv => fv._1)
    features += fvs.map(fv => fv._2)
    values += fvs.map(fv => fv._3)
  }

  private def queryToArray(queryDatums:Iterable[Datum[Int, F]]):Array[(Int, Array[Int], Array[Double])] = {
    val b = new ListBuffer[(Int, Array[Int], Array[Double])]
    for(d <- queryDatums) {
      d match {
        case rd:RVFDatum[Int, F] => {
          val fvs = featuresCounterToArray(d.featuresCounter)
          b += new Tuple3[Int, Array[Int], Array[Double]](
            rd.label,
            fvs.map(fv => fv._1),
            fvs.map(fv => fv._2))
        }
        case _ => throw new RuntimeException("ERROR: you cannot add a non RVFDatum to a RVFRankingDataset!")
      }
    }
    b.toArray
  }

  protected def featuresCounterToArray(fs:Counter[F]):Array[(Int, Double)] = {
    val fb = new ListBuffer[(Int, Double)]
    for(f <- fs.keySet) {
      fb += new Tuple2[Int, Double](featureLexicon.add(f), fs.getCount(f))
    }
    fb.sortBy(_._1).toArray
  }

  override def featuresCounter(queryOffset:Int, datumOffset:Int):Counter[Int] = {
    val c = new Counter[Int]
    val fs = features(queryOffset)(datumOffset)
    val vs = values(queryOffset)(datumOffset)
    for(i <- 0 until numFeatures) c.incrementCount(i, 0.0f)           // Include each feature in the counter keyset
    for(i <- 0 until fs.length) {
      c.incrementCount(fs(i), vs(i))
    }
    c
  }

  override def mkQueryDatums(queryOffset:Int):Array[Datum[Int, F]] = {
    val datums = new ArrayBuffer[Datum[Int, F]]
    for(i <- 0 until querySize(queryOffset)) {
      val feats = new Counter[F]
      val fs = features(queryOffset)(i)
      val vs = values(queryOffset)(i)
      for(j <- 0 until fs.length) {
        feats.incrementCount(featureLexicon.get(fs(j)), vs(j))
      }
      datums += new RVFDatum[Int, F](labels(queryOffset)(i), feats)
    }
    datums.toArray
  }

  override def shuffle():(RankingDataset[F], Array[Int]) = {
    val datasetShuffled = new RVFRankingDataset[F]

    // Step 1: Create new order map
    var indicies = new ArrayBuffer[Int]()
    for (i <- 0 until labels.size) indicies.append(i)
    val orderMap = util.Random.shuffle(indicies).toArray

    // Step 2: Copy member variables
    // Step 2A: Copy over feature lexicon
    datasetShuffled.featureLexicon = this.featureLexicon

    // Step 2B: Copy over labels, features, and values, in shuffled order
    for (i <- 0 until orderMap.size) {
      datasetShuffled.labels.append( this.labels(orderMap(i)) )
      datasetShuffled.features.append ( this.features(orderMap(i)) )
      datasetShuffled.values.append ( this.values(orderMap(i)) )
    }

    // Return shuffled dataset, and reordering information
    (datasetShuffled, orderMap)
  }

  override def bootstrapSample(n:Int):RankingDataset[F] = {
    // n - number of boostrap samples to draw from original dataset
    val datasetBootstrapped = new RVFRankingDataset[F]
    val datasetSize:Int = this.size

    // Step 1: Create new order map
    var orderMap = new Array[Int](n)
    for (i <- 0 until n) orderMap(i) = util.Random.nextInt(datasetSize)

    // Step 2: Copy member variables
    // Step 2A: Copy over feature lexicon, to maintain feature indicies from parent dataset
    datasetBootstrapped.featureLexicon = this.featureLexicon

    // Step 2B: Copy over labels, features, and values, in shuffled order
    for (i <- 0 until n) {
      datasetBootstrapped.labels.append( this.labels(orderMap(i)) )
      datasetBootstrapped.features.append ( this.features(orderMap(i)) )
      datasetBootstrapped.values.append ( this.values(orderMap(i)) )
    }

    // Return bootstrapped dataset
    datasetBootstrapped
  }

  def saveTo[F](fileName:String): Unit = {
    Serializer.save(this, fileName)
  }

  def featureUpdater: FeatureUpdater[F, Double] = new FeatureUpdater[F, Double] {
    def foreach[U](fn: ((F, Double)) => U): Unit = {
      for(i <- 0 until RVFRankingDataset.this.size) // group
        for(j <- 0 until features(i).size) // datum
          for (k <- 0 until features(i)(j).size) { // feature
            val fi = features(i)(j)(k)
            val v = values(i)(j)(k)
            val f = featureLexicon.get(fi)
            fn((f, v))
          }
    }

    def updateAll(fn: ((F, Double)) => Double): Unit = {
      for(i <- 0 until RVFRankingDataset.this.size) // group
        for(j <- 0 until features(i).size) // datum
          for (k <- 0 until features(i)(j).size) { // feature
          val fi = features(i)(j)(k)
            val v = values(i)(j)(k)
            val f = featureLexicon.get(fi)
            values(i)(j)(k) = fn((f, v))
          }
    }
  }
}

object RVFRankingDataset {
  val logger = LoggerFactory.getLogger(classOf[RVFRankingDataset[String]])

  def mkDatasetFromSvmRankResource(path: String): RVFRankingDataset[String] = {
    val stream = getClass.getClassLoader.getResourceAsStream(path)
    val source = if (path endsWith ".gz") {
      Source.fromInputStream(new GZIPInputStream(stream))
    } else {
      Source.fromInputStream(stream)
    }
    mkDatasetFromSvmRankFormat(source)
  }

  /** reads dataset from a file */
  def mkDatasetFromSvmRankFormat(filename: String): RVFRankingDataset[String] = {
    val source = if (filename endsWith ".gz") {
      val stream = Files.newGZIPInputStream(filename)
      Source.fromInputStream(stream)
    } else {
      Source.fromFile(filename)
    }
    mkDatasetFromSvmRankFormat(source)
  }

  def mkDatasetFromSvmRankFormat(source: BufferedSource): RVFRankingDataset[String] = {
    val dataset = new RVFRankingDataset[String]
    var crtQid = ""
    var crtBlock:ArrayBuffer[Datum[Int, String]] = null
    var blockCount = 0
    var datumCount = 0

    for(line <- source.getLines()) {
      // strip comments following #
      val pound = line.indexOf("#")
      var content = line
      if(pound > 0) {
        content = line.substring(0, pound)
      }
      //logger.debug("Parsing line: " + content)
      val bits = content.split("\\s+")
      val label = bits(0).toInt // we support ONLY integer labels
      assert(bits(1).startsWith("qid:") && bits(1).length > 4)
      val qid = bits(1).substring(4)

      val features = new Counter[String]
      for(i <- 2 until bits.length) {
        val fbits = bits(i).split(":")
        if(fbits.length != 2) {
          throw new RuntimeException("ERROR: invalid feature format: " + bits(i))
        }
        val f = fbits(0)
        val v = fbits(1).toDouble
        features.incrementCount(f, v)
      }
      val datum = new RVFDatum[Int, String](label, features)
      datumCount += 1

      if(qid == crtQid) {
        // append to current block
        crtBlock += datum
      } else {
        // store the crt block in the dataset
        assert(crtBlock == null || crtBlock.size > 0)
        if(crtBlock != null) {
          dataset += crtBlock
          blockCount += 1
        }
        // start a new one
        crtBlock = new ArrayBuffer[Datum[Int, String]]()
        crtQid = qid
        // append the crt datum
        crtBlock += datum
      }
    }
    if(crtBlock.size > 0) {
      dataset += crtBlock
      blockCount += 1
    }
    logger.debug(s"Loaded $blockCount blocks with $datumCount datums.")
    dataset
  }

  def mkDatumsFromSvmRankResource(path: String): Iterable[Iterable[Datum[Int, String]]] = {
    val stream = getClass.getClassLoader.getResourceAsStream(path)
    val source = if (path endsWith ".gz") {
      Source.fromInputStream(new GZIPInputStream(stream))
    } else {
      Source.fromInputStream(stream)
    }
    mkDatumsFromSvmRankFormat(source)
  }

  /** reads dataset from a file */
  def mkDatumsFromSvmRankFormat(filename: String): Iterable[Iterable[Datum[Int, String]]] = {
    val source = if (filename endsWith ".gz") {
      val stream = Files.newGZIPInputStream(filename)
      Source.fromInputStream(stream)
    } else {
      Source.fromFile(filename)
    }
    mkDatumsFromSvmRankFormat(source)
  }

  def mkDatumsFromSvmRankFormat(source: BufferedSource): Iterable[Iterable[Datum[Int, String]]] = {
    val queries = new ArrayBuffer[Iterable[Datum[Int, String]]]()
    var crtQid = ""
    var crtBlock:ArrayBuffer[Datum[Int, String]] = null
    var blockCount = 0
    var datumCount = 0

    for(line <- source.getLines()) {
      // strip comments following #
      val pound = line.indexOf("#")
      var content = line
      if(pound > 0) {
        content = line.substring(0, pound)
      }
      //logger.debug("Parsing line: " + content)
      val bits = content.split("\\s+")
      val label = bits(0).toInt // we support ONLY integer labels
      assert(bits(1).startsWith("qid:") && bits(1).length > 4)
      val qid = bits(1).substring(4)

      val features = new Counter[String]
      for(i <- 2 until bits.length) {
        val fbits = bits(i).split(":")
        if(fbits.length != 2) {
          throw new RuntimeException("ERROR: invalid feature format: " + bits(i))
        }
        val f = fbits(0)
        val v = fbits(1).toDouble
        features.incrementCount(f, v)
      }
      val datum = new RVFDatum[Int, String](label, features)
      datumCount += 1

      if(qid == crtQid) {
        // append to current block
        crtBlock += datum
      } else {
        // store the crt block in the dataset
        assert(crtBlock == null || crtBlock.size > 0)
        if(crtBlock != null) {
          queries += crtBlock
          blockCount += 1
        }
        // start a new one
        crtBlock = new ArrayBuffer[Datum[Int, String]]()
        crtQid = qid
        // append the crt datum
        crtBlock += datum
      }
    }
    if(crtBlock.size > 0) {
      queries += crtBlock
      blockCount += 1
    }
    logger.debug(s"Loaded $blockCount blocks with $datumCount datums.")
    queries
  }

  def saveToSvmRankFormat( queries:Iterable[Iterable[Datum[Int, String]]],
                           featureLexicon:Lexicon[String],
                           fn:String): Unit = {
    var qid = 0
    val os = new PrintWriter(new FileWriter(fn))
    for(query <- queries) {
      qid += 1
      for(datum <- query) {
        os.print(datum.label)
        os.print(s" qid:$qid")
        val fs = new ListBuffer[(Int, Double)]
        val c = datum.featuresCounter
        for(k <- c.keySet) {
          val fi = featureLexicon.get(k)
          if(fi.isDefined) {
            // logger.debug(s"Feature [$k] converted to index ${fi.get + 1}")
            fs += new Tuple2(fi.get + 1, c.getCount(k))
          }
        }
        val fss = fs.toList.sortBy(_._1)
        for(t <- fss) {
          os.print(s" ${t._1}:${t._2}")
        }
        os.println()
      }
    }
    os.close()
  }

  def loadFrom[F](fileName:String):RVFRankingDataset[F] = {
    Serializer.load(fileName)
  }
}

class RVFKRankingDataset[F] extends RVFRankingDataset[F] {
  /** Contains the String representation for each datum, on which the kernel is built */
  val kernels = new ArrayBuffer[Array[String]]

  override def += (queryDatums:Iterable[Datum[Int, F]]): Unit = {
    val fvsk = queryToArray(queryDatums)
    labels += fvsk.map(fv => fv._1)
    features += fvsk.map(fv => fv._2)
    values += fvsk.map(fv => fv._3)
    kernels += fvsk.map(fv => fv._4)
  }

  private def queryToArray(queryDatums:Iterable[Datum[Int, F]]):Array[(Int, Array[Int], Array[Double], String)] = {
    val b = new ListBuffer[(Int, Array[Int], Array[Double], String)]
    for(d <- queryDatums) {
      d match {
        case rd:RVFKDatum[Int, F] => {
          val fvs = featuresCounterToArray(d.featuresCounter)
          b += new Tuple4[Int, Array[Int], Array[Double], String](
            rd.label,
            fvs.map(fv => fv._1),
            fvs.map(fv => fv._2),
            rd.kernel)
        }
        case _ => throw new RuntimeException("ERROR: you cannot add a non RVFKDatum to a RVFKRankingDataset!")
      }
    }
    b.toArray
  }

  override def mkQueryDatums(queryOffset:Int):Array[Datum[Int, F]] = {
    val datums = new ArrayBuffer[Datum[Int, F]]
    for(i <- 0 until querySize(queryOffset)) {
      val feats = new Counter[F]
      val fs = features(queryOffset)(i)
      val vs = values(queryOffset)(i)
      for(j <- 0 until fs.length) {
        feats.incrementCount(featureLexicon.get(fs(j)), vs(j))
      }
      val k = kernels(queryOffset)(i)
      datums += new RVFKDatum[Int, F](labels(queryOffset)(i), feats, k)
    }
    datums.toArray
  }

  override def shuffle():(RankingDataset[F], Array[Int]) = {
    val datasetShuffled = new RVFKRankingDataset[F]

    // Step 1: Create new order map
    var indicies = new ArrayBuffer[Int]()
    for (i <- 0 until labels.size) indicies.append(i)
    val orderMap = util.Random.shuffle(indicies).toArray

    // Step 2: Copy member variables
    // Step 2A: Copy over feature lexicon
    datasetShuffled.featureLexicon = this.featureLexicon

    // Step 2B: Copy over labels, features, and values, in shuffled order
    for (i <- 0 until orderMap.size) {
      datasetShuffled.labels.append( this.labels(orderMap(i)) )
      datasetShuffled.features.append ( this.features(orderMap(i)) )
      datasetShuffled.values.append ( this.values(orderMap(i)) )
      datasetShuffled.kernels.append ( this.kernels(orderMap(i)) )
    }

    // Return shuffled dataset, and reordering information
    (datasetShuffled, orderMap)
  }

  override def bootstrapSample(n:Int):RankingDataset[F] = {
    // n - number of boostrap samples to draw from original dataset
    val datasetBootstrapped = new RVFKRankingDataset[F]
    val datasetSize:Int = this.size

    // Step 1: Create new order map
    var orderMap = new Array[Int](n)
    for (i <- 0 until n) orderMap(i) = util.Random.nextInt(datasetSize)

    // Step 2: Copy member variables
    // Step 2A: Copy over feature lexicon, to maintain feature indicies from parent dataset
    datasetBootstrapped.featureLexicon = this.featureLexicon

    // Step 2B: Copy over labels, features, and values, in shuffled order
    for (i <- 0 until n) {
      datasetBootstrapped.labels.append( this.labels(orderMap(i)) )
      datasetBootstrapped.features.append ( this.features(orderMap(i)) )
      datasetBootstrapped.values.append ( this.values(orderMap(i)) )
      datasetBootstrapped.kernels.append( this.kernels(orderMap(i)) )
    }

    // Return bootstrapped dataset
    datasetBootstrapped
  }

}
