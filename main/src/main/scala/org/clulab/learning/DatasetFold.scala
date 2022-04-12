package org.clulab.learning

/**
 * Stores one fold, containing testing and training partitions
 * Each tuple stores start and end offsets, starting from 0
 * User: mihais
 * Date: 5/1/13
 */
class DatasetFold(val testFold:(Int, Int), val trainFolds:Iterable[(Int, Int)]) {

}
