package edu.arizona.sista.discourse.rstparser

import edu.arizona.sista.struct.Counter

/**
 * Stores useful stats from the training corpus
 * User: mihais
 * Date: 5/29/14
 */
class CorpusStats (var knownWords:Counter[String],
                   var knownNgrams:Counter[String]) extends Serializable
