package org.clulab.openie.conceptdiscovery

/**
 * Class for facilitating the concept discovery work.
 * @param id
 * @param sentences
 */
case class DiscoveryDocument(docid: String, sentences: Seq[ScoredSentence])

/**
 *
 * @param text text of the sentences
 * @param start start char offset (inclusive)
 * @param end start char offset (exclusive)
 * @param score
 */
case class ScoredSentence(text: String, start: Int, end: Int, score: Double)