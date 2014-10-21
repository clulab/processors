package edu.arizona.sista.matcher


abstract class Mention(val label: String, val sentence: Int, val tokenInterval: Interval)

class EntityMention(label: String, sentence: Int, tokenInterval: Interval) extends Mention(label, sentence, tokenInterval)
