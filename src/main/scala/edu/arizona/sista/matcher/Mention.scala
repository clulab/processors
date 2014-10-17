package edu.arizona.sista.matcher

// annotated tokens in a BioDocument
case class Annotation(paperId: String,
                      section: String,
                      sentence: Int,
                      tokenInterval: Interval)

abstract class Mention(val annotation: Annotation)
