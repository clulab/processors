package edu.arizona.sista.discourse.rstparser

import edu.arizona.sista.processors.{CorefMention, Document}
import edu.arizona.sista.struct.Counter
import edu.arizona.sista.struct.Tree
import Utils._

/**
 * Generates features necessary for relation classification
 * User: mihais
 * Date: 5/23/14
 */
class RelationFeatureExtractor(val filter:Set[String] = null) {
  var verbose = false

  def mkFeatures(left:DiscourseTree,
                 right:DiscourseTree,
                 doc:Document,
                 edus:Array[Array[(Int, Int)]],
                 corpusStats:CorpusStats,
                 label:String):Counter[String] = {
    val features = new Counter[String]

    def f(fn:String, fv:Double = 1) {
      // assert(features.isDefined)
      if(filter == null || filter.contains(prefix(fn, ":"))) {
        features.incrementCount(fn, fv)
      }
    }

    def printFeatures() {
      for (k <- features.keySet) {
        val v = features.getCount(k)
        println(s"\t$k: $v")
      }
    }

    /**
     * This aims to replicate the (Feng and Hirst, 2012) feature set
     * Equivalent to write_baseline_features in tree_feature_writer_Feng_Hirst.py
     */
    def writeBaselineFeatures(left:DiscourseTree,
                              right:DiscourseTree,
                              doc:Document,
                              edus:Array[Array[(Int, Int)]]) {

      val leftSubtree = findSubtree(left, doc)
      val rightSubtree = findSubtree(right, doc)

      f("Len_L", left.tokenCount)
      f("Len_R", right.tokenCount)
      f("Num_edus_L", left.eduCount)
      f("Num_edus_R", right.eduCount)
      f("num_edus_diff", left.eduCount - right.eduCount)
      f("num_tokens_diff", left.tokenCount - right.tokenCount)

      writePositionFeatures(left, doc, edus, leftSubtree, "L")
      writePositionFeatures(right, doc, edus, rightSubtree, "R")

      if(left.firstSentence == right.lastSentence) {
        if(sameSubtree(leftSubtree, rightSubtree)) f("embedded", 3)
        else f("embedded", 2)
      } else {
        f("embedded", 0)
      }

      // TODO: add last 4 from Table 1
    }

    def writeDomSetWithDepsFeatures(left:DiscourseTree,
                                    right:DiscourseTree,
                                    doc:Document,
                                    edus:Array[Array[(Int, Int)]],
                                    corpusStats:CorpusStats) {
      // this only works if syntactic dependencies are available
      if(! doc.sentences(left.firstSentence).dependencies.isDefined) {
        return
      }

      if(left.firstToken.sentence == right.lastToken.sentence) {
        val words = doc.sentences(left.firstSentence).words
        val tags = doc.sentences(left.firstSentence).tags.get
        val deps = doc.sentences(left.firstSentence).dependencies.get
        val (leftHead, leftHeadParent, leftParentRel) = Utils.findSyntacticHeadFromDependencies(deps, left.firstToken.token, left.lastToken.token)
        val (rightHead, rightHeadParent, rightParentRel) = Utils.findSyntacticHeadFromDependencies(deps, right.firstToken.token, right.lastToken.token)
        val commonAncestors = Utils.findCommonAncestorsFromDependencies(deps, left.firstToken.token, right.lastToken.token)

        if(leftHead != -1 && rightHead != -1) {
          var dominating:Int = -1
          var dominated:Int = -1
          var domRel:String = ""
          var leftDominates = true

          if(rightHeadParent != -1 &&
             rightHeadParent >= left.firstToken.token &&
             rightHeadParent <= left.lastToken.token) {
            leftDominates = true
            dominating = rightHeadParent
            dominated = rightHead
            domRel = rightParentRel
          } else if(leftHeadParent != -1 &&
                    leftHeadParent >= right.firstToken.token &&
                    leftHeadParent <= right.lastToken.token) {
            leftDominates = false
            dominating = leftHeadParent
            dominated = leftHead
            domRel = leftParentRel
          } else {
            // TODO: case when neither dominates (happens for negative examples?)
          }

          if(leftHead != -1) {
            val leftHeadPos = leftHead.toDouble / words.size.toDouble
            f(s"deps-leftheadpos", leftHeadPos)
          }
          if(rightHead != -1) {
            val rightHeadPos = rightHead.toDouble / words.size.toDouble
            f(s"deps-rightheadpos", rightHeadPos)
          }

          if(dominating != -1 && dominated != -1) {
            f("deps-dominates-" + leftDominates)

            for(ancestor <- commonAncestors) {
              if(ancestor == -1) {
                f("deps-ancestorisroot")
              } else {
                val aw = words(ancestor)
                f(s"deps-ancestorword:$aw")
                val at = tags(ancestor)
                f(s"deps-ancestortag:$at")
                for (d <- deps.incomingEdges(ancestor)) {
                  f(s"deps-ancestor-inc:${d._2}")
                }
              }
            }

            f(s"deps-dominatingword:${words(dominating)}")
            f(s"deps-dominatedword:${words(dominated)}")
            f(s"deps-dominatingtag:${tags(dominating)}")
            f(s"deps-dominatedtag:${tags(dominated)}")
            f(s"deps-domrel:${domRel}")

            f(s"$leftDominates-deps-dominatingword:${words(dominating)}")
            f(s"$leftDominates-deps-dominatedword:${words(dominated)}")
            f(s"$leftDominates-deps-dominatingtag:${tags(dominating)}")
            f(s"$leftDominates-deps-dominatedtag:${tags(dominated)}")
            f(s"$leftDominates-deps-domrel:${domRel}")
          } else {
            f("deps-samesent-dominates-unk")
          }
        }
      } else {
        f("dominates-unk")
      }
    }

    def writeDomSetFeatures(left:DiscourseTree,
                            right:DiscourseTree,
                            doc:Document,
                            edus:Array[Array[(Int, Int)]],
                            corpusStats:CorpusStats) {
      // this only works if constituent syntax is available
      if(! doc.sentences(left.firstSentence).syntacticTree.isDefined) {
        // backoff to dependency-based features
        if(doc.sentences(left.firstSentence).dependencies.isDefined) {
          writeDomSetWithDepsFeatures(left, right, doc, edus, corpusStats)
          return
        } else {
          throw new RuntimeException("ERROR: you need either constituent or dependency-based available for discourse parsing!")
        }
      }

      if(left.firstToken.sentence == right.lastToken.sentence) {
        val words = doc.sentences(left.firstSentence).words
        val tags = doc.sentences(left.firstSentence).tags.get
        val tree = doc.sentences(left.firstSentence).syntacticTree.get
        val (leftHead, leftHeadParent) = Utils.findSyntacticHead(tree, null, left.firstToken.token, left.lastToken.token)
        val (rightHead, rightHeadParent) = Utils.findSyntacticHead(tree, null, right.firstToken.token, right.lastToken.token)
        val commonAncestor = Utils.findSmallestCommonAncestor(tree, left.firstToken.token, right.lastToken.token)
        assert(commonAncestor != null)

        if(leftHead != null && rightHead != null) {
          //println(s"For DT with span [${left.firstToken}, ${left.lastToken}] found this tree as head:\n$leftHead")
          //println(s"For DT with span [${right.firstToken}, ${right.lastToken}] found this tree as head:\n$rightHead")

          var dominating:Tree = null
          var dominated:Tree = null
          var leftDominates = true

          if (rightHeadParent != null &&
              rightHeadParent.headOffset >= leftHead.startOffset &&
              rightHeadParent.headOffset < leftHead.endOffset) {
            //println("Right dominated by left!")
            leftDominates = true
            dominating = rightHeadParent
            dominated = rightHead
          } else if(leftHeadParent != null &&
                    leftHeadParent.headOffset >= rightHead.startOffset &&
                    leftHeadParent.headOffset < rightHead.endOffset) {
            //println("Left dominated by right!")
            leftDominates = false
            dominating = leftHeadParent
            dominated = leftHead
          } else {
            // TODO: case when neither dominates (happens for negative examples?)
          }

          val leftHeadPos = leftHead.headOffset.toDouble / words.size.toDouble
          val rightHeadPos = rightHead.headOffset.toDouble / words.size.toDouble
          f(s"leftheadpos", leftHeadPos)
          f(s"rightheadpos", rightHeadPos)

          if(dominating != null && dominated != null) {
            f("dominates-" + leftDominates)
            f(s"$leftDominates-dominatinglabel:${dominating.value}")
            f(s"$leftDominates-dominatedlabel:${dominated.value}")
            f(s"$leftDominates-ancestorlabel:${commonAncestor.value}")
            f(s"$leftDominates-dominatingword:${words(dominating.headOffset)}")
            f(s"$leftDominates-dominatedword:${words(dominated.headOffset)}")
            f(s"$leftDominates-ancestorword:${words(commonAncestor.headOffset)}")
            f(s"$leftDominates-dominatingtag:${tags(dominating.headOffset)}")
            f(s"$leftDominates-dominatedtag:${tags(dominated.headOffset)}")
            f(s"$leftDominates-ancestortag:${tags(commonAncestor.headOffset)}")

            f(s"dominatinglabel:${dominating.value}")
            f(s"dominatedlabel:${dominated.value}")
            f(s"ancestorlabel:${commonAncestor.value}")
            f(s"dominatingword:${words(dominating.headOffset)}")
            f(s"dominatedword:${words(dominated.headOffset)}")
            f(s"ancestorword:${words(commonAncestor.headOffset)}")
            f(s"dominatingtag:${tags(dominating.headOffset)}")
            f(s"dominatedtag:${tags(dominated.headOffset)}")
            f(s"ancestortag:${tags(commonAncestor.headOffset)}")
          } else {
            f("samesent-dominates-unk")
          }
        }

      } else {
        f("dominates-unk")
      }
    }

    def writePrefixSuffixFeatures(tree:DiscourseTree,
                            doc:Document,
                            corpusStats:CorpusStats,
                            suffix:String) {
      val words = extractWords(tree, doc)
      val tags = extractTags(tree, doc)

      f("prefix_" + suffix + ":" + mkNgram(words, 0, 3), 1)
      f("prefix_" + suffix + ":" + mkNgram(words, 0, 2), 1)
      f("prefix_" + suffix + ":" + mkNgram(words, 0, 1), 1)

      f("tprefix_" + suffix + ":" + mkNgram(tags, 0, 3), 1)
      f("tprefix_" + suffix + ":" + mkNgram(tags, 0, 2), 1)
      f("tprefix_" + suffix + ":" + mkNgram(tags, 0, 1), 1)

      f("suffix_" + suffix + ":" + mkNgram(words, words.size - 3, words.size), 1)
      f("suffix_" + suffix + ":" + mkNgram(words, words.size - 2, words.size), 1)
      f("suffix_" + suffix + ":" + mkNgram(words, words.size - 1, words.size), 1)

      f("tsuffix_" + suffix + ":" + mkNgram(tags, tags.size - 3, tags.size), 1)
      f("tsuffix_" + suffix + ":" + mkNgram(tags, tags.size - 2, tags.size), 1)
      f("tsuffix_" + suffix + ":" + mkNgram(tags, tags.size - 1, tags.size), 1)
    }

    def writeUnigrams(tree:DiscourseTree,
                      doc:Document,
                      corpusStats:CorpusStats,
                      suffix:String) {
      val words = extractWords(tree, doc, corpusStats.knownWords, threshold = 10)
      for(w <- words) {
        f("unigram:" + w)
      }
    }

    /** Equivalent to write_position_features in tree_feature_writer_Feng_Hirst.py */
    def writePositionFeatures(tree:DiscourseTree,
                              doc:Document,
                              edus:Array[Array[(Int, Int)]],
                              subtree:Tree,
                              suffix:String) {
      if(tree.firstToken.sentence == tree.lastToken.sentence) {
        f("Inside_sentence_" + suffix, 1)
        f("Sentence_EDUs_cover_" + suffix, (tree.lastEDU - tree.firstEDU).toDouble / edus(tree.firstSentence).length.toDouble)
        f("Sentence_tokens_cover_" + suffix, (tree.lastToken.token - tree.firstToken.token).toDouble / doc.sentences(tree.firstSentence).size.toDouble)

        if(subtree != null &&
          (subtree.startOffset < tree.firstToken.token ||
           subtree.endOffset > tree.lastToken.token + 1))
          f("Embedded_in_subtree_with_other_EDU" + suffix, 1)
        else
          f("Embedded_in_subtree_with_other_EDU" + suffix, 0)

      } else {
        f("Inside_sentence_" + suffix, 0)
        f("Sentence_EDUs_cover_" + suffix, 0)
        f("Sentence_tokens_cover_" + suffix, 0)
        f("Embedded_in_subtree_with_other_EDU_" + suffix, 0)
      }

      f("Sentence_span_" + suffix, tree.lastToken.sentence - tree.firstToken.sentence)
    }

    def writeWordPairFeatures(left:DiscourseTree,
                              right:DiscourseTree,
                              doc:Document,
                              edus:Array[Array[(Int, Int)]],
                              corpusStats:CorpusStats) {
      val t = 100
      assert(corpusStats != null)
      val leftWords = extractWords(left, doc, corpusStats.knownWords, threshold = t)
      val rightWords = extractWords(right, doc, corpusStats.knownWords, threshold = t)
      for(l <- leftWords) {
        for(r <- rightWords) {
          f("wordpair:" + l + "_" + r, 1)
        }
      }
    }

    def writeConnectiveFeatures(left:DiscourseTree,
                                right:DiscourseTree,
                                doc:Document,
                                edus:Array[Array[(Int, Int)]]) {
      val leftConnectives = extractConnectives(left, doc)
      val rightConnectives = extractConnectives(right, doc)
      for(l <- leftConnectives.keySet) {
        val lv = leftConnectives.getCount(l)
        for(r <- rightConnectives.keySet) {
          val rv = rightConnectives.getCount(r)
          f("connpair:" + l + "_" + r, lv * rv)
        }
      }
    }

    /* // unfortunately, not useful
    def writeSimFeatures(left:DiscourseTree,
                         right:DiscourseTree,
                         doc:Document,
                         corpusStats:CorpusStats) {
      if(w2v != null) {
        val leftWords = extractWords(left, doc)
        val rightWords = extractWords(right, doc)

        f("textsim", w2v.textSimilarity(leftWords, rightWords))
        f("avgsim", w2v.avgSimilarity(leftWords, rightWords))
        val max = w2v.maxSimilarity(leftWords, rightWords)
        if(max > Double.MinValue)
          f("maxsim", w2v.maxSimilarity(leftWords, rightWords))
      }
    }
    */

    def writeProdRuleFeatures(tree:DiscourseTree,
                              doc:Document,
                              depth:Int,
                              suffix:String) {
      if(depth > 1) return

      if(tree.isTerminal) {
        f(s"rule-$suffix-d$depth:NO-REL")
      } else {
        f(s"rule-$suffix-d$depth:${tree.children(0).relationLabel}-${tree.children(1).relationLabel}")
        for(c <- tree.children)
          writeProdRuleFeatures(c, doc, depth + 1, suffix)
      }
    }

    def writeCorefFeatures(left:DiscourseTree,
                           right:DiscourseTree,
                           doc:Document) {
      // this only works if coreference information is available
      if(! doc.coreferenceChains.isDefined) return

      var linkCount = 0
      for(chain <- doc.coreferenceChains.get.getChains) {
        val leftMentions = countMentions(chain, left)
        val rightMentions = countMentions(chain, right)
        linkCount += leftMentions * rightMentions
      }

      //println(s"Found $linkCount links between trees $left and $right")

      f("coreflinks", linkCount)
    }

    def countMentions(chain:Iterable[CorefMention],
                      tree:DiscourseTree):Int = {
      var count = 0
      for(m <- chain) {
        if(m.sentenceIndex >= tree.firstSentence && m.sentenceIndex <= tree.lastSentence &&
           m.headIndex >= tree.firstToken.token && m.headIndex <= tree.lastToken.token) {
          count += 1
        }
      }
      count
    }

    def writeJointProdRuleFeatures(left:DiscourseTree,
                                   right:DiscourseTree,
                                   doc:Document) {
      f(s"toprule:${left.relationLabel}-${right.relationLabel}")
    }


    if(verbose)
      println("Creating features for label " + label + "\nLEFT:\n" + left + "RIGHT:\n" + right)

    writeBaselineFeatures(left, right, doc, edus)
    writeWordPairFeatures(left, right, doc, edus, corpusStats)

    writePrefixSuffixFeatures(left, doc, corpusStats, "L")
    writePrefixSuffixFeatures(right, doc, corpusStats, "R")

    writeDomSetFeatures(left, right, doc, edus, corpusStats)
    // TODO: dom set features for right-most and left-most nuclei, in left and right trees

    writeProdRuleFeatures(left, doc, 1, "L")
    writeProdRuleFeatures(right, doc, 1, "R")
    writeJointProdRuleFeatures(left, right, doc)

    writeCorefFeatures(left, right, doc)

    if(verbose) printFeatures()
    features
  }
}


