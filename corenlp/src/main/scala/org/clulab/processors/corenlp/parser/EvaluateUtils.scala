package org.clulab.processors.corenlp.parser

import org.clulab.struct.{DirectedGraph, Edge}


object EvaluateUtils {

  case class Performance(
    // true positives
    tp: Int,
    // false positives
    fp: Int,
    // true negatives
    tn: Int,
    // false negatives
    fn: Int,
    // what is being evaluated?
    label: String
  ) {
    /** Precision for the label */
    def precision: Double = tp / (tp + fp).toDouble
    /** Recall for the label */
    def recall: Double = tp / (tp + fn).toDouble
    /** Harmonic mean of precision and recall for the label */
    def f1: Double = 2 * (precision * recall) / (precision + recall)

    def +(other: Any): Performance = other match {
      case p: Performance =>
        Performance(
          tp = this.tp + p.tp,
          fp = this.fp + p.fp,
          tn = this.tn + p.tn,
          fn = this.fn + p.fn,
          label = this.label
        )
//      case pp: Seq[Performance] =>
//        pp.fold(this)((prev, next) => prev + next)
      case unknown =>
        throw new Exception(s"Cannot add Performance and class of type ${unknown.getClass.getName}")
    }
  }

  def evaluate(
    gold: DirectedGraph[String],
    predicted: DirectedGraph[String],
    withEdgeLabel: Boolean
  ): Performance = {

    // how many of the gold edges were predicted?
    val tp: Seq[Edge[String]] = gold.edges.filter { g =>
      predicted.edges.exists { p =>
        withEdgeLabel match {
          // consider the relation label
          case true =>
            g.source == p.source && g.destination == p.destination && g.relation == p.relation
          // ignore the relation label
          case false =>
            g.source == p.source && g.destination == p.destination
        }
      }
    }
    // how many of the predicted edges were incorrect?
    val fp: Seq[Edge[String]] = predicted.edges.filterNot { p =>
      // check if edge not in true positives
      tp.exists { g =>
        withEdgeLabel match {
          // consider the relation label
          case true =>
            g.source == p.source && g.destination == p.destination && g.relation == p.relation
          // ignore the relation label
          case false =>
            g.source == p.source && g.destination == p.destination
        }
      }
    }

    require(tp.size + fp.size == predicted.edges.size, "TP + FP should equal number of predicted edges")

    // n/a
    val tn: Seq[Edge[String]] = Nil

    // how many of the gold did we miss?
    val fn: Seq[Edge[String]] = gold.edges.filterNot(g => tp contains g)

    val label = withEdgeLabel match {
      case true => "edges with relation label"
      case false => "edges without relation label"
    }

    Performance(tp = tp.size, fp = fp.size, tn = tn.size, fn = fn.size, label = label)
  }
}