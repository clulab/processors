/**
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.uci.jforestsx.learning;

import edu.uci.jforestsx.dataset.Feature;
import edu.uci.jforestsx.learning.trees.Ensemble;
import edu.uci.jforestsx.learning.trees.decision.DecisionTree;
import edu.uci.jforestsx.learning.trees.regression.RegressionTree;
import edu.uci.jforestsx.sample.Sample;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class LearningUtils {
	
	public static void updateScores(Sample sampleSet, double[] scores, Ensemble ensemble) {
		updateScores(sampleSet, scores, ensemble, null);
	}
	
	public static void updateScores(Sample sampleSet, double[] scores, Ensemble ensemble, LearningProgressListener progressListener) {
		for (int t = 0; t < ensemble.getNumTrees(); t++) {
			RegressionTree tree = (RegressionTree) ensemble.getTreeAt(t);
			double treeWeight = ensemble.getWeightAt(t);
			//System.out.println("Using tree " + t + " with weight: " + treeWeight); // SISTA
			for (int i = 0; i < sampleSet.size; i++) {
				//System.out.println("Classifying datum #" + i + " with index " + sampleSet.indicesInDataset[i]); // SISTA
				scores[i] += treeWeight * tree.getOutput(sampleSet.dataset, sampleSet.indicesInDataset[i]);
			}
			if (progressListener != null) {
				progressListener.onScoreEval();
			}
		}
	}

  /**
   * Compute the ensemble score for an array of features
   * Indices in the feature array are valid jforests feature indices
   * SISTA added code
   * @param ensemble
   * @param features
   * @return
   */
  public static double computeScore(Ensemble ensemble, Feature[] features) {
    double score = 0.0;
    for (int t = 0; t < ensemble.getNumTrees(); t++) {
      RegressionTree tree = (RegressionTree) ensemble.getTreeAt(t);
      double treeWeight = ensemble.getWeightAt(t);
      score += treeWeight * tree.getOutput(features);
    }
    return score;
  }

	public static void updateScores(Sample sampleSet, double[] scores, RegressionTree tree, double treeWeight) {
		if (sampleSet.indicesInDataset == null) {
			for (int i = 0; i < sampleSet.size; i++) {
				scores[i] += treeWeight * tree.getOutput(sampleSet.dataset, i);
			}	
		} else {
			for (int i = 0; i < sampleSet.size; i++) {
				scores[i] += treeWeight * tree.getOutput(sampleSet.dataset, sampleSet.indicesInDataset[i]);
			}
		}
	}
	
	public static void updateDistributions(Sample sampleSet, double[][] dist, DecisionTree tree, double treeWeight) {
		for (int i = 0; i < sampleSet.size; i++) {
			double[] curDist = tree.getDistributionForInstance(sampleSet.dataset, sampleSet.indicesInDataset[i]);
			for (int c = 0; c < curDist.length; c++) {
				dist[i][c] += treeWeight * curDist[c];
			}
		}
	}

	public static void updateProbabilities(double[] prob, double[] scores, int size) {
		for (int i = 0; i < size; i++) {
			prob[i] = 1.0 / (1.0 + Math.exp(-2.0 * scores[i]));
		}
	}
	
	public static void updateProbabilities(double[] prob, double[] scores, int[] instances, int size) {
		for (int i = 0; i < size; i++) {
			int instance = instances[i];
			prob[instance] = 1.0 / (1.0 + Math.exp(-2.0 * scores[instance]));
		}
	}
	
}
