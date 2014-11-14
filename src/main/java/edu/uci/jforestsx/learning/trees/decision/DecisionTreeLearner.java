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

package edu.uci.jforestsx.learning.trees.decision;

import java.util.Arrays;

import edu.uci.jforestsx.dataset.Dataset;
import edu.uci.jforestsx.dataset.Feature;
import edu.uci.jforestsx.dataset.Histogram;
import edu.uci.jforestsx.learning.trees.CandidateSplitsForLeaf;
import edu.uci.jforestsx.learning.trees.Tree;
import edu.uci.jforestsx.learning.trees.TreeLearner;
import edu.uci.jforestsx.learning.trees.TreeSplit;
import edu.uci.jforestsx.util.ConfigHolder;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class DecisionTreeLearner extends TreeLearner {

	protected int numClasses;

	public DecisionTreeLearner() {
		super("DecisionTree");
	}
	
	public void init(int numClasses, Dataset dataset, ConfigHolder configHolder, int maxTrainInstances) throws Exception {
		this.numClasses = numClasses;
		super.init(dataset, configHolder, maxTrainInstances);
	}

	@Override
	protected Tree getNewTree() {
		DecisionTree tree = new DecisionTree();
		tree.init(maxLeaves, numClasses);
		return tree;
	}

	@Override
	protected TreeSplit getNewSplit() {
		return new DecisionTreeSplit(numClasses);
	}

	@Override
	protected CandidateSplitsForLeaf getNewCandidateSplitsForLeaf(int numFeatures, int numInstances) {
		return new DecisionCandidateSplitsForLeaf(numFeatures, numInstances, numClasses);
	}

	@Override
	protected Histogram getNewHistogram(Feature f) {
		return new DecisionHistogram(f, numClasses);
	}

	@Override
	protected void setBestThresholdForSplit(TreeSplit split, Histogram histogram) {
		
		DecisionHistogram decisionHistogram = (DecisionHistogram) histogram;
		DecisionTreeSplit decisionSplit = (DecisionTreeSplit) split;
		
		/*
		 * Check for pure nodes
		 */
		int nonZeroClassCount = 0;
		for (int c = 0; c < numClasses; c++) {
			if (decisionHistogram.targetDist[c] > 0) {
				nonZeroClassCount++;
			}
		}
		if (nonZeroClassCount == 1) {
			// This is a pure node. So, there is no gain in splitting it.
			split.gain = 0;
			return;
		}
		
		double minSplitEntropy = Double.POSITIVE_INFINITY;
		int bestThreshold = 0;
		int leftCount = 0;

		histogram.splittable = false;

		double[] leftTargetDist = new double[numClasses];
		double[] rightTargetDist = new double[numClasses];
		System.arraycopy(decisionHistogram.targetDist, 0, rightTargetDist, 0, numClasses);
		
		Arrays.fill(decisionSplit.leftTargetDist, 0);

		for (int t = 0; t < histogram.numValues - 1; t++) {
			leftCount += histogram.perValueCount[t];
			for (int c = 0; c < numClasses; c++) {
				leftTargetDist[c] += decisionHistogram.perValueTargetDist[t][c];
				rightTargetDist[c] -= decisionHistogram.perValueTargetDist[t][c];
			}

			if (leftCount < minInstancesPerLeaf || leftCount == 0) {
				continue;
			}
			int rightCount = histogram.totalCount - leftCount;

			if (rightCount < minInstancesPerLeaf || rightCount == 0) {
				break;
			}

			histogram.splittable = true;

			double currentSplitEntropy = Entropy.getSplitEntropy(leftTargetDist, rightTargetDist);

			if (currentSplitEntropy < minSplitEntropy) {
				bestThreshold = t;
				minSplitEntropy = currentSplitEntropy;
				for (int c = 0; c < numClasses; c++) {
					decisionSplit.leftTargetDist[c] = leftTargetDist[c];
				}
			}
		}

		Feature feature = curTrainSet.dataset.features[split.feature];
		split.threshold = feature.upperBounds[bestThreshold];
		split.originalThreshold = feature.getOriginalValue(split.threshold);

		for (int c = 0; c < numClasses; c++) {			
			decisionSplit.rightTargetDist[c] = decisionHistogram.targetDist[c] - decisionSplit.leftTargetDist[c];
		}
		
		if (Double.isInfinite(minSplitEntropy)) {
			split.gain = Double.NEGATIVE_INFINITY;
		} else {
			split.gain = Entropy.getEntropy(decisionHistogram.targetDist) - minSplitEntropy;
		}
	}

}
