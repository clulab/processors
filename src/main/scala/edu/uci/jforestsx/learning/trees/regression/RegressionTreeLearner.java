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

package edu.uci.jforestsx.learning.trees.regression;

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

public class RegressionTreeLearner extends TreeLearner {

	protected double maxLeafOutput;

	public RegressionTreeLearner() {
		super("RegressionTree");		
	}
	
	@Override
	public void init(Dataset dataset, ConfigHolder configHolder, int maxTrainInstances) throws Exception {
		super.init(dataset, configHolder, maxTrainInstances);
		RegressionTreesConfig regressionTreesConfig = configHolder.getConfig(RegressionTreesConfig.class);
		maxLeafOutput = regressionTreesConfig.maxLeafOutput;
	}

	@Override
	protected Tree getNewTree() {
		RegressionTree tree = new RegressionTree();
		tree.init(maxLeaves, maxLeafOutput);
		return tree;
	}

	@Override
	protected TreeSplit getNewSplit() {
		return new RegressionTreeSplit();
	}

	@Override
	protected CandidateSplitsForLeaf getNewCandidateSplitsForLeaf(int numFeatures, int numInstances) {
		return new RegressionCandidateSplitsForLeaf(numFeatures, numInstances);
	}
	
	@Override
	protected Histogram getNewHistogram(Feature f) {
		return new RegressionHistogram(f);
	}

	@Override
	protected void setBestThresholdForSplit(TreeSplit split, Histogram histogram) {
		RegressionHistogram regHistogram = (RegressionHistogram) histogram;
		double bestSumLeftTargets = Double.NaN;
		double bestGain = Double.NEGATIVE_INFINITY;

		double bestWeightedLeftCount = -1;
		int bestThreshold = 0;

		double sumLeftTargets = 0.0;
		int leftCount = 0;
		double weightedLeftCount = 0.0;

		histogram.splittable = false;

		if (randomizedSplits) {
			int minIdx = 0;
			int maxIdx = histogram.numValues - 1;
			for (int t = 0; t < histogram.numValues - 1; t++) {
				sumLeftTargets += regHistogram.perValueSumTargets[t];
				leftCount += histogram.perValueCount[t];

				if (leftCount < minInstancesPerLeaf) {
					minIdx = t;
					continue;
				}

				if ((histogram.totalCount - leftCount) < minInstancesPerLeaf) {
					maxIdx = t + 1;
					break;
				}

				histogram.splittable = true;
			}
			int range = maxIdx - minIdx;
			int randThresholdIdx = minIdx + rand.nextInt(range);

			sumLeftTargets = 0.0;
			leftCount = 0;
			weightedLeftCount = 0.0;

			if (histogram.splittable) {
				for (int t = 0; t < randThresholdIdx; t++) {
					sumLeftTargets += regHistogram.perValueSumTargets[t];
					leftCount += histogram.perValueCount[t];
					weightedLeftCount += histogram.perValueWeightedCount[t];
				}

				double weightedRightCount = histogram.totalWeightedCount - weightedLeftCount;

				double sumRightTargets = regHistogram.sumTargets - sumLeftTargets;
				double currentGain = (sumLeftTargets * sumLeftTargets) / weightedLeftCount + (sumRightTargets * sumRightTargets) / weightedRightCount;

				if (currentGain > bestGain) {
					bestWeightedLeftCount = weightedLeftCount;
					bestSumLeftTargets = sumLeftTargets;
					bestThreshold = randThresholdIdx;
					bestGain = currentGain;
				}
			}
		} else {
			for (int t = 0; t < histogram.numValues - 1; t++) {
				leftCount += histogram.perValueCount[t];
				weightedLeftCount += histogram.perValueWeightedCount[t];
				sumLeftTargets += regHistogram.perValueSumTargets[t];

				if (leftCount < minInstancesPerLeaf || leftCount == 0) {
					continue;
				}
				int rightCount = histogram.totalCount - leftCount;

				if (rightCount < minInstancesPerLeaf || rightCount == 0) {
					break;
				}

				histogram.splittable = true;

				double weightedRightCount = histogram.totalWeightedCount - weightedLeftCount;

				double sumRightTargets = regHistogram.sumTargets - sumLeftTargets;
				double currentGain = (sumLeftTargets * sumLeftTargets) / weightedLeftCount + (sumRightTargets * sumRightTargets) / weightedRightCount;

				if (currentGain > bestGain) {
					bestWeightedLeftCount = weightedLeftCount;
					bestSumLeftTargets = sumLeftTargets;
					bestThreshold = t;
					bestGain = currentGain;
				}
			}
		}

		Feature feature = curTrainSet.dataset.features[split.feature];
		split.threshold = feature.upperBounds[bestThreshold];
		split.originalThreshold = feature.getOriginalValue(split.threshold);
		
		RegressionTreeSplit regressionSplit = (RegressionTreeSplit) split;
		regressionSplit.leftOutput = bestSumLeftTargets / bestWeightedLeftCount;
		regressionSplit.rightOutput = (regHistogram.sumTargets - bestSumLeftTargets) / (histogram.totalWeightedCount - bestWeightedLeftCount);

		split.gain = bestGain - (regHistogram.sumTargets * regHistogram.sumTargets) / histogram.totalWeightedCount;
	}
}
