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

package edu.uci.jforestsx.learning.trees;

import java.util.Random;

import edu.uci.jforestsx.sample.Sample;
import edu.uci.jforestsx.util.ArraysUtil;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public abstract class CandidateSplitsForLeaf {

	protected int leafIdx;
	protected int numInstancesInLeaf;

	protected double totalWeightedCount;

	protected int[] indices;
	protected double[] targets;
	protected double[] weights;
	protected TreeSplit[] bestSplitPerFeature;
	protected int[] tempIndices;

	public abstract void init(int curLeafIndex, TreeLeafInstances treeLeafInstances, Sample trainSet);

	public CandidateSplitsForLeaf(int numFeatures, int numInstances) {
		tempIndices = new int[numFeatures];
		for (int f = 0; f < numFeatures; f++) {
			tempIndices[f] = f;
		}
		indices = new int[numInstances];
		targets = new double[numInstances];
		weights = new double[numInstances];
	}

	public TreeSplit getFeatureSplit(int f) {
		return bestSplitPerFeature[f];
	}

	public int getBestFeature() {
		int maxIndex = -1;
		double maxGain = Double.NEGATIVE_INFINITY;
		for (int f = 0; f < bestSplitPerFeature.length; f++) {
			if (bestSplitPerFeature[f].gain > maxGain) {
				maxGain = bestSplitPerFeature[f].gain;
				maxIndex = f;
			}
		}
		return maxIndex;
	}

	public int getBestFeature(double splitFraction, Random rnd) {
		int maxIndex = -1;
		double maxGain = Double.NEGATIVE_INFINITY;
		ArraysUtil.shuffle(tempIndices, rnd);
		int maxFeaturesToConsider = Math.max((int) (bestSplitPerFeature.length * splitFraction), 1);
		int featuresConsidered = 0;
		for (int i = 0; i < bestSplitPerFeature.length && featuresConsidered < maxFeaturesToConsider; i++) {
			int f = tempIndices[i];
			if (!Double.isInfinite(bestSplitPerFeature[f].gain)) {
				featuresConsidered++;
				if (bestSplitPerFeature[f].gain > maxGain) {
					maxGain = bestSplitPerFeature[f].gain;
					maxIndex = f;
				}
			}
		}
		return maxIndex;
	}

	public int getLeafIndex() {
		return leafIdx;
	}

	public int getNumInstancesInLeaf() {
		return numInstancesInLeaf;
	}

	public double getTotalWeightedCount() {
		return totalWeightedCount;
	}

	public int[] getIndices() {
		return indices;
	}

	public double[] getTargets() {
		return targets;
	}

	public double[] getWeights() {
		return weights;
	}

	public void init(int curLeafIndex) {
		this.leafIdx = curLeafIndex;
		for (int f = 0; f < bestSplitPerFeature.length; f++) {
			bestSplitPerFeature[f].feature = f;
			bestSplitPerFeature[f].gain = Double.NEGATIVE_INFINITY;
		}
	}

}
