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

import java.util.Arrays;
import java.util.Random;

import edu.uci.jforestsx.config.TrainingConfig;
import edu.uci.jforestsx.dataset.Dataset;
import edu.uci.jforestsx.dataset.Feature;
import edu.uci.jforestsx.dataset.Histogram;
import edu.uci.jforestsx.learning.LearningModule;
import edu.uci.jforestsx.sample.Sample;
import edu.uci.jforestsx.util.ConfigHolder;
import edu.uci.jforestsx.util.concurrency.BlockingThreadPoolExecutor;
import edu.uci.jforestsx.util.concurrency.TaskCollection;
import edu.uci.jforestsx.util.concurrency.TaskItem;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public abstract class TreeLearner extends LearningModule {

	protected double featureSamplingPerSplit;
	protected boolean randomizedSplits;

	protected double minInstancePercentagePerLeaf;
	protected int minInstancesPerLeaf;
	protected int maxLeaves;

	protected boolean[] selectedFeatures;
	protected boolean[] featuresToDiscard;

	protected Random rand;

	private TreeLeafInstances trainTreeLeafInstances;
	protected Sample curTrainSet;

	private Histogram[][] perNodeHistograms;
	protected TreeSplit[] perLeafBestSplit;

	private int parentNodeIndex;
	private int smallerChildIndex;
	private int largerChildIndex;

	private CandidateSplitsForLeaf candidateSplitsForSmallerChild;
	private CandidateSplitsForLeaf candidateSplitsForLargerChild;

	private TaskCollection<BestThresholdForFeatureFinder> leafCandidateSplitsCalculationTask;

	private final static int ROOT_LEAF_INDEX = 0;

	public TreeLearner(String algorithmName) {
		super(algorithmName);
	}

	public void init(Dataset dataset, ConfigHolder configHolder, int maxTrainInstances) throws Exception {
		TrainingConfig trainingConfig = configHolder.getConfig(TrainingConfig.class);
		TreesConfig treesConfig = configHolder.getConfig(TreesConfig.class);

		minInstancePercentagePerLeaf = treesConfig.minInstancePercentagePerLeaf;
		maxLeaves = treesConfig.numLeaves;

		perLeafBestSplit = new TreeSplit[treesConfig.numLeaves];

		leafCandidateSplitsCalculationTask = new TaskCollection<BestThresholdForFeatureFinder>();
		int chunkSize = 1 + (dataset.numFeatures / BlockingThreadPoolExecutor.getInstance().getMaximumPoolSize());
		int offset = 0;
		for (int i = 0; offset < dataset.numFeatures; i++) {
			int endOffset = offset + Math.min(dataset.numFeatures - offset, chunkSize);
			leafCandidateSplitsCalculationTask.addTask(new BestThresholdForFeatureFinder(offset, endOffset));
			offset += chunkSize;
		}

		perNodeHistograms = new Histogram[treesConfig.numLeaves][];

		candidateSplitsForSmallerChild = getNewCandidateSplitsForLeaf(dataset.numFeatures, maxTrainInstances);
		candidateSplitsForLargerChild = getNewCandidateSplitsForLeaf(dataset.numFeatures, maxTrainInstances);

		rand = new Random(trainingConfig.randomSeed);

		featureSamplingPerSplit = treesConfig.featureSamplingPerSplit;
		randomizedSplits = treesConfig.randomizedSplits;
		selectedFeatures = new boolean[dataset.numFeatures];
		trainTreeLeafInstances = new TreeLeafInstances(maxTrainInstances, maxLeaves);

		featuresToDiscard = new boolean[dataset.numFeatures];
		String featuresToIncludeString = treesConfig.featuresToInclude;
		if (featuresToIncludeString != null && featuresToIncludeString.trim().length() > 0) {
			Arrays.fill(featuresToDiscard, true);
			String[] featureNamesToInclude = featuresToIncludeString.split(",");
			for (String featureNameToInclude : featureNamesToInclude) {
				int fidx = dataset.getFeatureIdx(featureNameToInclude);
				if (fidx < 0) {
					throw new Exception("Unknown feature: '" + featureNameToInclude + "'");
				}
				featuresToDiscard[fidx] = false;
			}
		}
		
		String featuresToDiscardString = treesConfig.featuresToDiscard;
		if (featuresToDiscardString != null && featuresToDiscardString.trim().length() > 0) {
			String[] featureNamesToDiscard = featuresToDiscardString.split(",");
			for (String featureNameToDiscard : featureNamesToDiscard) {
				int fidx = dataset.getFeatureIdx(featureNameToDiscard);
				if (fidx < 0) {
					throw new Exception("Unknown feature: '" + featureNameToDiscard + "'");
				}
				featuresToDiscard[fidx] = true;
			}
		}
	}
	
	public void setRnd() {
		rand = new Random(1);
	}

	protected abstract Tree getNewTree();

	protected abstract TreeSplit getNewSplit();

	protected abstract CandidateSplitsForLeaf getNewCandidateSplitsForLeaf(int numFeatures, int numInstances);

	protected abstract Histogram getNewHistogram(Feature f);

	@Override
	public Ensemble learn(Sample trainSet, Sample validSet) throws Exception {
		curTrainSet = trainSet;
		trainTreeLeafInstances.init(curTrainSet.size);
		minInstancesPerLeaf = (int) (curTrainSet.size * minInstancePercentagePerLeaf / 100.0);
		for (int i = 0; i < selectedFeatures.length; i++) {
			selectedFeatures[i] = !featuresToDiscard[i];
		}

		for (int i = 0; i < perNodeHistograms.length; i++) {
			if (perNodeHistograms[i] != null) {
				for (int f = 0; f < perNodeHistograms[0].length; f++) {
					Histogram dist = perNodeHistograms[i][f];
					if (dist != null) {
						dist.splittable = true;
					}
				}
			}
		}

		Tree tree = getNewTree();

		candidateSplitsForSmallerChild.init(ROOT_LEAF_INDEX, trainTreeLeafInstances, curTrainSet);
		parentNodeIndex = -1;
		smallerChildIndex = ROOT_LEAF_INDEX;
		if (perNodeHistograms[ROOT_LEAF_INDEX] == null) {
			perNodeHistograms[ROOT_LEAF_INDEX] = getNewHistogramArray();
		}
		candidateSplitsForLargerChild.init(-1);
		leafCandidateSplitsCalculationTask.run();
		setBestTreeSplitForLeaf(candidateSplitsForSmallerChild);

		TreeSplit rootSplit = perLeafBestSplit[ROOT_LEAF_INDEX];
		if (Double.isInfinite(rootSplit.gain)) {
			return null;
		}

		int newInteriorNodeIndex = tree.split(ROOT_LEAF_INDEX, rootSplit);
		int rightChild = ~tree.getRightChild(newInteriorNodeIndex);
		int leftChild = ROOT_LEAF_INDEX;

		int bestLeaf = ROOT_LEAF_INDEX;
		parentNodeIndex = ROOT_LEAF_INDEX;
		trainTreeLeafInstances.split(bestLeaf, curTrainSet.dataset, rootSplit.feature, rootSplit.threshold, rightChild, curTrainSet.indicesInDataset);
		for (int k = 2; k < maxLeaves; k++) {

			int numInstancesInLeftChild = trainTreeLeafInstances.getNumberOfInstancesInLeaf(leftChild);
			int numInstancesInRightChild = trainTreeLeafInstances.getNumberOfInstancesInLeaf(rightChild);

			if (numInstancesInRightChild >= 2 * minInstancesPerLeaf || numInstancesInLeftChild >= 2 * minInstancesPerLeaf) {

				if (numInstancesInLeftChild < numInstancesInRightChild) {

					Histogram[] tmpDist = perNodeHistograms[rightChild];
					perNodeHistograms[rightChild] = perNodeHistograms[leftChild];
					if (tmpDist != null) {
						perNodeHistograms[leftChild] = tmpDist;
					} else {
						perNodeHistograms[leftChild] = getNewHistogramArray();
					}

					largerChildIndex = rightChild;
					smallerChildIndex = leftChild;

				} else {

					if (perNodeHistograms[rightChild] == null) {
						perNodeHistograms[rightChild] = getNewHistogramArray();
					}

					largerChildIndex = leftChild;
					smallerChildIndex = rightChild;

				}

				candidateSplitsForSmallerChild.init(smallerChildIndex, trainTreeLeafInstances, curTrainSet);
				candidateSplitsForLargerChild.init(largerChildIndex, trainTreeLeafInstances, curTrainSet);

				leafCandidateSplitsCalculationTask.run();

				setBestTreeSplitForLeaf(candidateSplitsForSmallerChild);
				setBestTreeSplitForLeaf(candidateSplitsForLargerChild);

			} else {
				perLeafBestSplit[leftChild].gain = Double.NEGATIVE_INFINITY;
				perLeafBestSplit[rightChild] = getNewSplit();
				perLeafBestSplit[rightChild].gain = Double.NEGATIVE_INFINITY;
			}

			bestLeaf = 0;
			double maxGain = Double.NEGATIVE_INFINITY;
			for (int i = 0; i < tree.numLeaves; i++) {
				if (perLeafBestSplit[i].gain > maxGain) {
					maxGain = perLeafBestSplit[i].gain;
					bestLeaf = i;
				}
			}
			TreeSplit bestLeafSplit = perLeafBestSplit[bestLeaf];

			if (bestLeafSplit.gain <= 0 || Double.isNaN(bestLeafSplit.gain)) {
				break;
			}

			newInteriorNodeIndex = tree.split(bestLeaf, bestLeafSplit);
			leftChild = bestLeaf;
			rightChild = ~tree.getRightChild(newInteriorNodeIndex);
			parentNodeIndex = bestLeaf;

			trainTreeLeafInstances.split(bestLeaf, curTrainSet.dataset, bestLeafSplit.feature, bestLeafSplit.threshold, rightChild,
					curTrainSet.indicesInDataset);

		}

		if (parentLearner != null) {
			parentLearner.postProcess(tree, trainTreeLeafInstances);
		}

		Ensemble ensemble = new Ensemble();
		ensemble.addTree(tree, treeWeight);
		return ensemble;
	}

	protected void setBestTreeSplitForLeaf(CandidateSplitsForLeaf leafSplitCandidates) {
		int bestFeature;
		if (featureSamplingPerSplit < 1.0) {
			bestFeature = leafSplitCandidates.getBestFeature(featureSamplingPerSplit, rand);
		} else {
			bestFeature = leafSplitCandidates.getBestFeature();
		}

		int leaf = leafSplitCandidates.getLeafIndex();

		if (perLeafBestSplit[leaf] == null) {
			perLeafBestSplit[leaf] = getNewSplit();
		}

		if (bestFeature < 0) {
			perLeafBestSplit[leaf].copy(leafSplitCandidates.getFeatureSplit(0));
			perLeafBestSplit[leaf].gain = Double.NEGATIVE_INFINITY;
		} else {
			perLeafBestSplit[leaf].copy(leafSplitCandidates.getFeatureSplit(bestFeature));
		}
	}

	private class BestThresholdForFeatureFinder extends TaskItem {

		private int beginIdx;
		private int endIdx;

		public BestThresholdForFeatureFinder(int beginIdx, int endIdx) {
			this.beginIdx = beginIdx;
			this.endIdx = endIdx;
		}

		@Override
		public void run() {
			for (int f = beginIdx; f < endIdx; f++) {
				if (!selectedFeatures[f]) {
					continue;
				}

				if (parentNodeIndex != -1 && !perNodeHistograms[parentNodeIndex][f].splittable) {
					perNodeHistograms[smallerChildIndex][f].splittable = false;
					continue;
				}

				perNodeHistograms[smallerChildIndex][f].init(candidateSplitsForSmallerChild, curTrainSet.indicesInDataset);
				setBestThresholdForSplit(candidateSplitsForSmallerChild.getFeatureSplit(f), perNodeHistograms[smallerChildIndex][f]);

				if (parentNodeIndex != -1) {
					try {
						perNodeHistograms[largerChildIndex][f].subtractFromMe(perNodeHistograms[smallerChildIndex][f]);
						setBestThresholdForSplit(candidateSplitsForLargerChild.getFeatureSplit(f), perNodeHistograms[largerChildIndex][f]);
					} catch (Exception e) {
						e.printStackTrace();
					}
				}
			}
		}
	}

	protected abstract void setBestThresholdForSplit(TreeSplit split, Histogram histogram);

	@Override
	public double getValidationMeasurement() throws Exception {
		throw new Exception("Validation Measurement should not be computed for TreeLearner.");
	}

	private Histogram[] getNewHistogramArray() {
		Histogram[] result = new Histogram[curTrainSet.dataset.numFeatures];
		for (int j = 0; j < curTrainSet.dataset.numFeatures; j++) {
			result[j] = getNewHistogram(curTrainSet.dataset.features[j]);
		}
		return result;
	}
}
