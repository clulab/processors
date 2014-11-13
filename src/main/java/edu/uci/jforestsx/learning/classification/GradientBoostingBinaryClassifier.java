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

package edu.uci.jforestsx.learning.classification;

import java.util.Arrays;

import edu.uci.jforestsx.eval.EvaluationMetric;
import edu.uci.jforestsx.learning.LearningUtils;
import edu.uci.jforestsx.learning.boosting.GradientBoosting;
import edu.uci.jforestsx.learning.boosting.GradientBoostingConfig;
import edu.uci.jforestsx.learning.trees.LeafInstances;
import edu.uci.jforestsx.learning.trees.Tree;
import edu.uci.jforestsx.learning.trees.TreeLeafInstances;
import edu.uci.jforestsx.learning.trees.regression.RegressionTree;
import edu.uci.jforestsx.sample.Sample;
import edu.uci.jforestsx.util.ConfigHolder;
import edu.uci.jforestsx.util.Constants;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class GradientBoostingBinaryClassifier extends GradientBoosting {

	protected double[] balancingFactors;

	protected double[] prob;
	protected double[] validProb;

	protected double[] weights;

	private int[] subLearnerSampleIndicesInTrainSet;

	private boolean imbalanceCostAdjustment;

	public GradientBoostingBinaryClassifier() throws Exception {
		super("GradientBoostingBinaryClassifier");
	}

	@Override
	public void init(ConfigHolder configHolder, int maxNumTrainInstances, int maxNumValidInstances, EvaluationMetric evaluationMetric) throws Exception {
		super.init(configHolder, maxNumTrainInstances, maxNumValidInstances, evaluationMetric);
		
		imbalanceCostAdjustment = configHolder.getConfig(GradientBoostingConfig.class).imbalanceCostAdjustment;

		prob = new double[maxNumTrainInstances];
		validProb = new double[maxNumValidInstances];

		weights = new double[maxNumTrainInstances];

		subLearnerSampleIndicesInTrainSet = new int[maxNumTrainInstances];
	}

	@Override
	protected void preprocess() {
		if (balancingFactors == null || balancingFactors.length < curTrainSet.size) {
			balancingFactors = new double[residuals.length];
		}
		int totalPositive = 0;
		int totalNegative = 0;
		for (int i = 0; i < curTrainSet.size; i++) {
			if (curTrainSet.targets[i] == 0) {
				totalNegative++;
			} else {
				totalPositive++;
			}
		}
		if (!imbalanceCostAdjustment) {
			Arrays.fill(balancingFactors, 1.0);
		} else {
			for (int i = 0; i < curTrainSet.size; i++) {
				balancingFactors[i] = (curTrainSet.targets[i] > 0 ? 1.0 / totalPositive : 1.0 / totalNegative);
			}
		}

		// FIXME: use of initial value
		double avg = totalPositive / (totalPositive + totalNegative);
		double initialValue = 0.5 * (Math.log((1 + avg) / (1 - avg)) / Math.log(2));
		Arrays.fill(trainPredictions, 0, curTrainSet.size, initialValue);
		if (curValidSet != null) {
			Arrays.fill(validPredictions, 0, curValidSet.size, initialValue);
		}
	}

	@Override
	protected double getValidMeasurement() throws Exception {
		LearningUtils.updateProbabilities(validProb, validPredictions, curValidSet.size);
		return curValidSet.evaluate(validProb, evaluationMetric);
	}

	@Override
	protected Sample getSubLearnerSample() {
		double responseAbs;
		double target;
		for (int d = 0; d < curTrainSet.size; d++) {
			int instance = curTrainSet.indicesInDataset[d];
			target = (curTrainSet.targets[d] == 0 ? -1 : +1);
			residuals[instance] = (2 * target) / (1 + Math.exp(2 * target * trainPredictions[d]));
			responseAbs = Math.abs(residuals[instance]);
			weights[instance] = responseAbs * (2 - responseAbs);
		}

		Sample subLearnerSample = curTrainSet.getRandomSubSample(samplingRate, rnd).getClone();
		subLearnerSample.targets = residuals;

		for (int i = 0; i < subLearnerSample.size; i++) {
			subLearnerSampleIndicesInTrainSet[i] = subLearnerSample.indicesInParentSample[i];
		}

		return subLearnerSample;
	}

	protected double getAdjustedOutput(LeafInstances leafInstances) {
		double numerator = 0.0;
		double denomerator = 0.0;
		int instance;
		for (int i = leafInstances.begin; i < leafInstances.end; i++) {
			instance = subLearnerSampleIndicesInTrainSet[leafInstances.indices[i]];
			numerator += residuals[instance] * balancingFactors[instance];
			denomerator += weights[instance] * balancingFactors[instance];
		}
		return learningRate * ((numerator + Constants.EPSILON) / (denomerator + Constants.EPSILON));
	}

	@Override
	protected void adjustOutputs(Tree tree, TreeLeafInstances treeLeafInstances) {
		LeafInstances leafInstances = new LeafInstances();
		for (int l = 0; l < tree.numLeaves; l++) {
			treeLeafInstances.loadLeafInstances(l, leafInstances);
			((RegressionTree) tree).setLeafOutput(l, getAdjustedOutput(leafInstances));
		}
	}

	@Override
	protected void postProcessScores() {
		LearningUtils.updateProbabilities(prob, trainPredictions, curTrainSet.size);
	}
}
