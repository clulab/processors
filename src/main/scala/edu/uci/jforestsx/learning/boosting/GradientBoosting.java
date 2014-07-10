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

package edu.uci.jforestsx.learning.boosting;

import java.util.Arrays;
import java.util.Random;

import edu.uci.jforestsx.config.TrainingConfig;
import edu.uci.jforestsx.eval.EvaluationMetric;
import edu.uci.jforestsx.learning.LearningModule;
import edu.uci.jforestsx.learning.LearningUtils;
import edu.uci.jforestsx.learning.trees.Ensemble;
import edu.uci.jforestsx.learning.trees.Tree;
import edu.uci.jforestsx.learning.trees.TreeLeafInstances;
import edu.uci.jforestsx.learning.trees.regression.RegressionTree;
import edu.uci.jforestsx.sample.Sample;
import edu.uci.jforestsx.util.ConfigHolder;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class GradientBoosting extends LearningModule {

	protected double[] trainPredictions;
	protected double[] validPredictions;

	protected double[] residuals;

	protected int numInstances;

	private int numSubModules;
	protected double learningRate;
	protected double samplingRate;
	protected double earlyStoppingTolerance;

	protected Sample curTrainSet;
	protected Sample curValidSet;
	protected int curIteration;

	protected double bestValidationMeasurement;
	protected boolean printIntermediateValidMeasurements;
	protected EvaluationMetric evaluationMetric;

	protected Random rnd;

	public GradientBoosting(String algorithmName) {
		super(algorithmName);
	}
	
	public GradientBoosting() {
		super("GradientBoosting");
	}

	public void init(ConfigHolder configHolder, int maxNumTrainInstances, int maxNumValidInstances, EvaluationMetric evaluationMetric) throws Exception {
		this.evaluationMetric = evaluationMetric;
		GradientBoostingConfig gradientBoostingConfig = configHolder.getConfig(GradientBoostingConfig.class);
		this.numSubModules = gradientBoostingConfig.numTrees;
		this.learningRate = gradientBoostingConfig.learningRate;
		this.samplingRate = gradientBoostingConfig.samplingRate;
		this.earlyStoppingTolerance = gradientBoostingConfig.earlyStoppingTolerance;

		trainPredictions = new double[maxNumTrainInstances];
		residuals = new double[maxNumTrainInstances];
		validPredictions = new double[maxNumValidInstances];

		TrainingConfig trainingConfig = configHolder.getConfig(TrainingConfig.class);
		printIntermediateValidMeasurements = trainingConfig.printIntermediateValidMeasurements;
		rnd = new Random(trainingConfig.randomSeed);
	}

	protected void preprocess() {
		Arrays.fill(trainPredictions, 0, curTrainSet.size, 0);
		Arrays.fill(validPredictions, 0, curValidSet.size, 0);
	}

	@Override
	public Ensemble learn(Sample trainSet, Sample validSet) throws Exception {
		curTrainSet = trainSet;
		curValidSet = validSet;

		preprocess();

		Ensemble ensemble = new Ensemble();
		bestValidationMeasurement = Double.NaN;
		int earlyStoppingIteration = 0;
		int bestIteration = 0;
		int[] treeCounts = new int[numSubModules];
		subLearner.setTreeWeight(treeWeight);
		for (curIteration = 1; curIteration <= numSubModules; curIteration++) {
			Sample subLeanerSample = getSubLearnerSample();
			Ensemble subEnsemble = subLearner.learn(subLeanerSample, validSet);
			if (subEnsemble == null) {
				break;
			}
			for (int t = 0; t < subEnsemble.getNumTrees(); t++) {
				Tree tree = subEnsemble.getTreeAt(t);
				ensemble.addTree(tree, subEnsemble.getWeightAt(t));
				if (validSet != null) {
					LearningUtils.updateScores(validSet, validPredictions, ((RegressionTree) tree), 1.0);
				}
			}
			treeCounts[curIteration - 1] = ensemble.getNumTrees();

			if (validSet == null) {
				earlyStoppingIteration = curIteration;
			} else {
				double validMeasurement = getValidMeasurement();
				if (evaluationMetric.isFirstBetter(validMeasurement, bestValidationMeasurement, earlyStoppingTolerance)) {
					earlyStoppingIteration = curIteration;
					if (evaluationMetric.isFirstBetter(validMeasurement, bestValidationMeasurement, 0)) {
						bestValidationMeasurement = validMeasurement;
						bestIteration = curIteration;
					}
				}

				// FIXME
				if (curIteration - bestIteration > 100) {
					break;
				}

				if (printIntermediateValidMeasurements) {
					printTrainAndValidMeasurement(curIteration, validMeasurement, getTrainMeasurement(), evaluationMetric);
				}
			}
			onIterationEnd();
		}

		if (earlyStoppingIteration > 0) {
			int treesToKeep = treeCounts[earlyStoppingIteration - 1];
			int treesToDelete = ensemble.getNumTrees() - treesToKeep;
			ensemble.removeLastTrees(treesToDelete);
		}

		onLearningEnd();
		return ensemble;
	}

	@Override
	public double getValidationMeasurement() {
		return bestValidationMeasurement;
	}

	protected double getValidMeasurement() throws Exception {
		return curValidSet.evaluate(validPredictions, evaluationMetric);
	}

	protected double getTrainMeasurement() throws Exception {
		return curTrainSet.evaluate(trainPredictions, evaluationMetric);
	}

	protected Sample getSubLearnerSample() {
		for (int i = 0; i < curTrainSet.size; i++) {
			residuals[i] = curTrainSet.targets[i] - trainPredictions[i];
		}
		Sample subLearnerSample = curTrainSet.getClone();
		subLearnerSample.targets = residuals;
		subLearnerSample = subLearnerSample.getRandomSubSample(samplingRate, rnd);
		return subLearnerSample;
	}

	protected void adjustOutputs(Tree tree, TreeLeafInstances treeLeafInstances) {
		((RegressionTree) tree).multiplyLeafOutputs(learningRate);
	}

	@Override
	public void postProcess(Tree tree, TreeLeafInstances treeLeafInstances) {
		adjustOutputs(tree, treeLeafInstances);
		LearningUtils.updateScores(curTrainSet, trainPredictions, ((RegressionTree) tree), 1.0);
		postProcessScores();
	}

	protected void postProcessScores() {
		// Do nothing
	}

}
