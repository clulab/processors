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

package edu.uci.jforestsx.learning.bagging;

import java.util.Random;

import edu.uci.jforestsx.config.TrainingConfig;
import edu.uci.jforestsx.eval.EvaluationMetric;
import edu.uci.jforestsx.learning.LearningModule;
import edu.uci.jforestsx.learning.trees.Ensemble;
import edu.uci.jforestsx.learning.trees.Tree;
import edu.uci.jforestsx.learning.trees.TreeLeafInstances;
import edu.uci.jforestsx.sample.Predictions;
import edu.uci.jforestsx.sample.Sample;
import edu.uci.jforestsx.util.ConfigHolder;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public abstract class Bagging extends LearningModule {

	protected int bagCount;
	protected double baggingTrainFraction;
	protected boolean backfit;

	protected Sample curValidSet;
	protected double lastValidMeasurement;
	protected Predictions validPredictions;

	protected boolean printIntermediateValidMeasurements;
	protected Random rnd;
	protected EvaluationMetric evaluationMetric;
	
	public Bagging() {
		super("Bagging");
	}

	public void init(ConfigHolder configHolder, int maxNumTrainInstances, int maxNumValidInstances, EvaluationMetric evaluationMetric)
			throws Exception {
		TrainingConfig trainingConfig = configHolder.getConfig(TrainingConfig.class);
		BaggingConfig baggingConfig = configHolder.getConfig(BaggingConfig.class);
		
		this.bagCount = baggingConfig.bagCount;
		this.baggingTrainFraction = baggingConfig.trainFraction;
		this.backfit = baggingConfig.backfitting;

		validPredictions = getNewPredictions();
		validPredictions.allocate(maxNumValidInstances);

		printIntermediateValidMeasurements = configHolder.getConfig(TrainingConfig.class).printIntermediateValidMeasurements;
		this.evaluationMetric = evaluationMetric;
		rnd = new Random(trainingConfig.randomSeed);
	}

	protected abstract Predictions getNewPredictions();

	@Override
	public Ensemble learn(Sample trainSet, Sample validSet) throws Exception {

		curValidSet = validSet;
		validPredictions.setSample(curValidSet);
		validPredictions.reset();

		Ensemble ensemble = new Ensemble();
		subLearner.setTreeWeight(treeWeight / bagCount);
		for (int iteration = 1; iteration <= bagCount; iteration++) {
			System.out.println("Iteration: " + iteration);
			Sample subLearnerTrainSet = trainSet.getRandomSubSample(baggingTrainFraction, rnd);
			//((TreeLearner) subLearner).setRnd();
			Sample subLearnerOutOfTrainSet = trainSet.getOutOfSample(subLearnerTrainSet);
			Sample subLearnerValidSet = (validSet == null || validSet.isEmpty() ? subLearnerOutOfTrainSet : validSet);
			Ensemble subEnsemble = subLearner.learn(subLearnerTrainSet, subLearnerValidSet);

			for (int t = 0; t < subEnsemble.getNumTrees(); t++) {
				Tree tree = subEnsemble.getTreeAt(t);
				double curTreeWeight = subEnsemble.getWeightAt(t);
				if (backfit) {
					tree.backfit(subLearnerOutOfTrainSet);
				}
				ensemble.addTree(tree, curTreeWeight);
				System.out.println(tree.numLeaves);
			}

			if (validSet != null && !validSet.isEmpty()) {
				for (int t = 0; t < subEnsemble.getNumTrees(); t++) {
					validPredictions.update(subEnsemble.getTreeAt(t), 1.0 / bagCount);
				}
				lastValidMeasurement = validPredictions.evaluate(evaluationMetric);
				if (printIntermediateValidMeasurements) {
					printValidMeasurement(iteration, lastValidMeasurement, evaluationMetric);
				}
			}
			onIterationEnd();
		}

		onLearningEnd();
		return ensemble;
	}

	@Override
	public void postProcess(Tree tree, TreeLeafInstances treeLeafInstances) {
		if (parentLearner != null) {
			parentLearner.postProcess(tree, treeLeafInstances);
		}
	}

	@Override
	public double getValidationMeasurement() throws Exception {
		return lastValidMeasurement;
	}
}
