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

import edu.uci.jforestsx.eval.EvaluationMetric;
import edu.uci.jforestsx.learning.trees.Ensemble;
import edu.uci.jforestsx.learning.trees.Tree;
import edu.uci.jforestsx.learning.trees.TreeLeafInstances;
import edu.uci.jforestsx.sample.Sample;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public abstract class LearningModule {

	protected String algorithmName;
	protected int moduleLevel = 1;

	protected LearningModule subLearner;
	protected LearningModule parentLearner;
	protected double treeWeight = 1.0;
	protected EvaluationMetric evaluationMetric;
	protected LearningProgressListener progressListener;

	public LearningModule(String algorithmName) {
		this.algorithmName = algorithmName;
	}

	public void setProgressListener(LearningProgressListener progressListener) {
		this.progressListener = progressListener;
	}

	public abstract Ensemble learn(Sample trainSet, Sample validSet) throws Exception;

	public abstract double getValidationMeasurement() throws Exception;

	/**
	 * @param tree
	 * @param treeLeafInstances
	 */
	public void postProcess(Tree tree, TreeLeafInstances treeLeafInstances) {
		// Subclasses can override this method if needed.
	}

	public void setSubModule(LearningModule subModule) {
		this.subLearner = subModule;
		this.subLearner.setParentModule(this);
		this.subLearner.moduleLevel = this.moduleLevel + 1;
	}

	public void setParentModule(LearningModule parentModule) {
		this.parentLearner = parentModule;
	}

	public void setTreeWeight(double treeWeight) {
		this.treeWeight = treeWeight;
	}

	public void setAlgorithmName(String name) {
		this.algorithmName = name;
	}

	private double bestPrintedValidMeasurement = Double.NaN;

	public void printTrainAndValidMeasurement(int iteration, double validMeasurement, double trainMeasurement, EvaluationMetric evaluationMetric) {
		if (evaluationMetric.isFirstBetter(validMeasurement, bestPrintedValidMeasurement, 0)) {
			bestPrintedValidMeasurement = validMeasurement;
		}
		if (moduleLevel > 1) {
			return;
		}
		for (int i = 0; i < moduleLevel - 1; i++) {
			System.out.print("\t");
		}
		if (Double.isNaN(trainMeasurement)) {
			System.out.println(algorithmName + ": [Iteration: " + iteration + ", Valid: " + validMeasurement + ", Best: " + bestPrintedValidMeasurement
					+ "]");
		} else {
			System.out.println(iteration + "\t" + validMeasurement + "\t" + trainMeasurement);
		}
	}

	public void printValidMeasurement(int iteration, double validMeasurement, EvaluationMetric evaluationMetric) {
		printTrainAndValidMeasurement(iteration, validMeasurement, Double.NaN, evaluationMetric);
	}

	protected void onIterationEnd() {
		if (progressListener != null) {
			progressListener.onIterationEnd();
		}
	}

	protected void onLearningEnd() {
	}
}
