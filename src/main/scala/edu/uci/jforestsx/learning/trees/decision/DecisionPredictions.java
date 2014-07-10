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

import edu.uci.jforestsx.eval.EvaluationMetric;
import edu.uci.jforestsx.learning.LearningUtils;
import edu.uci.jforestsx.learning.trees.Tree;
import edu.uci.jforestsx.sample.Predictions;
import edu.uci.jforestsx.util.ArraysUtil;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class DecisionPredictions extends Predictions {
	
	protected double[][] perInstanceDistribution;
	protected double[] perInstancePredictions;
	protected int numClasses;
	
	public DecisionPredictions(int numClasses) {
		this.numClasses = numClasses;
	}

	@Override
	public void allocate(int maxNumInstances) {
		perInstanceDistribution = new double[maxNumInstances][numClasses];
		perInstancePredictions = new double[maxNumInstances];
		for (int i = 0; i < maxNumInstances; i++) {
			perInstanceDistribution[i] = new double[numClasses];
		}
	}

	@Override
	public void update(Tree tree, double weight) {
		LearningUtils.updateDistributions(sample, perInstanceDistribution, (DecisionTree) tree, weight); 
		
	}

	@Override
	public double evaluate(EvaluationMetric evalMetric) throws Exception {
		if (numClasses == 2) {
			for (int i = 0; i < sample.size; i++) {
				perInstancePredictions[i] = perInstanceDistribution[i][1] / (perInstanceDistribution[i][0] + perInstanceDistribution[i][1]);
			}	
		} else {
			for (int i = 0; i < sample.size; i++) {
				perInstancePredictions[i] = ArraysUtil.findMaxIndex(perInstanceDistribution[i]);
			}
		}
		return sample.evaluate(perInstancePredictions, evalMetric);
	}

	@Override
	public void reset() {
	 	Arrays.fill(perInstancePredictions, 0);
	}

}
