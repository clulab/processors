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

import edu.uci.jforestsx.dataset.Dataset;
import edu.uci.jforestsx.eval.EvaluationMetric;
import edu.uci.jforestsx.learning.bagging.Bagging;
import edu.uci.jforestsx.sample.Predictions;
import edu.uci.jforestsx.util.ConfigHolder;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class RandomForest extends Bagging {

	protected int numClasses;
	protected DecisionTreeLearner treeLearner;

	public RandomForest() throws Exception {
		super();

		treeLearner = new DecisionTreeLearner();
		this.setSubModule(treeLearner);

	}

	public void init(Dataset dataset, ConfigHolder configHolder, int maxNumTrainInstances, int maxNumValidInstances, EvaluationMetric evaluationMetric)
			throws Exception {
		
		double maxTarget = 0;
		for (int i = 0; i < dataset.numInstances; i++) {
			if (dataset.targets[i] > maxTarget) {
				maxTarget = dataset.targets[i];
			}
		}
		numClasses = ((int) maxTarget) + 1;
		
		super.init(configHolder, maxNumTrainInstances, maxNumValidInstances, evaluationMetric);

		treeLearner.init(numClasses, dataset, configHolder, maxNumTrainInstances);

	}

	@Override
	protected Predictions getNewPredictions() {
		return new DecisionPredictions(numClasses);
	}

}
