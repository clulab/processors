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

import java.util.Arrays;

import edu.uci.jforestsx.eval.EvaluationMetric;
import edu.uci.jforestsx.learning.LearningUtils;
import edu.uci.jforestsx.learning.trees.Tree;
import edu.uci.jforestsx.sample.Predictions;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class RegressionPredictions extends Predictions {
	
	protected double[] perInstancePredictions;
	
	public RegressionPredictions() {
	}

	@Override
	public void allocate(int maxNumInstances) {
		perInstancePredictions = new double[maxNumInstances];
	}

	@Override
	public void update(Tree tree, double weight) {
		LearningUtils.updateScores(sample, perInstancePredictions, (RegressionTree) tree, weight); 
		
	}

	@Override
	public double evaluate(EvaluationMetric evalMetric) throws Exception {
		return sample.evaluate(perInstancePredictions, evalMetric);
	}

	@Override
	public void reset() {
	 	Arrays.fill(perInstancePredictions, 0);
	}

}
