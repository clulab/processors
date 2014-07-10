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

import edu.uci.jforestsx.learning.trees.CandidateSplitsForLeaf;
import edu.uci.jforestsx.learning.trees.LeafInstances;
import edu.uci.jforestsx.learning.trees.TreeLeafInstances;
import edu.uci.jforestsx.sample.Sample;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class RegressionCandidateSplitsForLeaf extends CandidateSplitsForLeaf {
	
	protected double sumTargets;
	
	public RegressionCandidateSplitsForLeaf(int numFeatures, int numInstances) {
		super(numFeatures, numInstances);		
		bestSplitPerFeature = new RegressionTreeSplit[numFeatures];
		for (int f = 0; f < numFeatures; f++) {
			bestSplitPerFeature[f] = new RegressionTreeSplit();			
		}
	}

	public double getSumTargets() {
		return sumTargets;
	}
	
	@Override
	public void init(int curLeafIndex, TreeLeafInstances treeLeafInstances, Sample trainSet) {
		this.init(curLeafIndex);
		totalWeightedCount = 0;
		
		LeafInstances leafInstances = treeLeafInstances.getLeafInstances(curLeafIndex);
		numInstancesInLeaf = leafInstances.end - leafInstances.begin;
		
		sumTargets = 0;
		for (int i = 0; i < numInstancesInLeaf; i++) {
			indices[i] = leafInstances.indices[leafInstances.begin + i];
			double target = trainSet.targets[indices[i]];
			double weight = trainSet.weights[indices[i]];
			this.targets[i] = target;
			this.weights[i] = weight;
			sumTargets += target * weight;
			totalWeightedCount += weight;
		}
	}
	
}
