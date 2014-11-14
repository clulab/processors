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

import edu.uci.jforestsx.dataset.Feature;
import edu.uci.jforestsx.dataset.Histogram;
import edu.uci.jforestsx.learning.trees.CandidateSplitsForLeaf;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class RegressionHistogram extends Histogram {

	public double sumTargets;
	public double[] perValueSumTargets;

	public RegressionHistogram(Feature feature) {
		super(feature);
		sumTargets = 0;		
		perValueSumTargets = new double[numValues];
	}
	
	@Override
	protected void initCustomData(CandidateSplitsForLeaf leafSplitCandidates, int[] instances) {
		RegressionCandidateSplitsForLeaf rLeafSplitCandidates = (RegressionCandidateSplitsForLeaf) leafSplitCandidates;
		
		sumTargets = rLeafSplitCandidates.getSumTargets();		
		Arrays.fill(perValueSumTargets, 0);
		
		feature.bins.initHistogram(this, totalCount, leafSplitCandidates.getTargets(),
				leafSplitCandidates.getWeights(), leafSplitCandidates.getIndices(), instances);
	}

	@Override
	protected void subtractCustomData(Histogram child) {
		RegressionHistogram rChild = (RegressionHistogram) child;
		sumTargets -= rChild.sumTargets;
		for (int i = 0; i < numValues; i++) {
			perValueSumTargets[i] -= rChild.perValueSumTargets[i];			
		}
	}
	
}
