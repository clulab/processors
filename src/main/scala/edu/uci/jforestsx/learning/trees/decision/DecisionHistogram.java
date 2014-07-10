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

import edu.uci.jforestsx.dataset.Feature;
import edu.uci.jforestsx.dataset.Histogram;
import edu.uci.jforestsx.learning.trees.CandidateSplitsForLeaf;
import edu.uci.jforestsx.util.MathUtil;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class DecisionHistogram extends Histogram {

	public double[] targetDist;
	public double[][] perValueTargetDist;
	
	private int numClasses;

	public DecisionHistogram(Feature feature, int numClasses) {
		super(feature);
		this.numClasses = numClasses; 
		targetDist = new double[numClasses];
		perValueTargetDist = new double[numValues][numClasses];
		for (int i = 0; i < numValues; i++) {
			perValueTargetDist[i] = new double[numClasses];
		}
	}
	
	@Override
	protected void initCustomData(CandidateSplitsForLeaf leafSplitCandidates, int[] instances) {
		DecisionCandidateSplitsForLeaf dLeafSplitCandidates = (DecisionCandidateSplitsForLeaf) leafSplitCandidates;
		
		System.arraycopy(dLeafSplitCandidates.getTargetDist(), 0, targetDist, 0, numClasses);
		MathUtil.clearDoubleMatrix(perValueTargetDist);
		
		feature.bins.initHistogram(this, totalCount, leafSplitCandidates.getTargets(),
				leafSplitCandidates.getWeights(), leafSplitCandidates.getIndices(), instances);
	}

	@Override
	protected void subtractCustomData(Histogram child) {
		DecisionHistogram dChild = (DecisionHistogram) child;
		for (int c = 0; c < numClasses; c++) {
			targetDist[c] -= dChild.targetDist[c];
			for (int i = 0; i < numValues; i++) {
				perValueTargetDist[i][c] -= dChild.perValueTargetDist[i][c];			
			}
		}
	}
	
}
