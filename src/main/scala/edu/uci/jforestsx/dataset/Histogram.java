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

package edu.uci.jforestsx.dataset;

import java.util.Arrays;

import edu.uci.jforestsx.learning.trees.CandidateSplitsForLeaf;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public abstract class Histogram {

	protected Feature feature;
	
	public int numValues;
	public boolean splittable;
	
	public int totalCount;
	public double totalWeightedCount;
	
	public int[] perValueCount;
	public double[] perValueWeightedCount;
	
	public Histogram(Feature feature) {
		this.feature = feature;
		numValues = feature.getNumberOfValues();
		splittable = true;
		
		totalCount = 0;
		totalWeightedCount = 0;
		perValueCount = new int[numValues];
		perValueWeightedCount = new double[numValues];
	}
	
	protected abstract void initCustomData(CandidateSplitsForLeaf leafSplitCandidates, int[] instances);
	protected abstract void subtractCustomData(Histogram child);
	
	public void init(CandidateSplitsForLeaf leafSplitCandidates, int[] instances) {
		
		totalCount = leafSplitCandidates.getNumInstancesInLeaf();
		totalWeightedCount = leafSplitCandidates.getTotalWeightedCount();
		Arrays.fill(perValueCount, 0);
		Arrays.fill(perValueWeightedCount, 0);
		
		initCustomData(leafSplitCandidates, instances);
		
	}
	
	public void subtractFromMe(Histogram child) throws Exception {
		if (child.numValues != numValues) {
			throw new Exception("inconsistent lengths for Histograms");
		}

		totalCount -= child.totalCount;
		totalWeightedCount -= child.totalWeightedCount;
		
		for (int i = 0; i < numValues; i++) {
			perValueCount[i] -= child.perValueCount[i];
			perValueWeightedCount[i] -= child.perValueWeightedCount[i];
		}
		
		subtractCustomData(child);
	}	
}
