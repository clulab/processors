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

import edu.uci.jforestsx.dataset.NumericArrayFactory.NumericArrayType;
import edu.uci.jforestsx.learning.trees.decision.DecisionHistogram;
import edu.uci.jforestsx.learning.trees.regression.RegressionHistogram;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public abstract class NumericArray implements ByteSerializable {

	protected int length;

	public NumericArray(int length) {
		this.length = length;
	}

	public int getLength() {
		return length;
	}

	public abstract NumericArrayType getType();

	public abstract int getBitsPerItem();

	@Override
	public abstract int getSizeInBytes();

	@Override
	public abstract int toByteArray(byte[] arr, int offset);

	@Override
	public abstract int loadFromByteArray(byte[] arr, int offset);

	public abstract int get(int index);

	public abstract void set(int index, int value);

	public void initHistogram(RegressionHistogram histogram, int numInstancesInLeaf, double[] targets,
			double[] weights, int[] indices, int[] instances) {
		
		for (int i = 0; i < numInstancesInLeaf; i++) {
			int featureValue = get(instances[indices[i]]);
			histogram.perValueCount[featureValue]++;
			histogram.perValueWeightedCount[featureValue] += weights[i];
			histogram.perValueSumTargets[featureValue] += targets[i] * weights[i];
		}
		
	}
	
	public void initHistogram(DecisionHistogram histogram, int numInstancesInLeaf, double[] targets,
			double[] weights, int[] indices, int[] instances) {
		
		for (int i = 0; i < numInstancesInLeaf; i++) {
			int featureValue = get(instances[indices[i]]);
			histogram.perValueCount[featureValue]++;
			histogram.perValueWeightedCount[featureValue] += weights[i];
			histogram.perValueTargetDist[featureValue][(int)targets[i]] += weights[i];
		}
		
	}
	
	public abstract NumericArray getSubSampleNumericArray(int[] indices);
}
