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

public class NullNumericArray extends NumericArray {

	private static NullNumericArray instance;

	static {
		instance = new NullNumericArray(0);
	}

	public static NullNumericArray getInstance() {
		return instance;
	}

	public NullNumericArray(int length) {
		super(length);
	}

	@Override
	public int getSizeInBytes() {
		return 0;
	}

	@Override
	public int get(int index) {
		return 0;
	}

	@Override
	public void set(int index, int value) {
		// Do nothing
	}

	@Override
	public int getBitsPerItem() {
		return 0;
	}

	@Override
	public int toByteArray(byte[] arr, int offset) {
		return offset;
	}

	@Override
	public int loadFromByteArray(byte[] arr, int offset) {
		return offset;
	}

	@Override
	public NumericArrayType getType() {
		return NumericArrayType.NULL;
	}

	@Override
	public void initHistogram(RegressionHistogram histogram, int numInstancesInLeaf,
			double[] targets, double[] weights, int[] indices, int[] instances) {
		
		histogram.perValueCount[0] = histogram.totalCount;
		histogram.perValueWeightedCount[0] = histogram.totalWeightedCount;
		histogram.perValueSumTargets[0] = histogram.sumTargets;		
		
	}
	
	@Override
	public void initHistogram(DecisionHistogram histogram, int numInstancesInLeaf,
			double[] targets, double[] weights, int[] indices, int[] instances) {
		
		histogram.perValueCount[0] = histogram.totalCount;
		histogram.perValueWeightedCount[0] = histogram.totalWeightedCount;
		histogram.perValueTargetDist[0] = histogram.targetDist;		
		
	}

	@Override
	public NumericArray getSubSampleNumericArray(int[] indices) {
		return instance;
	}
}