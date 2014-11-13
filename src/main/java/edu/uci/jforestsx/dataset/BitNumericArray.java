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

public class BitNumericArray extends NumericArray {

	private byte[] data;

	public BitNumericArray(int length) {
		super(length);
		int arrLength = (int) Math.ceil(length / 8.0);
		data = new byte[arrLength];
	}

	@Override
	public int getSizeInBytes() {
		return data.length;
	}

	@Override
	public int get(int index) {
		return ((data[index / 8] & (1 << (index % 8))) > 0) ? 1 : 0;
	}

	@Override
	public void set(int index, int value) {
		data[index / 8] |= (byte) (value << (index % 8));
	}

	@Override
	public int getBitsPerItem() {
		return 1;
	}

	@Override
	public int toByteArray(byte[] arr, int offset) {
		for (int i = 0; i < data.length; i++) {
			arr[offset] = data[i];
			offset++;
		}
		return offset;
	}

	@Override
	public int loadFromByteArray(byte[] arr, int offset) {
		for (int i = 0; i < data.length; i++) {
			data[i] = arr[offset];
			offset++;
		}
		return offset;
	}

	@Override
	public NumericArrayType getType() {
		return NumericArrayType.BIT;
	}

	@Override
	public void initHistogram(RegressionHistogram histogram, int numInstancesInLeaf, double[] targets, double[] weights,
			int[] indices, int[] instances) {
		
		double sumTargetsForOne = 0;
		double weightedCountForOne = 0;
		int countForOne = 0;
		int position;
		for (int i = 0; i < numInstancesInLeaf; i++) {
			position = instances[indices[i]];
			int dataIndex = position / 8;
			int bitIndex = position % 8;
			byte v = data[dataIndex];
			v >>= bitIndex;
			if ((v & 1) > 0) {
				double target = targets[i];
				double weight = weights[i];
				countForOne++;
				weightedCountForOne += weight;
				sumTargetsForOne += target * weight;				
			}
		}

		histogram.perValueCount[0] = histogram.totalCount - countForOne;
		histogram.perValueCount[1] = countForOne;
		
		histogram.perValueWeightedCount[0] = histogram.totalWeightedCount - weightedCountForOne;		
		histogram.perValueWeightedCount[1] = weightedCountForOne;
		
		histogram.perValueSumTargets[0] = histogram.sumTargets - sumTargetsForOne;
		histogram.perValueSumTargets[1] = sumTargetsForOne;
		
	}
	
	@Override
	public void initHistogram(DecisionHistogram histogram, int numInstancesInLeaf, double[] targets, double[] weights,
			int[] indices, int[] instances) {
		
		double[] targetDistForOne = histogram.perValueTargetDist[1];
		double weightedCountForOne = 0;
		int countForOne = 0;
		int position;
		for (int i = 0; i < numInstancesInLeaf; i++) {
			position = instances[indices[i]];
			int dataIndex = position / 8;
			int bitIndex = position % 8;
			byte v = data[dataIndex];
			v >>= bitIndex;
			if ((v & 1) > 0) {
				double target = targets[i];
				double weight = weights[i];
				countForOne++;
				weightedCountForOne += weight;
				targetDistForOne[(int) target] += weight;				
			}
		}

		histogram.perValueCount[0] = histogram.totalCount - countForOne;
		histogram.perValueCount[1] = countForOne;
		
		histogram.perValueWeightedCount[0] = histogram.totalWeightedCount - weightedCountForOne;		
		histogram.perValueWeightedCount[1] = weightedCountForOne;
		
		int numClasses = histogram.targetDist.length;
		for (int c = 0; c < numClasses; c++) {
			histogram.perValueTargetDist[0][c] = histogram.targetDist[c] - targetDistForOne[c];
		}
	}
	
	@Override
	public NumericArray getSubSampleNumericArray(int[] indices) {
		BitNumericArray subsampleArray = new BitNumericArray(indices.length);
		for (int i = 0 ; i < indices.length; i++) {
			subsampleArray.set(i, get(indices[i]));
		}
		return subsampleArray;
	}
}