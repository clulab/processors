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

package edu.uci.jforestsx.learning.trees;

import java.util.Arrays;

import edu.uci.jforestsx.dataset.Dataset;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class TreeLeafInstances {
	private int[] leafBegin;
	private int[] leafEnd;
	private int[] indices;
	private int[] tempIndices;

	public TreeLeafInstances(int numInstances, int maxLeaves) {
		leafBegin = new int[maxLeaves];
		leafEnd = new int[maxLeaves];
		indices = new int[numInstances];
	}

	public void init(int size) {
		Arrays.fill(leafBegin, 0);
		Arrays.fill(leafEnd, 0);
		leafBegin[0] = 0;
		leafEnd[0] = size;
		for (int i = 0; i < size; i++) {
			indices[i] = i;
		}		
	}

	public void split(int leaf, Dataset dataset, int featureIndex, int threshold, int rightChild, int[] instances) {
		if (tempIndices == null) {
			tempIndices = new int[indices.length];
		}

		int begin = leafBegin[leaf];
		int end = leafEnd[leaf];
		int newEnd = begin;
		int tempEnd = begin;

		for (int idx = begin; idx < end; idx++) {
			int index = indices[idx];
			if (dataset.getFeatureValue(instances[index], featureIndex) > threshold) {
				tempIndices[tempEnd++] = index;
			} else {
				indices[newEnd++] = index;
			}
		}

		int rightCount = tempEnd - begin;
		System.arraycopy(tempIndices, begin, indices, newEnd, rightCount);

		leafEnd[leaf] = newEnd;
		leafBegin[rightChild] = newEnd;
		leafEnd[rightChild] = newEnd + rightCount;
	}

	public LeafInstances getLeafInstances(int leaf) {
		LeafInstances leafInstances = new LeafInstances();
		loadLeafInstances(leaf, leafInstances);
		return leafInstances;
	}

	public void loadLeafInstances(int leaf, LeafInstances leafInstances) {
		leafInstances.indices = indices;
		leafInstances.begin = leafBegin[leaf];
		leafInstances.end = leafEnd[leaf];
	}

	public int getNumberOfInstancesInLeaf(int leaf) {
		return leafEnd[leaf] - leafBegin[leaf];
	}
}
