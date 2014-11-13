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

import edu.uci.jforestsx.dataset.Dataset;
import edu.uci.jforestsx.dataset.Feature;
import edu.uci.jforestsx.learning.trees.TreeSplit;
import edu.uci.jforestsx.learning.trees.Tree;
import edu.uci.jforestsx.sample.Sample;
import edu.uci.jforestsx.util.ArraysUtil;

import java.io.Serializable;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class RegressionTree extends Tree implements Serializable {

	private double[] leafOutputs;
	private double maxLeafOutput;

	public RegressionTree() {
	}

	@Override
	public Object clone() {
		RegressionTree copy = new RegressionTree();
		super.copyTo(copy);
		copy.maxLeafOutput = maxLeafOutput;
		copy.leafOutputs = new double[leftChild.length + 1];
		System.arraycopy(leafOutputs, 0, copy.leafOutputs, 0, copy.leafOutputs.length);
		return copy;
	}

	public void init(int maxLeaves, double maxLeafOutput) {
		super.init(maxLeaves);
		leafOutputs = new double[maxLeaves];
		this.maxLeafOutput = maxLeafOutput;
	}

	private void markPresenceOfNodesInSubtree(int node, boolean[] internalNodeIsPresent, boolean[] leafIsPresent) {
		if (node < 0) {
			leafIsPresent[~node] = true;
		} else {
			internalNodeIsPresent[node] = true;
			markPresenceOfNodesInSubtree(leftChild[node], internalNodeIsPresent, leafIsPresent);
			markPresenceOfNodesInSubtree(rightChild[node], internalNodeIsPresent, leafIsPresent);
		}
	}

	/**
	 * Renames node names in the tree to make them 
	 * as 0, 1, 2, ..., n
	 */
	public void normalizeNodeNames() {
		int maxNumLeaves = leftChild.length + 1;

		/*
		 * Extract node names that are present in the tree
		 */
		boolean[] internalNodeIsPresent = new boolean[maxNumLeaves - 1];
		boolean[] leafIsPresent = new boolean[maxNumLeaves];
		markPresenceOfNodesInSubtree(0, internalNodeIsPresent, leafIsPresent);

		int newLeafCount = 0;
		for (int l = 0; l < maxNumLeaves; l++) {
			if (leafIsPresent[l]) {
				newLeafCount++;
			}
		}

		/*
		 * Compute mappings from old names to new names
		 */
		int[] internalNodesMappingsOld2New = new int[maxNumLeaves - 1];
		int[] internalNodesMappingsNew2Old = new int[newLeafCount - 1];
		int internalIdx = 0;
		for (int i = 0; i < maxNumLeaves - 1; i++) {
			if (internalNodeIsPresent[i]) {
				internalNodesMappingsOld2New[i] = internalIdx;
				internalNodesMappingsNew2Old[internalIdx] = i;
				internalIdx++;
				if (internalIdx == newLeafCount - 1) {
					break;
				}
			}
		}

		int[] leavesMappingNew2Old = new int[newLeafCount];
		int[] leavesMappingOld2New = new int[maxNumLeaves];
		int leafIdx = 0;
		for (int i = 0; i < maxNumLeaves; i++) {
			if (leafIsPresent[i]) {
				leavesMappingNew2Old[leafIdx] = i;
				leavesMappingOld2New[i] = leafIdx;
				leafIdx++;
				if (leafIdx == newLeafCount) {
					break;
				}
			}
		}

		/*
		 * Convert old names to new names
		 */
		int[] newLeftChild = new int[newLeafCount - 1];
		int[] newRightChild = new int[newLeafCount - 1];
		for (int i = 0; i < maxNumLeaves - 1; i++) {
			if (internalNodeIsPresent[i]) {
				int prevLeftChild = leftChild[i];
				if (prevLeftChild < 0) {
					newLeftChild[internalNodesMappingsOld2New[i]] = ~leavesMappingOld2New[~prevLeftChild];
				} else {
					newLeftChild[internalNodesMappingsOld2New[i]] = internalNodesMappingsOld2New[prevLeftChild];
				}
				int prevRightChild = rightChild[i];
				if (prevRightChild < 0) {
					newRightChild[internalNodesMappingsOld2New[i]] = ~leavesMappingOld2New[~prevRightChild];
				} else {
					newRightChild[internalNodesMappingsOld2New[i]] = internalNodesMappingsOld2New[prevRightChild];
				}
			}
		}
		for (int i = 0; i < newLeafCount - 1; i++) {
			leftChild[i] = newLeftChild[i];
			rightChild[i] = newRightChild[i];
			splitFeatures[i] = splitFeatures[internalNodesMappingsNew2Old[i]];
			thresholds[i] = thresholds[internalNodesMappingsNew2Old[i]];
		}
		for (int i = 0; i < newLeafCount; i++) {
			leafOutputs[i] = leafOutputs[leavesMappingNew2Old[i]];
		}
		this.numLeaves = newLeafCount;
	}

	public double getLeafOutput(int leaf) {
		return leafOutputs[leaf];
	}

	public void setLeafOutput(int leaf, double output) {
		if (maxLeafOutput > 0) {
			if (output > maxLeafOutput) {
				output = maxLeafOutput;
			} else if (output < -maxLeafOutput) {
				output = -maxLeafOutput;
			}
		}
		leafOutputs[leaf] = output;
	}

	public void multiplyLeafOutputs(double factor) {
		if (factor == 1.0) {
			return;
		}
		for (int l = 0; l < numLeaves; l++) {
			setLeafOutput(l, leafOutputs[l] * factor);
		}
	}

	public void incrementLeafOutputs(double constant) {
		if (constant == 0) {
			return;
		}
		for (int l = 0; l < numLeaves; l++) {
			setLeafOutput(l, leafOutputs[l] + constant);
		}
	}

	public double getOutput(Dataset dataset, int instanceIndex) {
		return leafOutputs[getLeaf(dataset, instanceIndex)];
	}

  /** SISTA added code: similar to getOutput for a Dataset element */
  public double getOutput(Feature[] features) {
    return leafOutputs[getLeaf(features)];
  }

	public double[] getOutputs(Dataset dataset) {
		double[] outputs = new double[dataset.numInstances];
		for (int i = 0; i < dataset.numInstances; i++) {
			outputs[i] = getOutput(dataset, i);
		}
		return outputs;
	}

	// turns a leaf of the tree into an internal node with two leaf-children
	@Override
	public int split(int leaf, TreeSplit split) {
		int indexOfNewNonLeaf = super.split(leaf, split);
		RegressionTreeSplit rsplit = (RegressionTreeSplit) split;
		leafOutputs[leaf] = rsplit.leftOutput;
		leafOutputs[numLeaves - 1] = rsplit.rightOutput;
		return indexOfNewNonLeaf;
	}

	@Override
	public void loadCustomData(String str) throws Exception {
		leafOutputs = ArraysUtil.loadDoubleArrayFromLine(removeXmlTag(str, "LeafOutputs"), numLeaves);
	}

	@Override
	protected void addCustomData(String linePrefix, StringBuilder sb) {
		StringBuilder sbOutput = new StringBuilder();
		for (int n = 0; n < numLeaves; n++) {
			sbOutput.append(" " + leafOutputs[n]);
		}
		sb.append("\n" + linePrefix + "\t<LeafOutputs>" + sbOutput.toString().trim() + "</LeafOutputs>");
	}

	@Override
	public void backfit(Sample sample) {
		double[] sumPerLeaf = new double[numLeaves];
		double[] weightedCountPerLeaf = new double[numLeaves];
		for (int i = 0; i < sample.size; i++) {
			int leaf = getLeaf(sample.dataset, sample.indicesInDataset[i]);
			sumPerLeaf[leaf] += sample.targets[i] * sample.weights[i];
			weightedCountPerLeaf[leaf] += sample.weights[i];
		}
		boolean hasZeroCountLeaf = false;
		double[] sumPerInternalNode = new double[numLeaves - 1];
		int[] countPerInternalNode = new int[numLeaves - 1];
		for (int l = 0; l < numLeaves; l++) {
			if (weightedCountPerLeaf[l] > 0) {
				double newOutput = sumPerLeaf[l] / weightedCountPerLeaf[l];
				setLeafOutput(l, newOutput);
				int parent = getParent(~l);
				while (parent >= 0) {
					sumPerInternalNode[parent] += newOutput;
					countPerInternalNode[parent] += weightedCountPerLeaf[l];
					parent = getParent(parent);
				}
			} else {
				hasZeroCountLeaf = true;
			}
		}
		if (hasZeroCountLeaf) {
			for (int l = 0; l < numLeaves; l++) {
				if (weightedCountPerLeaf[l] == 0) {
					int parent = getParent(~l);
					while (parent >= 0) {
						if (countPerInternalNode[parent] > 0) {
							setLeafOutput(l, sumPerInternalNode[parent] / countPerInternalNode[parent]);
							break;
						}
						parent = getParent(parent);
					}
				}
			}
		}
	}
}
