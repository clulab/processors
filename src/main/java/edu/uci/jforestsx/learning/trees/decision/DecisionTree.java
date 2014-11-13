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

import edu.uci.jforestsx.dataset.Dataset;
import edu.uci.jforestsx.learning.trees.TreeSplit;
import edu.uci.jforestsx.learning.trees.Tree;
import edu.uci.jforestsx.sample.Sample;
import edu.uci.jforestsx.util.ArraysUtil;
import edu.uci.jforestsx.util.MathUtil;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class DecisionTree extends Tree {

	private double[][] leafTargetDistributions;
	private int numClasses;

	public DecisionTree() {
	}

	@Override
	public Object clone() {
		DecisionTree copy = new DecisionTree();
		super.copyTo(copy);
		copy.leafTargetDistributions = MathUtil.cloneDoubleMatrix(leafTargetDistributions);
		return copy;
	}

	public void init(int maxLeaves, int numClasses) {
		super.init(maxLeaves);
		this.numClasses = numClasses;
		leafTargetDistributions = new double[maxLeaves][numClasses];
	}

	public double[] getLeafTargetDistribution(int leaf) {
		return leafTargetDistributions[leaf];
	}

	public void setLeafTargetDistribution(int leaf, double[] dist) {
		System.arraycopy(dist, 0, leafTargetDistributions[leaf], 0, dist.length);
	}

	public int classify(Dataset dataset, int instanceIndex) {
		double[] dist = leafTargetDistributions[getLeaf(dataset, instanceIndex)];
		return ArraysUtil.findMaxIndex(dist);
	}

	public double[] getDistributionForInstance(Dataset dataset, int instanceIndex) {
		return leafTargetDistributions[getLeaf(dataset, instanceIndex)];
	}

	public int[] getPredictions(Dataset dataset) {
		int[] predictions = new int[dataset.numInstances];
		for (int i = 0; i < dataset.numInstances; i++) {
			predictions[i] = classify(dataset, i);
		}
		return predictions;
	}

	/**
	 * turns a leaf of the tree into an internal node with two leaf-children
	 */
	@Override
	public int split(int leaf, TreeSplit split) {
		int indexOfNewNonLeaf = super.split(leaf, split);
		DecisionTreeSplit dsplit = (DecisionTreeSplit) split;
		for (int c = 0; c < numClasses; c++) {
			leafTargetDistributions[leaf][c] = dsplit.leftTargetDist[c];
			leafTargetDistributions[numLeaves - 1][c] = dsplit.rightTargetDist[c];			
		}		
		normalizeLeafTargetDistributions(leaf);
		normalizeLeafTargetDistributions(numLeaves - 1);
		return indexOfNewNonLeaf;
	}
	
	/*public void normalizeLeafTargetDistributions() {
		for (int l = 0; l < numLeaves; l++) {
			normalizeLeafTargetDistributions(l);
		}
	}*/
	
	private void normalizeLeafTargetDistributions(int leaf) {
		double sum = 0;
		for (int c = 0; c < numClasses; c++) {
			sum += leafTargetDistributions[leaf][c];
		}
		for (int c = 0; c < numClasses; c++) {
			leafTargetDistributions[leaf][c] /= sum;
		}
	}

	@Override
	public void loadCustomData(String str) throws Exception {
		leafTargetDistributions = ArraysUtil.loadDoubleMatrixFromLine(removeXmlTag(str, "LeafTargetDistributions"), numLeaves, numClasses);
	}

	@Override
	protected void addCustomData(String linePrefix, StringBuilder sb) {
		StringBuilder sbOutput = new StringBuilder();
		for (int n = 0; n < numLeaves; n++) {
			for (int c = 0; c < numClasses; c++) {
				sbOutput.append(" " + leafTargetDistributions[n][c]);
			}
		}
		sb.append("\n" + linePrefix + "\t<LeafTargetDistributions>" + sbOutput.toString().trim() + "</LeafTargetDistributions>");
	}

	@Override
	public void backfit(Sample sample) {
		double[][] distPerLeaf = new double[numLeaves][numClasses];
		for (int i = 0; i < sample.size; i++) {
			int leaf = getLeaf(sample.dataset, sample.indicesInDataset[i]);
			distPerLeaf[leaf][(int) sample.targets[i]] += sample.weights[i];
		}
		double[] weightedCountPerLeaf = new double[numLeaves];
		for (int l = 0; l < numLeaves; l++) {
			for (int c = 0; c < numClasses; c++) {
				weightedCountPerLeaf[l] += distPerLeaf[l][c];
			}
		}
		boolean hasZeroCountLeaf = false;
		double[][] distPerInternalNode = new double[numLeaves - 1][numClasses];		
		for (int l = 0; l < numLeaves; l++) {
			if (weightedCountPerLeaf[l] > 0) {
				setLeafTargetDistribution(l, distPerLeaf[l]);
				int parent = getParent(~l);
				while (parent >= 0) {
					for (int c = 0; c < numClasses; c++) {
						distPerInternalNode[parent][c] += distPerLeaf[l][c];
					}
					parent = getParent(parent);
				}
			} else {
				hasZeroCountLeaf = true;
			}
		}
		if (hasZeroCountLeaf) {
			double[] weightedCountPerInternalNode = new double[numLeaves - 1];
			for (int i = 0; i < weightedCountPerInternalNode.length; i++) {
				for (int c = 0; c < numClasses; c++) {
					weightedCountPerInternalNode[i] += distPerInternalNode[i][c];
				}
			}
			for (int l = 0; l < numLeaves; l++) {
				if (weightedCountPerLeaf[l] == 0) {
					int parent = getParent(~l);
					while (parent >= 0) {
						if (weightedCountPerInternalNode[parent] > 0) {
							setLeafTargetDistribution(l, distPerInternalNode[parent]);							
							break;
						}
						parent = getParent(parent);
					}
				}
			}
		}
	}
}
