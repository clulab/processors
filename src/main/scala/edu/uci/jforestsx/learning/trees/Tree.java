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

import java.util.ArrayList;
import java.util.List;

import edu.uci.jforestsx.dataset.Dataset;

import edu.uci.jforestsx.dataset.Feature;
import edu.uci.jforestsx.sample.Sample;
import edu.uci.jforestsx.util.ArraysUtil;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public abstract class Tree implements Cloneable {

	public int numLeaves;

	protected int[] leftChild;
	protected int[] rightChild;
	protected int[] splitFeatures;
	protected int[] thresholds;
	protected double[] originalThresholds;

	public Tree() {
	}

	public void init(int maxLeaves) {
		leftChild = new int[maxLeaves - 1];
		rightChild = new int[maxLeaves - 1];
		splitFeatures = new int[maxLeaves - 1];
		thresholds = new int[maxLeaves - 1];
		originalThresholds = new double[maxLeaves - 1];
		numLeaves = 1;
	}

	protected void copyTo(Tree copy) {
		copy.init(leftChild.length + 1);
		copy.numLeaves = numLeaves;
		System.arraycopy(leftChild, 0, copy.leftChild, 0, numLeaves - 1);
		System.arraycopy(rightChild, 0, copy.rightChild, 0, numLeaves - 1);
		System.arraycopy(splitFeatures, 0, copy.splitFeatures, 0, numLeaves - 1);
		System.arraycopy(thresholds, 0, copy.thresholds, 0, numLeaves - 1);
		System.arraycopy(originalThresholds, 0, copy.originalThresholds, 0, numLeaves - 1);
	}

	public int getLeftChild(int node) {
		return leftChild[node];
	}

	public void setLeftChild(int node, int newChild) {
		leftChild[node] = newChild;
	}

	public int getRightChild(int node) {
		return rightChild[node];
	}

	public void setRightChild(int node, int newChild) {
		rightChild[node] = newChild;
	}

	public int getSplitFeature(int node) {
		return splitFeatures[node];
	}

	public int getThreshold(int node) {
		return thresholds[node];
	}

	public double getOriginalThreshold(int node) {
		return originalThresholds[node];
	}

  /** SISTA added code: similar to getLeaf for one Dataset instance */
  public int getLeaf(Feature[] features) {
    if (numLeaves == 1) {
      return 0;
    }

    int node = 0;
    while (node >= 0) {
      int split = splitFeatures[node];
      assert(split < features.length);
      Feature feat = features[split];
      if(feat.upperBounds[feat.bins.get(0)] <= thresholds[node]) {
        node = leftChild[node];
      } else {
        node = rightChild[node];
      }
    }

    return ~node;
  }

	public int getLeaf(Dataset dataset, int instanceIndex) {
		if (numLeaves == 1) {
			return 0;
		}

		int node = 0;
		while (node >= 0) {
      /*
			// SISTA
			double v = dataset.getOriginalFeatureValue(instanceIndex, splitFeatures[node]);
			System.out.println("Classification point:");
			System.out.println("\tnode: " + node);
			System.out.println("\tsplitFeatures[node]: " + splitFeatures[node]);
			System.out.println("\tv: " + v);
			*/

			if (dataset.getFeatureValue(instanceIndex, splitFeatures[node]) <= thresholds[node]) {
				node = leftChild[node];
			} else {
				node = rightChild[node];
			}
		}
		return ~node;
	}

	public int getLeafFromOriginalThreshold(Dataset dataset, int instanceIndex) {
		if (numLeaves == 1) {
			return 0;
		}

		int node = 0;
		while (node >= 0) {
			

			if (dataset.getOriginalFeatureValue(instanceIndex, splitFeatures[node]) <= originalThresholds[node]) {
				node = leftChild[node];
			} else {
				node = rightChild[node];
			}
		}
		return ~node;
	}

	public int getLeafFromOriginalThreshold(double[] featureVector) {
		if (numLeaves == 1) {
			return 0;
		}

		int node = 0;
		while (node >= 0) {
			if (featureVector[splitFeatures[node]] <= originalThresholds[node]) {
				node = leftChild[node];
			} else {
				node = rightChild[node];
			}
		}
		return ~node;
	}

	public int getParent(int node) {
		int parent = ArraysUtil.findIndex(leftChild, node, numLeaves - 1);
		if (parent >= 0) {
			return parent;
		}
		parent = ArraysUtil.findIndex(rightChild, node, numLeaves - 1);
		if (parent >= 0) {
			return parent;
		}
		return -1;
	}

	/**
	 * Returns list of node parents
	 */
	public int[] getNodeParents(int node) {
		List<Integer> parents = new ArrayList<Integer>();
		int parent = getParent(node);
		while (parent >= 0) {
			parents.add(parent);
			parent = getParent(parent);
		}
		int[] result = new int[parents.size()];
		for (int i = 0; i < result.length; i++) {
			result[i] = parents.get(parents.size() - i - 1);
		}
		return result;
	}
	
	/**
	 * Extracts the list of leaves that belong to the subtree which
	 * which has 'node' as its root. The result is added to 'leaves'
	 * input list.
	 */
	public void loadLeavesInSubtree(int node, List<Integer> leaves) {
		if (node < 0) {
			leaves.add(~node);
		} else {
			loadLeavesInSubtree(leftChild[node], leaves);
			loadLeavesInSubtree(rightChild[node], leaves);
		}
	}

	/**
	 * turns a leaf of the tree into an internal node with two leaf-children
	 */
	public int split(int leaf, TreeSplit split) {
		int indexOfNewNonLeaf = numLeaves - 1;

		// find the leaf's parent, and update its info
		int parent = ArraysUtil.findIndex(leftChild, ~leaf, numLeaves - 1);
		if (parent >= 0) {
			leftChild[parent] = indexOfNewNonLeaf;
		} else {
			parent = ArraysUtil.findIndex(rightChild, ~leaf, numLeaves - 1);
			if (parent >= 0) {
				rightChild[parent] = indexOfNewNonLeaf;
			}
		}

		splitFeatures[indexOfNewNonLeaf] = split.feature;
		thresholds[indexOfNewNonLeaf] = split.threshold;
		originalThresholds[indexOfNewNonLeaf] = split.originalThreshold;
		leftChild[indexOfNewNonLeaf] = ~leaf;
		rightChild[indexOfNewNonLeaf] = ~numLeaves;

		numLeaves++;
		return indexOfNewNonLeaf;
	}

	private int getNodeLabel(int n) {
		if (n < 0) {
			return -1 - (~n);
		}
		return n;
	}

	protected abstract void addCustomData(String linePrefix, StringBuilder sb);

	public abstract void loadCustomData(String str) throws Exception;

	public String toString(double weight, int indentationLevel) {
		String linePrefix = "";
		for (int i = 0; i < indentationLevel; i++) {
			linePrefix += "\t";
		}
		StringBuilder sb = new StringBuilder();
		sb.append("\n" + linePrefix + "<Tree leaves=\"" + numLeaves + "\" weight=\"" + weight + "\">");

		StringBuilder sbFeatures = new StringBuilder();
		StringBuilder sbLeftChildren = new StringBuilder();
		StringBuilder sbRightChild = new StringBuilder();
		StringBuilder sbThreshold = new StringBuilder();
		StringBuilder sbOriginalThreshold = new StringBuilder();

		int numNonLeaves = numLeaves - 1;
		for (int n = 0; n < numNonLeaves; n++) {
			sbFeatures.append(" " + splitFeatures[n]);
			sbLeftChildren.append(" " + getNodeLabel(leftChild[n]));
			sbRightChild.append(" " + getNodeLabel(rightChild[n]));
			sbThreshold.append(" " + thresholds[n]);
			sbOriginalThreshold.append(" " + originalThresholds[n]);
		}

		sb.append("\n" + linePrefix + "\t<SplitFeatures>" + sbFeatures.toString().trim() + "</SplitFeatures>");
		sb.append("\n" + linePrefix + "\t<LeftChildren>" + sbLeftChildren.toString().trim() + "</LeftChildren>");
		sb.append("\n" + linePrefix + "\t<RightChildren>" + sbRightChild.toString().trim() + "</RightChildren>");
		sb.append("\n" + linePrefix + "\t<Thresholds>" + sbThreshold.toString().trim() + "</Thresholds>");
		sb.append("\n" + linePrefix + "\t<OriginalThresholds>" + sbOriginalThreshold.toString().trim() + "</OriginalThresholds>");

		addCustomData(linePrefix, sb);

		sb.append("\n" + linePrefix + "</Tree>");
		return sb.toString();
	}

	protected String removeXmlTag(String line, String tagName) {
		return line.trim().replace(tagName, "").replace("<>", "").replace("</>", "");
	}

	public void loadFromString(int numLeaves, String splitFeaturesLine, String leftChildrenLine, String rightChildrenLine, String thresholdsLine,
			String originalThresholdsLine) throws Exception {
		splitFeatures = ArraysUtil.loadIntArrayFromLine(removeXmlTag(splitFeaturesLine, "SplitFeatures"), numLeaves - 1);
		leftChild = ArraysUtil.loadIntArrayFromLine(removeXmlTag(leftChildrenLine, "LeftChildren"), numLeaves - 1);
		rightChild = ArraysUtil.loadIntArrayFromLine(removeXmlTag(rightChildrenLine, "RightChildren"), numLeaves - 1);
		thresholds = ArraysUtil.loadIntArrayFromLine(removeXmlTag(thresholdsLine, "Thresholds"), numLeaves - 1);
		originalThresholds = ArraysUtil.loadDoubleArrayFromLine(removeXmlTag(originalThresholdsLine, "OriginalThresholds"), numLeaves - 1);
		this.numLeaves = numLeaves;
	}
	
	public abstract void backfit(Sample sample);
	
}
