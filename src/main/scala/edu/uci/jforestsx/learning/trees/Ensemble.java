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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class Ensemble implements Serializable { // SISTA added Serializable

	private List<Tree> trees;
	private List<Double> weights;

	public Ensemble() {
		trees = new ArrayList<Tree>();
		weights = new ArrayList<Double>();		
	}

	public List<Tree> getTrees() {
		return trees;
	}

	public void addTree(Tree tree, double weight) {
		trees.add(tree);
		weights.add(weight);
	}

	public void addTreeAt(Tree tree, double weight, int index) {
		trees.add(index, tree);
		weights.add(weight);
	}

	public void removeTree(int index) {
		trees.remove(index);
		weights.remove(index);
	}

	public void removeLastTrees(int k) {
		for (int i = 0; i < k; i++) {
			removeTree(trees.size() - 1);
		}
	}

	public Tree getTreeAt(int index) {
		return trees.get(index);
	}

	public double getWeightAt(int index) {
		return weights.get(index);
	}

	public int getNumTrees() {
		return trees.size();
	}

	public List<Double> getWeights() {
		return weights;
	}

	public void setWeights(List<Double> weights) {
		this.weights = weights;
	}

	@Override
	public String toString() {
		return toString(-1);
	}

	public String toString(int prefix) {
		if (prefix > trees.size() || prefix < 0) {
			prefix = trees.size();
		}
		StringBuilder sb = new StringBuilder();
		sb.append("<Ensemble>");
		for (int i = 0; i < prefix; i++) {
			sb.append(trees.get(i).toString(weights.get(i), 1));
		}
		sb.append("\n</Ensemble>");
		return sb.toString();
	}

	private String getXmlAttribute(String line, String attr) {
		int idx = line.indexOf(attr + "=\"");
		if (idx < 0) {
			return null;
		}
		int endIdx = line.indexOf('"', idx + attr.length() + 2);
		return line.substring(idx + attr.length() + 2, endIdx);
	}

	public <T extends Tree> void loadFromFile(Class<T> _c, File file) throws Exception {
		BufferedReader reader = new BufferedReader(new FileReader(file));
		String line = reader.readLine(); // <Ensemble>
		while ((line = reader.readLine()) != null) {
			line = line.trim();
			if (line.equals("</Ensemble>")) {
				break;
			}
			String header = line;
			int leaves = Integer.parseInt(getXmlAttribute(header, "leaves"));
			double weight = Double.parseDouble(getXmlAttribute(header, "weight"));

			String featuresLine = reader.readLine();
			String leftChildrenLine = reader.readLine();
			String rightChildrenLine = reader.readLine();
			String thresholds = reader.readLine();
			String originalThresholds = reader.readLine();

			T tree = _c.newInstance();
			tree.loadFromString(leaves, featuresLine, leftChildrenLine, rightChildrenLine, thresholds, originalThresholds);

			tree.loadCustomData(reader.readLine());
			
			reader.readLine(); // </RegressionTree>

			addTree(tree, weight);
		}
	}
}
