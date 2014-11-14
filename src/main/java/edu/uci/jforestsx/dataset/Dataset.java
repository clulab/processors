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

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class Dataset {
	public Feature[] features;
	public double[] targets;
	public int numInstances;
	public int numFeatures;

	// This uri helps preventing reloading of the same dataset
	public String uri;
	public boolean needsInitialization;

	public Dataset() {
	}

	public void init(Feature[] features, double[] targets) {
		this.features = features;
		this.targets = targets;
		this.numInstances = targets.length;
		this.numFeatures = features.length;
	}
	
	public void printFeatureValueCounts() {
		int sum = 0;
		for (int f = 0; f < numFeatures; f++) {
			int count = features[f].upperBounds.length;
			System.out.println(count);
			sum += count;
		}
		System.out.println("Avg: " + (sum / numFeatures));
	}

	public int getFeatureValue(int instanceIndex, int featureIndex) {
		Feature feature = features[featureIndex];
		try {
			return feature.upperBounds[feature.bins.get(instanceIndex)];
		} catch (Exception e) {
			e.printStackTrace();
		}
		return 0;
	}
	
	public double getOriginalFeatureValue(int instanceIndex, int featureIndex) {
		Feature feature = features[featureIndex];
		try {
			int scaledValue = feature.upperBounds[feature.bins.get(instanceIndex)];
			return feature.getOriginalValue(scaledValue);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return 0;
	}

	public int getMaxFeatureValues() {
		int max = 0;
		for (Feature f : features) {
			if (f.getNumberOfValues() > max) {
				max = f.getNumberOfValues();
			}
		}
		return max;
	}
	
	public int getFeatureIdx(String featureName) {
		for (int p = 0; p < features.length; p++) {
			Feature f = features[p];
			if (featureName.equals(f.getName())) {
				return p;
			}
		}
		return -1;
	}
	
	public void loadFeatureNamesFromExternalResource(InputStream inputStream) throws Exception {
		BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));
		String line;
		int fIdx = 0;
		while ((line = reader.readLine()) != null) {
			if (fIdx >= features.length) {
				throw new Exception("Number of lines in feature names file is more than expected number of features which is " + features.length);
			}
			features[fIdx].setName(line.trim());
			fIdx++;
		}
		reader.close();
	}
}
