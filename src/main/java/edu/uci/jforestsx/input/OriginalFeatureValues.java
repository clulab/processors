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

package edu.uci.jforestsx.input;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class OriginalFeatureValues {
	
	private final static int MAX_FEATURE_VALUE = Short.MAX_VALUE - 1;
	private double[] min;
	private double[] factor;
	private boolean[] onLogScale;

	public OriginalFeatureValues(String featuresStatFile) {
		FeatureAnalyzer featureAnalyzer = new FeatureAnalyzer();
		featureAnalyzer.loadFeaturesFromFile(featuresStatFile);

		int featureCount = featureAnalyzer.getFeatureCount();
		min = new double[featureCount];
		double[] max = new double[featureCount];
		for (int f = 0; f < featureCount; f++) {
			FeatureStatistics stat = featureAnalyzer.getStatistics(f + 1);
			if (stat == null) {
				max[f] = min[f] = 0;
			} else {
				min[f] = stat.minValue;
				max[f] = stat.maxValue;
			}
		}

		factor = new double[featureCount];
		onLogScale = new boolean[featureCount];
		for (int i = 0; i < featureCount; i++) {
			double range = max[i] - min[i];
			if (range < MAX_FEATURE_VALUE) {
				factor[i] = MAX_FEATURE_VALUE / range;
			} else {
				factor[i] = MAX_FEATURE_VALUE / Math.log(range + 1);
				onLogScale[i] = true;
			}
		}
	}

	public double getOriginalFeatureValue(int f, double value) {
		value /= factor[f];
		if (onLogScale[f]) {
			value = Math.exp(value) - 1 + min[f];
		} else {
			value += min[f];
		}
		return value;
	}
}
