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

import java.util.Map.Entry;

import edu.uci.jforestsx.config.ComponentConfig;
import edu.uci.jforestsx.util.ConfigHolder;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class TreesConfig extends ComponentConfig {
	
	public int numLeaves = 50;
	private final static String NUM_LEAVES = "trees.num-leaves";
	
	public double minInstancePercentagePerLeaf = 0.25;
	private final static String MIN_INSTANCE_PERCENTAGE_PER_LEAF = "trees.min-instance-percentage-per-leaf";
	
	public double featureSamplingPerSplit = 1.0;
	private final static String SPLIT_SAMPLING = "trees.feature-sampling";
	
	public boolean randomizedSplits = false;
	private final static String RANDOMIZED_SPLITS = "trees.randomized-splits";
	
	/**
	 * If this parameter is set to a not null value, the listed features will be
	 * discarded from the training.
	 */
	public String featuresToDiscard = null;
	private final static String FEATURES_TO_DISCARD = "trees.features-to-discard";
	
	/**
	 * If this parameter is set to a not null value, only the listed features will be
	 * included in the training.
	 */
	public String featuresToInclude = null;
	private final static String FEATURES_TO_INCLUDE = "trees.features-to-include";
	

	public void init(ConfigHolder config) {
		for (Entry<Object, Object> entry : config.getEntries()) {
			String key = ((String) entry.getKey()).toLowerCase();
			String value = (String) entry.getValue();

			if (key.equals(SPLIT_SAMPLING)) {
				featureSamplingPerSplit = Double.parseDouble(value);
			} else if (key.equals(RANDOMIZED_SPLITS)) {
				randomizedSplits = value.equals("true");
			} else if (key.equals(NUM_LEAVES)) {
				numLeaves = Integer.parseInt(value);
			} else if (key.equals(MIN_INSTANCE_PERCENTAGE_PER_LEAF)) {
				minInstancePercentagePerLeaf = Double.parseDouble(value);
			} else if (key.equals(FEATURES_TO_DISCARD)) {
				featuresToDiscard = value;
			} else if (key.equals(FEATURES_TO_INCLUDE)) {
				featuresToInclude = value;
			}
		}
	}

	@Override
	public String toString() {		
		StringBuilder sb = new StringBuilder();
		sb.append(NUM_LEAVES + ": " + numLeaves + "\n");
		sb.append(MIN_INSTANCE_PERCENTAGE_PER_LEAF + ": " + minInstancePercentagePerLeaf + "\n");
		sb.append(SPLIT_SAMPLING + ": " + featureSamplingPerSplit + "\n");
		sb.append(RANDOMIZED_SPLITS + ": " + randomizedSplits + "\n");
		sb.append(FEATURES_TO_DISCARD + ": " + featuresToDiscard);
		return sb.toString();
	}
}
