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

package edu.uci.jforestsx.learning.boosting;

import java.util.Map.Entry;

import edu.uci.jforestsx.config.ComponentConfig;
import edu.uci.jforestsx.util.ConfigHolder;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class GradientBoostingConfig extends ComponentConfig {
	
	private final static String NUM_TREES = "boosting.num-trees";
	public int numTrees = 100;
	
	private final static String LEARNING_RATE = "boosting.learning-rate";
	public double learningRate = 1.0;
	
	private final static String IMBALANCE_COST_ADJUSTMENT = "boosting.imbalance-cost-adjustment";
	public boolean imbalanceCostAdjustment = true;
	
	private final static String SAMPLING_RATE = "boosting.sub-sampling";
	public double samplingRate = 1.0;
	
	private final static String EARLY_STOPPING_TOLERANCE = "boosting.early-stopping-tolerance";
	public double earlyStoppingTolerance = 0.0;
	
	public void init(ConfigHolder config) {
		for (Entry<Object, Object> entry : config.getEntries()) {
			String key = ((String) entry.getKey()).toLowerCase();
			String value = (String) entry.getValue();

			if (key.equals(NUM_TREES)) {
				numTrees = Integer.parseInt(value);
			} else if (key.equals(LEARNING_RATE)) {
				learningRate = Double.parseDouble(value);
			} else if (key.equals(IMBALANCE_COST_ADJUSTMENT)) {
				imbalanceCostAdjustment = value.equals("true");
			} else if (key.equals(SAMPLING_RATE)) {
				samplingRate = Double.parseDouble(value);
			} else if (key.equals(EARLY_STOPPING_TOLERANCE)) {
				earlyStoppingTolerance = Double.parseDouble(value);
			}
		}
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(NUM_TREES + ": " + numTrees + "\n");
		sb.append(LEARNING_RATE + ": " + learningRate + "\n");
		sb.append(IMBALANCE_COST_ADJUSTMENT + ": " + imbalanceCostAdjustment + "\n");
		sb.append(SAMPLING_RATE + ": " + samplingRate);
		sb.append(EARLY_STOPPING_TOLERANCE + ": " + earlyStoppingTolerance + "\n");
		return sb.toString();
	}
}
