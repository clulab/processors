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

package edu.uci.jforestsx.learning.bagging;

import java.util.Map.Entry;

import edu.uci.jforestsx.config.ComponentConfig;
import edu.uci.jforestsx.util.ConfigHolder;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class BaggingConfig extends ComponentConfig {

	/**
	 * Number of bags
	 */
	public int bagCount = 10;
	private final static String BAG_COUNT = "bagging.bag-count";
	
	/**
	 * Fraction of training instances that should be randomly selected
	 * for training of each bag.
	 */
	public double trainFraction = 0.67;
	private final static String TRAIN_FRACTION = "bagging.train-fraction";
	
	/**
	 * Should we perform backfitting during the bagging process?
	 */
	public boolean backfitting = false;
	private final static String BACKFITTING = "bagging.backfitting";	

	public void init(ConfigHolder config) {
		for (Entry<Object, Object> entry : config.getEntries()) {
			String key = ((String) entry.getKey()).toLowerCase();
			String value = (String) entry.getValue();

			if (key.equals(BAG_COUNT)) {
				bagCount = Integer.parseInt(value);
			} else if (key.equals(TRAIN_FRACTION)) {
				trainFraction = Double.parseDouble(value);
			} else if (key.equals(BACKFITTING)) {
				backfitting = Boolean.parseBoolean(value);
			}
		}
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(BAG_COUNT + ": " + bagCount + "\n");
		sb.append(TRAIN_FRACTION + ": " + trainFraction + "\n");
		sb.append(BACKFITTING + ": " + backfitting);
		return sb.toString();
	}
}
