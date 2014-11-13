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

public class LambdaMARTConfig extends ComponentConfig {
	
	private final static String SIGMOID_BINS = "lambdamart.sigmoid-bins";
	public int sigmoidBins = 1000000;
	
	private final static String MAX_DCG_TRUNCATION = "lambdamart.max-dcg-truncation";
	public int maxDCGTruncation = 50;
	
	private final static String COST_FUNCTION = "lambdamart.cost-function";
	public String costFunction = "cross-entropy";
	
	public void init(ConfigHolder config) {
		for (Entry<Object, Object> entry : config.getEntries()) {
			String key = ((String) entry.getKey()).toLowerCase();
			String value = ((String) entry.getValue()).trim();

			if (key.equals(SIGMOID_BINS)) {
				sigmoidBins = Integer.parseInt(value);
			} else if (key.equals(MAX_DCG_TRUNCATION)) {
				maxDCGTruncation = Integer.parseInt(value);
			} else if (key.equals(COST_FUNCTION)) {
				costFunction = value;
			}
		}
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(SIGMOID_BINS + ": " + sigmoidBins + "\n");
		sb.append(MAX_DCG_TRUNCATION + ": " + maxDCGTruncation + "\n");
		sb.append(COST_FUNCTION + ": " + costFunction);
		return sb.toString();
	}
}
