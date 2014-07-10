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

package edu.uci.jforestsx.eval;

import edu.uci.jforestsx.sample.Sample;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class BalancedYoundenIndex extends EvaluationMetric {
	
	private static final double THRESHOLD = 0.5;
	
	public BalancedYoundenIndex() {
		super(true);
	}
	
	@Override
	public double measure(double[] predictions, Sample sample) {
		int tp = 0;
		int tn = 0;
		int fn = 0;
		int fp = 0;
		for (int i = 0; i < sample.size; i++) {
			double target = sample.targets[i];
			double pred = predictions[i];
			if (target > THRESHOLD) {
				if (pred > THRESHOLD) {
					tp++;
				} else {
					fn++;
				}
			} else {
				if (pred > THRESHOLD) {
					fp++;
				} else {
					tn++;
				}
			}
		}
		
		double sensitivity = (double) tp / (tp + fn);
		double specificity = (double) tn / (tn + fp);
		
		return Math.min(sensitivity, specificity);
	}
}
