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

public class Accuracy extends EvaluationMetric {
	
	public Accuracy() {
		super(true);
	}
	
	@Override
	public double measure(double[] predictions, Sample sample) {
		int correctCount = 0;
		for (int i = 0; i < sample.size; i++) {
			if (sample.targets[i] == predictions[i]) {
				correctCount++;
			}			
		}		
		return (double) correctCount / sample.size;
	}
}
