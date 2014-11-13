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

public abstract class EvaluationMetric {
	
	private boolean isLargerBetter;
	
	public EvaluationMetric(boolean isLargerBetter) {
		this.isLargerBetter = isLargerBetter;
	}
	
	public abstract double measure(double[] predictions, Sample sample) throws Exception;
	
	public boolean isFirstBetter(double first, double second, double tolerance) {
		if (Double.isNaN(second)) {
			return true;
		}
		if (isLargerBetter) {
			return (first * (1 + tolerance)) > second;
		} else {
			return (first * (1 - tolerance)) < second;
		}
	}
}
