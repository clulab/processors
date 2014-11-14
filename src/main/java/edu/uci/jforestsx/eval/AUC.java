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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import edu.uci.jforestsx.sample.Sample;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class AUC extends EvaluationMetric {

	public AUC() {
		super(true);
	}

	@Override
	public double measure(double[] predictions, Sample sample) {
		int totalPositive = 0;
		int totalNegative = 0;
		List<DoubleDoublePair> sortedProb = new ArrayList<DoubleDoublePair>();
		for (int i = 0; i < sample.size; i++) {
			double label = sample.targets[i];
			sortedProb.add(new DoubleDoublePair(predictions[i], label));
			if (label == 0) {
				totalNegative++;
			} else {
				totalPositive++;
			}
		}
		Collections.sort(sortedProb);

		double fp = 0;
		double tp = 0;
		double fpPrev = 0;
		double tpPrev = 0;
		double area = 0;
		double fPrev = Double.MIN_VALUE;

		int i = 0;
		while (i < sortedProb.size()) {
			DoubleDoublePair pair = sortedProb.get(i);
			double curF = pair.key;
			if (curF != fPrev) {
				area += Math.abs(fp - fpPrev) * ((tp + tpPrev) / 2.0);
				fPrev = curF;
				fpPrev = fp;
				tpPrev = tp;
			}
			double label = pair.value;
			if (label == +1) {
				tp++;
			} else {
				fp++;
			}
			i++;
		}
		area += Math.abs(totalNegative - fpPrev) * ((totalPositive + tpPrev) / 2.0);
		area /= ((double) totalPositive * totalNegative);
		return area;
	}

	private static class DoubleDoublePair implements Comparable<DoubleDoublePair> {
		public double key;
		public double value;

		public DoubleDoublePair(double key, double value) {
			this.key = key;
			this.value = value;
		}

		@Override
		public int compareTo(DoubleDoublePair o) {
			if (this.key > o.key) {
				return -1;
			} else if (this.key < o.key) {
				return 1;
			}
			return 0;
		}
	}
}
