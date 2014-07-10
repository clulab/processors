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

package edu.uci.jforestsx.learning.trees.decision;

import edu.uci.jforestsx.util.Constants;
import edu.uci.jforestsx.util.FloatingPointUtil;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class Entropy {

	/**
	 * Computes the entropy of a given distribution.
	 * Entropy is computed as sum(-ci/T log2(ci/T)),
	 * where ci is the i-th element in the distribution
	 * and T is the sum of these elements. The following
	 * method computes the above definition using a faster
	 * computation.
	 * 
	 * @param dist
	 * @return
	 */
	public static double getEntropy(double[] dist) {
		double numerator = 0;
		double total = 0;

		for (int i = 0; i < dist.length; i++) {
			numerator -= entropyLn(dist[i]);
			total += dist[i];
		}
		if (FloatingPointUtil.equal(total, 0)) {
			return 0;
		}
		return (numerator + entropyLn(total)) / (total * Constants.LN2);
	}

	public static double getSplitEntropy(double[] leftDist, double[] rightDist) {
		double numerator = 0;

		double leftTotal = 0;
		for (int i = 0; i < leftDist.length; i++) {
			numerator -= entropyLn(leftDist[i]);
			leftTotal += leftDist[i];
		}
		numerator += entropyLn(leftTotal);

		double rightTotal = 0;
		for (int i = 0; i < rightDist.length; i++) {
			numerator -= entropyLn(rightDist[i]);
			rightTotal += rightDist[i];
		}
		numerator += entropyLn(rightTotal);

		double total = leftTotal + rightTotal;

		if (FloatingPointUtil.equal(total, 0)) {
			return 0;
		}
		return numerator / (total * Constants.LN2);
	}

	/**
	 * Helper function for computing entropy.
	 * 
	 * @param num
	 * 			input value
	 * @return
	 * 			num * ln(num)
	 */
	private static double entropyLn(double num) {
		if (num < 1e-6) {
			return 0;
		} else {
			// TODO: replay log with fastLog
			return num * Math.log(num);
		}
	}
}
