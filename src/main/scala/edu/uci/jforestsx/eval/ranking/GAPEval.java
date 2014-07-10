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

package edu.uci.jforestsx.eval.ranking;

/**
 * Graded Average Precision (GAP)
 *
 */

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class GAPEval {

	public static final int GAIN_LEVELS = 5;
	private static double[] g_cumulative = { 0, 0.25, 0.50, 0.75, 1.0 };

	private static int[] getCountPerGrade(int[] labels) {
		int[] countPerGrade = new int[GAIN_LEVELS];
		for (int label : labels) {
			countPerGrade[label]++;
		}
		return countPerGrade;
	}

	private static double getMaxExpectedPrecision(int[] labels) {
		int[] countPerGrade = getCountPerGrade(labels);
		double denom = 0;
		for (int i = 1; i < GAIN_LEVELS; i++) {
			denom += countPerGrade[i] * g_cumulative[i];
		}
		return denom;
	}

	public static double getGAP(int[] labels) {
		double denom = getMaxExpectedPrecision(labels);
		double numerator = 0;
		for (int n = 0; n < labels.length; n++) {
			double sum = 0;
			for (int m = 0; m <= n; m++) {
				if (labels[m] > 0) {
					sum += g_cumulative[(int) Math.min(labels[m], labels[n])];
				}
			}
			numerator += sum / (n + 1);
		}
		return numerator / denom;
	}
	
	public static double getDeltaGAP(int[] labels, int[][] cumulativeLabelCount, int p, int q, double maxExpectedPrecision) {
		double numerator = 0;
		int label_p = labels[p];
		int label_q = labels[q];
		double sum1 = 0;
		double sum2 = 0;		
		if (label_q > 0) {
			for (int i = 1; i < GAIN_LEVELS; i++) {
				if (label_q > i) {
					if (p > 0) {
						sum1 += cumulativeLabelCount[p - 1][i] * g_cumulative[i];
					}
					sum2 -= cumulativeLabelCount[q][i] * g_cumulative[i];
				} else {
					if (p > 0) {
						sum1 += cumulativeLabelCount[p - 1][i] * g_cumulative[label_q];
					}
					sum2 -= cumulativeLabelCount[q][i] * g_cumulative[label_q];
				}
			}
			sum1 += g_cumulative[label_q];
		}
		if (label_p > 0) {
			for (int i = 1; i < GAIN_LEVELS; i++) {
				if (label_p > i) {
					if (p > 0) {
						sum1 -= cumulativeLabelCount[p - 1][i] * g_cumulative[i];
					}
					sum2 += cumulativeLabelCount[q][i] * g_cumulative[i];
				} else {
					if (p > 0) {
						sum1 -= cumulativeLabelCount[p - 1][i] * g_cumulative[label_p];
					}
					sum2 += cumulativeLabelCount[q][i] * g_cumulative[label_p];
				}
			}			
			sum1 -= g_cumulative[label_p];
		}

		numerator += sum1 / (p + 1);
		numerator += sum2 / (q + 1);

		for (int n = p + 1; n <= q - 1; n++) {
			if (labels[n] > 0) {
				double deltaQN = (label_q < labels[n] ? g_cumulative[label_q] : g_cumulative[labels[n]]);
				double deltaPN = (label_p < labels[n] ? g_cumulative[label_p] : g_cumulative[labels[n]]);
				numerator += (deltaQN - deltaPN) / (n + 1);
			}
		}

		return Math.abs(numerator) / maxExpectedPrecision;
	}

	public static double[] getMaxExpectedPrecisionForAllQueries(double[] labels, int[] boundaries, int[][] labelCounts)
			throws Exception {
		double[] maxExpectedPrecision = new double[boundaries.length - 1];

		for (int q = 0; q < boundaries.length - 1; q++) {
			maxExpectedPrecision[q] = 0;
			for (int i = 1; i < GAIN_LEVELS; i++) {
				maxExpectedPrecision[q] += labelCounts[q][i] * g_cumulative[i];
			}
		}
		return maxExpectedPrecision;
	}
	
}
