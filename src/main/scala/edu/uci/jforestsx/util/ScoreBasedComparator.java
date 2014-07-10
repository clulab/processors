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

package edu.uci.jforestsx.util;

import java.util.Random;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class ScoreBasedComparator {

	public double[] scores;
	public double[] labels;
	public int offset;
	public TieBreaker tieBreaker = TieBreaker.ReverseLabels;
	
	private Random rnd = new Random(1);
	
	public enum TieBreaker {
		ReverseLabels, Random, Labels, Positions
	}

	public int compare(int idx1, int idx2) {
		/*
		 * Compare Scores
		 */
		double s1 = scores[offset + idx1];
		double s2 = scores[offset + idx2];
		if (s1 > s2) {
			return -1;
		} else if (s1 < s2) {
			return 1;
		}

		if (tieBreaker == TieBreaker.ReverseLabels) {
			/*
			 * If scores are the same, compare labels
			 */
			double l1 = labels[offset + idx1];
			double l2 = labels[offset + idx2];
			if (l1 < l2) {
				return -1;
			} else if (l1 > l2) {
				return 1;
			}
		} else if (tieBreaker == TieBreaker.Random) {
			if (rnd.nextDouble() <= 0.5) {
				return -1;
			} else {
				return 1;
			}
		} else if (tieBreaker == TieBreaker.Positions) {
			return (idx1 - idx2);
		}
		return 0;		
	}

}
