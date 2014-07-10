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

import edu.uci.jforestsx.dataset.RankingDataset;
import edu.uci.jforestsx.sample.RankingSample;
import edu.uci.jforestsx.util.ArraysUtil;
import edu.uci.jforestsx.util.ScoreBasedComparator;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class RankGenerator {

	public static int[] getInstanceRanks(double[] scores, RankingSample sample) {
		ScoreBasedComparator comparator = new ScoreBasedComparator();
		comparator.labels = sample.targets;
		comparator.scores = scores;

		int[] ranks = new int[sample.size];
		int[] permutation = new int[((RankingDataset)sample.dataset).maxDocsPerQuery];

		for (int q = 0; q < sample.numQueries; q++) {
			int begin = sample.queryBoundaries[q];
			int numDocs = sample.queryBoundaries[q + 1] - begin;

			comparator.offset = begin;

			for (int d = 0; d < numDocs; d++) {
				permutation[d] = d;
			}			
			ArraysUtil.sort(permutation, numDocs, comparator);
			for (int d = 0; d < numDocs; d++) {
				ranks[begin + permutation[d]] = d;
			}
		}

		return ranks;
	}

	public static int[] getRanks(double[] scores, RankingSample sample) {
		ScoreBasedComparator comparator = new ScoreBasedComparator();
		comparator.labels = sample.targets;
		comparator.scores = scores;

		int[] ranks = new int[sample.size];
		int[] permutation = new int[((RankingDataset)sample.dataset).maxDocsPerQuery];

		for (int q = 0; q < sample.numQueries; q++) {
			int begin = sample.queryBoundaries[q];
			int numDocs = sample.queryBoundaries[q + 1] - begin;

			comparator.offset = begin;

			for (int d = 0; d < numDocs; d++) {
				permutation[d] = d;
			}
			ArraysUtil.sort(permutation, numDocs, comparator);
			System.arraycopy(permutation, 0, ranks, begin, numDocs);
		}

		return ranks;
	}
}
