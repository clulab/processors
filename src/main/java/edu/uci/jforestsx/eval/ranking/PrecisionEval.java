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

import java.util.Arrays;

import edu.uci.jforestsx.eval.EvaluationMetric;
import edu.uci.jforestsx.sample.RankingSample;
import edu.uci.jforestsx.sample.Sample;
import edu.uci.jforestsx.util.ArraysUtil;
import edu.uci.jforestsx.util.ScoreBasedComparator;
import edu.uci.jforestsx.util.ScoreBasedComparator.TieBreaker;
import edu.uci.jforestsx.util.concurrency.BlockingThreadPoolExecutor;
import edu.uci.jforestsx.util.concurrency.TaskCollection;
import edu.uci.jforestsx.util.concurrency.TaskItem;

/**
 * Precision at position k (P@k)
 */

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class PrecisionEval extends EvaluationMetric {

	private TaskCollection<PrecisionWorker> mapWorkers;
	private int maxLevels;
	double relevancyThreshold;
	private int maxDocsPerQuery;

	public PrecisionEval(int maxDocsPerQuery, int maxLevels, double relevancyThreshold) throws Exception {
		super(true);
		this.maxDocsPerQuery = maxDocsPerQuery;
		this.maxLevels = maxLevels;
		this.relevancyThreshold = relevancyThreshold;
		int numWorkers = BlockingThreadPoolExecutor.getInstance().getMaximumPoolSize();
		mapWorkers = new TaskCollection<PrecisionEval.PrecisionWorker>();
		for (int i = 0; i < numWorkers; i++) {
			mapWorkers.addTask(new PrecisionWorker());
		}		
	}

	private class PrecisionWorker extends TaskItem {

		private int[] permutation;
		private RankingSample sample;
		private int beginIdx;
		private int endIdx;
		private double[] result;
		private ScoreBasedComparator comparator;

		public PrecisionWorker() {
			permutation = new int[maxDocsPerQuery];
			comparator = new ScoreBasedComparator();
			result = new double[maxLevels];
		}

		public void init(RankingSample sample, double[] scores, int beginIdx, int endIdx, TieBreaker tieBreaker) {
			this.sample = sample;
			this.beginIdx = beginIdx;
			this.endIdx = endIdx;
			comparator.labels = sample.targets;
			comparator.scores = scores;
			comparator.tieBreaker = tieBreaker;
			Arrays.fill(result, 0);
		}

		public double[] getResult() {
			return result;
		}

		@Override
		public void run() {

			for (int q = beginIdx; q < endIdx; q++) {
				int begin = sample.queryBoundaries[q];
				int numDocs = sample.queryBoundaries[q + 1] - begin;

				comparator.offset = begin;

				for (int d = 0; d < numDocs; d++) {
					permutation[d] = d;
				}				
				ArraysUtil.sort(permutation, numDocs, comparator);
				
				try {
					int numRelevant = 0;
					for (int pos = 0; pos < Math.min(numDocs, maxLevels); pos++) {
						if (sample.targets[begin + permutation[pos]] >= relevancyThreshold) {
							numRelevant++;							
						}					
						result[pos] += ((double) numRelevant / (pos + 1));
					}					
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}
	}

	public double[] getPrecisions(double[] predictions, Sample sample, TieBreaker tieBreaker) throws Exception {
		RankingSample rankingSample = (RankingSample) sample;

		int chunkSize = 1 + (rankingSample.numQueries / mapWorkers.getSize());
		int offset = 0;
		int workerCount = 0;
		for (int i = 0; i < mapWorkers.getSize() && offset < rankingSample.numQueries; i++) {
			int endOffset = offset + Math.min(rankingSample.numQueries - offset, chunkSize);
			PrecisionWorker worker = mapWorkers.getTask(i);
			workerCount++;
			worker.init(rankingSample, predictions, offset, endOffset, tieBreaker);
			BlockingThreadPoolExecutor.getInstance().execute(worker);
			offset += chunkSize;
		}
		BlockingThreadPoolExecutor.getInstance().await();

		double[] result = new double[maxLevels];
		for (int i = 0; i < workerCount; i++) {
			double[] localResult = mapWorkers.getTask(i).getResult();
			for (int p = 0; p < maxLevels; p++) {
				result[p] += localResult[p];
			}			
		}
		
		for (int p = 0; p < maxLevels; p++) {
			result[p] /= rankingSample.numQueries;
		}		
		return result;
	}
	
	@Override
	public double measure(double[] predictions, Sample sample) throws Exception {
		return getPrecisions(predictions, sample, TieBreaker.ReverseLabels)[0];
	}
}
