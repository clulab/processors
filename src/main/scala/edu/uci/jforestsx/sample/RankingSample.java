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

package edu.uci.jforestsx.sample;

import java.util.ArrayList;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Random;

import edu.uci.jforestsx.dataset.RankingDataset;
import edu.uci.jforestsx.eval.ranking.NDCGEval;
import edu.uci.jforestsx.util.ArraysUtil;
import edu.uci.jforestsx.util.Constants;
import edu.uci.jforestsx.util.FloatingPointUtil;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class RankingSample extends Sample {

	public int numQueries;
	public int[] queryBoundaries;
	public int[] queryIndices;

	public RankingSample(RankingDataset dataset) {
		super(dataset);
		queryBoundaries = dataset.queryBoundaries;
		numQueries = queryBoundaries.length - 1;
		queryIndices = new int[numQueries];
		for (int q = 0; q < numQueries; q++) {
			queryIndices[q] = q;
		}
		indicesInParentSample = null;
	}

	public RankingSample(RankingDataset dataset, int[] queryIndices, int[] queryBoundaries, int[] instances, double[] weights, double[] targets,
			int[] indicesInParentSample, int docCount, int queryCount) {
		super(dataset, instances, weights, targets, indicesInParentSample, docCount);
		this.queryIndices = queryIndices;
		this.queryBoundaries = queryBoundaries;
		this.numQueries = queryCount;
	}

	@Override
	public RankingSample getRandomSubSample(double rate, Random rnd) {
		if (rate < 1.0) {
			int subSampleNumQueries = (int) (numQueries * rate);
			int[] tempQueryIndices = new int[numQueries];
			for (int i = 0; i < numQueries; i++) {
				tempQueryIndices[i] = i;
			}
			ArraysUtil.shuffle(tempQueryIndices, rnd);
			Arrays.sort(tempQueryIndices, 0, subSampleNumQueries);
			int[] subSampleQueryBoundaries = new int[subSampleNumQueries + 1];
			int subSampleSize = 0;
			for (int idx = 0; idx < subSampleNumQueries; idx++) {
				int f = tempQueryIndices[idx];
				subSampleSize += queryBoundaries[f + 1] - queryBoundaries[f];
			}
			int[] sampleIndicesInDataset = new int[subSampleSize];
			double[] sampleWeights = new double[subSampleSize];
			double[] sampleTargets = new double[subSampleSize];
			int[] sampleIndicesInParentSample = new int[subSampleSize];
			int curSampleSize = 0;
			int[] subSampleQueryIndices = new int[subSampleNumQueries];
			for (int idx = 0; idx < subSampleNumQueries; idx++) {
				int f = tempQueryIndices[idx];
				subSampleQueryBoundaries[idx] = curSampleSize;
				int beginOffset = queryBoundaries[f];
				int numDocs = queryBoundaries[f + 1] - beginOffset;
				for (int d = 0; d < numDocs; d++) {
					sampleIndicesInDataset[curSampleSize] = indicesInDataset[beginOffset + d];
					sampleWeights[curSampleSize] = weights[beginOffset + d];
					sampleTargets[curSampleSize] = targets[beginOffset + d];
					sampleIndicesInParentSample[curSampleSize] = beginOffset + d;
					curSampleSize++;
				}
				subSampleQueryIndices[idx] = queryIndices[f];
			}
			subSampleQueryBoundaries[subSampleNumQueries] = curSampleSize;
			return new RankingSample((RankingDataset) dataset, subSampleQueryIndices, subSampleQueryBoundaries, sampleIndicesInDataset, sampleWeights,
					sampleTargets, sampleIndicesInParentSample, subSampleSize, subSampleNumQueries);
		} else {
			RankingSample result = this.getClone();
			result.indicesInParentSample = Constants.ONE_TWO_THREE_ETC;
			return result;
		}
	}

	public RankingSample getFilteredSubSample(List<Integer> qids) {
		int subSampleNumQueries = qids.size();
		int[] subSampleQueryBoundaries = new int[subSampleNumQueries + 1];
		int subSampleSize = 0;
		for (int idx = 0; idx < subSampleNumQueries; idx++) {
			int q = qids.get(idx) - 1;
			subSampleSize += queryBoundaries[q + 1] - queryBoundaries[q];
		}
		int[] sampleIndicesInDataset = new int[subSampleSize];
		double[] sampleWeights = new double[subSampleSize];
		double[] sampleTargets = new double[subSampleSize];
		int[] sampleIndicesInParentSample = new int[subSampleSize];
		int curSampleSize = 0;
		int[] subSampleQueryIndices = new int[subSampleNumQueries];
		for (int idx = 0; idx < subSampleNumQueries; idx++) {
			int q = qids.get(idx) - 1;
			subSampleQueryBoundaries[idx] = curSampleSize;
			int beginOffset = queryBoundaries[q];
			int numDocs = queryBoundaries[q + 1] - beginOffset;
			for (int d = 0; d < numDocs; d++) {
				sampleIndicesInDataset[curSampleSize] = indicesInDataset[beginOffset + d];
				sampleWeights[curSampleSize] = weights[beginOffset + d];
				sampleTargets[curSampleSize] = targets[beginOffset + d];
				sampleIndicesInParentSample[curSampleSize] = beginOffset + d;
				curSampleSize++;
			}
			subSampleQueryIndices[idx] = queryIndices[q];
		}
		subSampleQueryBoundaries[subSampleNumQueries] = curSampleSize;
		return new RankingSample((RankingDataset) dataset, subSampleQueryIndices, subSampleQueryBoundaries, sampleIndicesInDataset, sampleWeights,
				sampleTargets, sampleIndicesInParentSample, subSampleSize, subSampleNumQueries);
	}
	
	/**
	 * Creates a sample from queries that are in this sample and their
	 * ids are not listed in the input list of qids.
	 */
	public RankingSample getOutOfSample(List<Integer> qids) {
		List<Integer> oosQids = new ArrayList<Integer>();
		for (int q = 0; q < numQueries; q++) {
			if (!qids.contains(q + 1)) {
				oosQids.add(q + 1);
			}
		}
		return getFilteredSubSample(oosQids);	
	}

	private int addQueryWithDocSampling(int oldSampleQueryIdx, int newSampleQueryIdx, double rate, int curSampleSize, int[] sampleIndicesInDataset,
			double[] sampleWeights, double[] sampleTargets, int[] sampleIndicesInParentSample, Random rnd) {
		int beginOffset = queryBoundaries[oldSampleQueryIdx];
		int numDocs = queryBoundaries[oldSampleQueryIdx + 1] - beginOffset;
		int newDocsAdded = 0;
		int[] tmpLevelCounts = new int[NDCGEval.GAIN_LEVELS];
		for (int d = 0; d < numDocs; d++) {
			if (rnd.nextDouble() <= rate) {
				sampleIndicesInDataset[curSampleSize + newDocsAdded] = indicesInDataset[beginOffset + d];
				sampleWeights[curSampleSize + newDocsAdded] = weights[beginOffset + d];
				sampleTargets[curSampleSize + newDocsAdded] = targets[beginOffset + d];
				tmpLevelCounts[(int) targets[beginOffset + d]]++;
				sampleIndicesInParentSample[curSampleSize + newDocsAdded] = beginOffset + d;
				newDocsAdded++;
			}
		}
		int distinctLevelCounts = 0;
		for (int k = 0; k < tmpLevelCounts.length; k++) {
			if (tmpLevelCounts[k] > 0) {
				distinctLevelCounts++;
			}
		}
		if (distinctLevelCounts > 1) {
			return newDocsAdded;
		} else {
			return 0;
		}
	}

	public RankingSample getAugmentedSampleWithDocSampling(int times, double rate, Random rnd) {
		int newSampleMaxSize = size * (times + 1);
		int newSampleNumQueries = numQueries * (times + 1);
		int[] newSampleIndicesInDataset = new int[newSampleMaxSize];
		double[] newSampleWeights = new double[newSampleMaxSize];
		double[] newSampleTargets = new double[newSampleMaxSize];
		int[] sampleIndicesInParentSample = new int[newSampleMaxSize];
		int curSampleSize = 0;
		int[] newSampleQueryIndices = new int[newSampleNumQueries];
		int[] newSampleQueryBoundaries = new int[newSampleNumQueries];
		int newSampleQueryIdx = 0;
		int newDocsAdded;
		for (int oldSampleQueryIdx = 0; oldSampleQueryIdx < numQueries; oldSampleQueryIdx++) {
			for (int k = 0; k < times + 1; k++) {
				newDocsAdded = addQueryWithDocSampling(oldSampleQueryIdx, newSampleQueryIdx, (k == 0 ? 1.0 : rate), curSampleSize, newSampleIndicesInDataset,
						newSampleWeights, newSampleTargets, sampleIndicesInParentSample, rnd);
				if (newDocsAdded > 0) {
					newSampleQueryBoundaries[newSampleQueryIdx] = curSampleSize;
					newSampleQueryIndices[newSampleQueryIdx] = oldSampleQueryIdx;
					curSampleSize += newDocsAdded;
					newSampleQueryIdx++;
				}
			}
		}
		newSampleQueryBoundaries[newSampleQueryIdx] = curSampleSize;
		return new RankingSample((RankingDataset) dataset, newSampleQueryIndices, newSampleQueryBoundaries, newSampleIndicesInDataset, newSampleWeights,
				newSampleTargets, sampleIndicesInParentSample, curSampleSize, newSampleQueryIdx);
	}

	@Override
	public RankingSample getClone() {
		return new RankingSample((RankingDataset) dataset, queryIndices, queryBoundaries, indicesInDataset, weights, targets, indicesInParentSample, size,
				numQueries);
	}

	public RankingSample getZeroFilteredSample() {
		int[] sampleIndicesInDataset = new int[size];
		double[] sampleWeights = new double[size];
		double[] sampleTargets = new double[size];
		int[] sampleIndicesInParentSample = new int[size];
		int curSampleSize = 0;
		int subSampleNumQueries = 0;
		int[] subSampleQueryBoundaries = new int[queryBoundaries.length];
		int prevSampleSize = 0;
		int[] subSampleQueryIndices = new int[queryBoundaries.length - 1];
		for (int idx = 0; idx < numQueries; idx++) {
			int beginOffset = queryBoundaries[idx];
			int numDocs = queryBoundaries[idx + 1] - beginOffset;
			for (int d = 0; d < numDocs; d++) {
				double target = targets[beginOffset + d];
				if (!FloatingPointUtil.equal(target, 0)) {
					sampleIndicesInDataset[curSampleSize] = indicesInDataset[beginOffset + d];
					sampleWeights[curSampleSize] = weights[beginOffset + d];
					sampleTargets[curSampleSize] = target;
					sampleIndicesInParentSample[curSampleSize] = beginOffset + d;
					curSampleSize++;
				}
			}
			if (curSampleSize > prevSampleSize) {
				subSampleQueryBoundaries[subSampleNumQueries] = prevSampleSize;
				subSampleQueryIndices[subSampleNumQueries] = idx;
				subSampleNumQueries++;
			}
			prevSampleSize = curSampleSize;
		}
		subSampleQueryBoundaries[subSampleNumQueries] = curSampleSize;
		return new RankingSample((RankingDataset) (dataset), subSampleQueryIndices, subSampleQueryBoundaries, sampleIndicesInDataset, sampleWeights,
				sampleTargets, sampleIndicesInParentSample, curSampleSize, subSampleNumQueries);
	}

	/**
	 * Returns a random subsample which is biased with respect
	 * to number of documents per query. It tries to include queries
	 * that have more diverse number of documents
	 */
	public Sample getRandomDocDistBiasedSubSample(double rate, Random rnd) {
		if (rate < 1.0) {
			int minDocsPerQuery = Integer.MAX_VALUE;
			int maxDocsPerQuery = Integer.MIN_VALUE;
			for (int q = 0; q < numQueries; q++) {
				int numDocuments = queryBoundaries[q + 1] - queryBoundaries[q];
				if (numDocuments < minDocsPerQuery) {
					minDocsPerQuery = numDocuments;
				}
				if (numDocuments > maxDocsPerQuery) {
					maxDocsPerQuery = numDocuments;
				}
			}
			int binCount = 20;
			int range = maxDocsPerQuery - minDocsPerQuery + 1;
			double binWidth = range / binCount;
			int[] bins = new int[numQueries];
			int[] binHist = new int[binCount];
			for (int q = 0; q < numQueries; q++) {
				int numDocuments = queryBoundaries[q + 1] - queryBoundaries[q];
				bins[q] = (int) ((numDocuments - minDocsPerQuery) / binWidth);
				if (bins[q] == binCount) {
					bins[q] = binCount - 1;
				}
				binHist[bins[q]]++;
			}
			List<BinFreq> binFreqs = new ArrayList<Sample.BinFreq>();
			for (int i = 0; i < binCount; i++) {
				binFreqs.add(new BinFreq(i, binHist[i]));
			}
			Collections.sort(binFreqs);
			int sampleQueries = (int) (numQueries * rate);
			int remainedQueries = sampleQueries;
			double[] perBinSampleRate = new double[binCount];
			for (int i = 0; i < binCount; i++) {
				BinFreq binFreq = binFreqs.get(i);
				int currentMax = remainedQueries / (binCount - i);
				int selectedSize = Math.min(binFreq.freq, currentMax);
				if (binFreq.freq > 0) {
					perBinSampleRate[binFreq.bin] = (double) selectedSize / binFreq.freq;
				}
				remainedQueries -= selectedSize;
			}

			boolean[] queryIsSelected = new boolean[numQueries];
			int sampleSize = 0;
			sampleQueries = 0;
			for (int q = 0; q < numQueries; q++) {
				if (rnd.nextDouble() <= perBinSampleRate[bins[q]]) {
					queryIsSelected[q] = true;
					sampleSize += (queryBoundaries[q + 1] - queryBoundaries[q]);
					sampleQueries++;
				}
			}

			int[] sampleIndicesInDataset = new int[sampleSize];
			double[] sampleWeights = new double[sampleSize];
			double[] sampleTargets = new double[sampleSize];
			int[] sampleIndicesInParentSample = new int[sampleSize];
			int curSampleSize = 0;
			int subSampleNumQueries = 0;
			int[] subSampleQueryBoundaries = new int[sampleQueries + 1];
			int prevSampleSize = 0;
			int[] subSampleQueryIndices = new int[sampleQueries];
			for (int q = 0; q < numQueries; q++) {
				if (!queryIsSelected[q]) {
					continue;
				}
				int beginOffset = queryBoundaries[q];
				int numDocs = queryBoundaries[q + 1] - beginOffset;
				for (int d = 0; d < numDocs; d++) {
					sampleIndicesInDataset[curSampleSize] = indicesInDataset[beginOffset + d];
					sampleWeights[curSampleSize] = weights[beginOffset + d];
					sampleTargets[curSampleSize] = targets[beginOffset + d];
					sampleIndicesInParentSample[curSampleSize] = beginOffset + d;
					curSampleSize++;
				}
				if (curSampleSize > prevSampleSize) {
					subSampleQueryBoundaries[subSampleNumQueries] = prevSampleSize;
					subSampleQueryIndices[subSampleNumQueries] = q;
					subSampleNumQueries++;
				}
				prevSampleSize = curSampleSize;
			}
			subSampleQueryBoundaries[subSampleNumQueries] = curSampleSize;
			return new RankingSample((RankingDataset) (dataset), subSampleQueryIndices, subSampleQueryBoundaries, sampleIndicesInDataset, sampleWeights,
					sampleTargets, sampleIndicesInParentSample, curSampleSize, subSampleNumQueries);
		} else {
			Sample result = this.getClone();
			result.indicesInParentSample = Constants.ONE_TWO_THREE_ETC;
			return result;
		}
	}

	public void printDocsPerQuery() {
		for (int q = 0; q < numQueries; q++) {
			int numDocs = queryBoundaries[q + 1] - queryBoundaries[q];
			System.out.println(numDocs);
		}
	}
}
