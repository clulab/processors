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

import edu.uci.jforestsx.dataset.Dataset;
import edu.uci.jforestsx.eval.EvaluationMetric;
import edu.uci.jforestsx.util.ArraysUtil;
import edu.uci.jforestsx.util.Constants;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class Sample {
	public Dataset dataset;
	public int[] indicesInDataset;
	public double[] weights;
	public double[] targets;
	public int size;

	// Only used in sub samples
	public int[] indicesInParentSample;

	public Sample(Dataset dataset) {
		this.dataset = dataset;
		size = dataset.numInstances;

		Constants.init(size);
		indicesInDataset = Constants.ONE_TWO_THREE_ETC;
		weights = Constants.DOUBLE_ONE_ONE_ONE_ETC;
		targets = dataset.targets;
	}

	public Sample(Dataset dataset, int[] indicesInDataset, double[] weights, double[] targets,
			int[] indicesInParentSample, int size) {
		this.dataset = dataset;
		this.indicesInDataset = indicesInDataset;
		this.weights = weights;
		this.targets = targets;
		this.size = size;
		this.indicesInParentSample = indicesInParentSample;
	}

	public Sample getClone() {
		return new Sample(dataset, indicesInDataset, weights, targets, indicesInParentSample, size);
	}
	
	public Sample getRandomSubSample(double rate, Random rnd) {
		if (rate < 1.0) {
			int sampleSize = (int) (size * rate);
			int[] tempIndices = new int[size];
			for (int i = 0; i < size; i++) {
				tempIndices[i] = i;
			}
			ArraysUtil.shuffle(tempIndices, rnd);
			Arrays.sort(tempIndices, 0, sampleSize);

			int[] sampleIndicesInDataset = new int[sampleSize];
			double[] sampleWeights = new double[sampleSize];
			double[] sampleTargets = new double[sampleSize];
			int[] sampleIndicesInParentSample = new int[sampleSize];
			int curSampleSize = 0;
			for (int idx = 0; idx < sampleSize; idx++) {
				int f = tempIndices[idx];
				sampleIndicesInDataset[curSampleSize] = indicesInDataset[f];
				sampleWeights[curSampleSize] = weights[f];
				sampleTargets[curSampleSize] = targets[f];
				sampleIndicesInParentSample[curSampleSize] = f;
				curSampleSize++;
			}
			return new Sample(dataset, sampleIndicesInDataset, sampleWeights, sampleTargets,
					sampleIndicesInParentSample, sampleSize);
		} else {
			Sample result = this.getClone();
			result.indicesInParentSample = Constants.ONE_TWO_THREE_ETC;
			return result;
		}
	}

	public class BinFreq implements Comparable<BinFreq> {
		public int bin;
		public int freq;

		public BinFreq(int bin, int freq) {
			this.bin = bin;
			this.freq = freq;
		}

		@Override
		public int compareTo(BinFreq o) {
			return freq - o.freq;
		}
	}

	/**
	 * Returns a random subsample that is biased with respect to 
	 * targets. In other words it tries to include items with
	 * more diverse targets
	 */
	public Sample getRandomTargetBiasedSubSample(double rate, Random rnd) {
		if (rate < 1.0) {
			double minTarget = Double.POSITIVE_INFINITY;
			double maxTarget = Double.NEGATIVE_INFINITY;
			for (int i = 0; i < size; i++) {
				if (targets[i] < minTarget) {
					minTarget = targets[i];
				}
				if (targets[i] > maxTarget) {
					maxTarget = targets[i];
				}
			}
			int binCount = 20;
			double range = maxTarget - minTarget + 1;
			double binWidth = range / binCount;
			int[] bins = new int[size];
			int[] binHist = new int[binCount];
			for (int i = 0; i < size; i++) {
				bins[i] = (int) ((targets[i] - minTarget) / binWidth);
				binHist[bins[i]]++;
			}
			List<BinFreq> binFreqs = new ArrayList<Sample.BinFreq>();
			for (int i = 0; i < binCount; i++) {
				binFreqs.add(new BinFreq(i, binHist[i]));
			}
			Collections.sort(binFreqs);
			int sampleSize = (int) (size * rate);
			int remainedSize = sampleSize;
			double[] perBinSampleRate = new double[binCount];
			for (int i = 0; i < binCount; i++) {
				BinFreq binFreq = binFreqs.get(i);
				int currentMax = remainedSize / (binCount - i);
				int selectedSize = Math.min(binFreq.freq, currentMax);
				if (binFreq.freq > 0) {
					perBinSampleRate[binFreq.bin] = (double) selectedSize / binFreq.freq;
				}
				remainedSize -= selectedSize;
			}

			int[] sampleIndicesInDataset = new int[sampleSize];
			double[] sampleWeights = new double[sampleSize];
			double[] sampleTargets = new double[sampleSize];
			int[] sampleIndicesInParentSample = new int[sampleSize];
			int curSampleSize = 0;

			int[] tempIndices = new int[size];

			for (int i = 0; i < binCount; i++) {
				BinFreq binFreq = binFreqs.get(i);
				if (binFreq.freq == 0) {
					continue;
				}
				int selectedSize = (int) (perBinSampleRate[binFreq.bin] * binHist[binFreq.bin]);
				int idx = 0;
				for (int j = 0; j < size; j++) {
					if (bins[j] == binFreq.bin) {
						tempIndices[idx] = j;
						idx++;
					}
				}
				ArraysUtil.shuffle(tempIndices, binHist[binFreq.bin], rnd);
				Arrays.sort(tempIndices, 0, selectedSize);

				for (idx = 0; idx < selectedSize; idx++) {
					int f = tempIndices[idx];
					sampleIndicesInDataset[curSampleSize] = indicesInDataset[f];
					sampleWeights[curSampleSize] = weights[f];
					sampleTargets[curSampleSize] = targets[f];
					sampleIndicesInParentSample[curSampleSize] = f;
					curSampleSize++;
				}
			}

			// Sort observations based on their indices in original dataset
			for (int i = 0; i < sampleSize; i++) {
				int minIdx = i;
				for (int j = i + 1; j < sampleSize; j++) {
					if (sampleIndicesInDataset[j] < sampleIndicesInDataset[minIdx]) {
						minIdx = j;
					}
				}
				ArraysUtil.swap(sampleIndicesInDataset, i, minIdx);
				ArraysUtil.swap(sampleWeights, i, minIdx);
				ArraysUtil.swap(sampleTargets, i, minIdx);
				ArraysUtil.swap(sampleIndicesInParentSample, i, minIdx);
			}
			return new Sample(dataset, sampleIndicesInDataset, sampleWeights, sampleTargets,
					sampleIndicesInParentSample, curSampleSize);
		} else {
			Sample result = this.getClone();
			result.indicesInParentSample = Constants.ONE_TWO_THREE_ETC;
			return result;
		}
	}
	
	public double evaluate(double[] predictions, EvaluationMetric evaluationMetric) throws Exception {
		return evaluationMetric.measure(predictions, this);
	}
	
	public double evaluate(double[] predictions, EvaluationMetric evaluationMetric, double factor) throws Exception {
		double[] newPredictions = new double[predictions.length];
		for (int i = 0; i < predictions.length; i++) {
			newPredictions[i] = predictions[i] * factor;
		}
		return evaluationMetric.measure(newPredictions, this);
	}

	public boolean isEmpty() {
		return size == 0;
	}

	/**
	 * Creates a sample from instances that are in this sample and not in
	 * subSample
	 */
	public Sample getOutOfSample(Sample subSample) {
		int oosSize = size - subSample.size;
		int[] oosIndicesInDataset = new int[oosSize];
		double[] oosWeights = new double[oosSize];
		double[] oosTargets = new double[oosSize];
		int samplePtr = 0;
		int oosCurSize = 0;
		for (int i = 0; i < size; i++) {
			if (subSample.indicesInDataset[samplePtr] > indicesInDataset[i]) {
				oosIndicesInDataset[oosCurSize] = indicesInDataset[i];
				oosWeights[oosCurSize] = weights[i];
				oosTargets[oosCurSize] = targets[i];
				oosCurSize++;
			} else if (subSample.indicesInDataset[samplePtr] == indicesInDataset[i]) {
				samplePtr++;
				if (samplePtr >= subSample.size) {
					break;
				}
			}
		}
		return new Sample(dataset, oosIndicesInDataset, oosWeights, oosTargets, null, oosSize);
	}
}
