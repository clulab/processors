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

package edu.uci.jforestsx.input;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import edu.uci.jforestsx.dataset.BitNumericArray;
import edu.uci.jforestsx.dataset.ByteNumericArray;
import edu.uci.jforestsx.dataset.NullNumericArray;
import edu.uci.jforestsx.dataset.ShortNumericArray;
import edu.uci.jforestsx.dataset.Feature;
import edu.uci.jforestsx.dataset.NumericArray;
import edu.uci.jforestsx.input.sparse.SparseTextFileLine;
import edu.uci.jforestsx.input.sparse.SparseTextFileReader;
import edu.uci.jforestsx.util.ArraysUtil;
import edu.uci.jforestsx.util.Timer;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class BinaryFileGenerator {

	private List<HashMap<Integer, Integer>> valueHashMaps;
	private int[][] valueDistributions;
	private int[] totalCount;
	private NumericArray[] bins;

	protected double[] targets;
	protected Feature[] features;

	private FeatureAnalyzer featureAnalyzer;
	private int featureCount;
	private int instanceCount;

	private String textFile;
	private String featuresStatFile;
	private Timer timer;

	protected String binFile;
	protected BinaryFileWriter writer;

	public BinaryFileGenerator(String textFile, String featuresStatFile, String binFile) {
		this.textFile = textFile;
		this.featuresStatFile = featuresStatFile;
		this.binFile = binFile;
		timer = new Timer();
	}

	protected void handle(SparseTextFileLine line) {
		// Subclasses will override this function if needed
	}

	protected void loadValueHashMaps() {
		timer.start();
		System.out.print("Loading values...");
		valueHashMaps = new ArrayList<HashMap<Integer, Integer>>(featureCount);
		for (int f = 0; f < featureCount; f++) {
			valueHashMaps.add(new HashMap<Integer, Integer>());
		}

		SparseTextFileReader reader = new SparseTextFileReader();
		reader.open(textFile);
		SparseTextFileLine line = new SparseTextFileLine();
		HashMap<Integer, Integer> curMap;
		int key;
		instanceCount = 0;
		while (reader.loadNextLine(line)) {
			if (line.meta) {
				continue;
			}
			for (int i = 0; i < line.numPairs; i++) {
				FeatureValuePair pair = line.pairs[i];
				curMap = valueHashMaps.get(pair.featureIndex - 1);
				key = (int) pair.featureValue;
				Integer count = curMap.get(key);
				if (count == null) {
					count = 0;
				}
				count++;
				curMap.put(key, count);
			}
			handle(line);
			instanceCount++;
		}
		reader.close();
		System.out.println("  [Done in: " + timer.getElapsedSeconds() + " seconds.]");
	}

	private void makeDistributions() {
		timer.start();
		System.out.print("Making distributions...");
		valueDistributions = new int[featureCount][];
		totalCount = new int[featureCount];

		List<Integer> valueMap = new ArrayList<Integer>();
		HashMap<Integer, Integer> curMap;
		for (int f = 0; f < featureCount; f++) {
			valueMap.clear();
			curMap = valueHashMaps.get(f);
			totalCount[f] = 0;
			for (Entry<Integer, Integer> entry : curMap.entrySet()) {
				valueMap.add(entry.getKey());
				totalCount[f] += entry.getValue();
			}

			if (!valueMap.contains(0)) {
				valueMap.add(0);
			}
			Collections.sort(valueMap);
			valueDistributions[f] = ArraysUtil.toArray(valueMap);
		}
		System.out.println("  [Done in: " + timer.getElapsedSeconds() + " seconds.]");
	}

	private void makeBins() throws Exception {
		System.out.println("Making bins...");
		timer.start();
		bins = new NumericArray[featureCount];
		for (int i = 0; i < featureCount; i++) {
			int numValues = valueDistributions[i].length;
			if (numValues == 1 && valueDistributions[i][0] == 0) {
				bins[i] = NullNumericArray.getInstance();
			} else if (numValues <= 2) {
				bins[i] = new BitNumericArray(instanceCount);
			} else if (numValues <= Byte.MAX_VALUE) {
				bins[i] = new ByteNumericArray(instanceCount);
			} else if (numValues <= Short.MAX_VALUE) {
				bins[i] = new ShortNumericArray(instanceCount);
			} else {
				throw new Exception("One of your features have more than " + Short.MAX_VALUE
						+ " distinct values. The support for this feature is not implemented yet.");
			}
			System.out.println("Feature: " + i + ", type: " + bins[i].getType().toString());
		}

		targets = new double[instanceCount];
		int[] zeroCount = new int[featureCount];

		SparseTextFileReader reader = new SparseTextFileReader();
		reader.open(textFile);
		SparseTextFileLine line = new SparseTextFileLine();
		int instanceIdx = 0;
		while (reader.loadNextLine(line)) {
			if (line.meta) {
				continue;
			}
			targets[instanceIdx] = line.target;
			for (int i = 0; i < line.numPairs; i++) {
				FeatureValuePair pair = line.pairs[i];
				int fidx = pair.featureIndex - 1;
				int index = Arrays.binarySearch(valueDistributions[fidx], (int) pair.featureValue);
				bins[fidx].set(instanceIdx, index);
				if (index == 0) {
					zeroCount[fidx]++;
				}
			}
			instanceIdx++;
		}
		reader.close();
		System.out.println("  [Done in: " + timer.getElapsedSeconds() + " seconds.]");
	}

	private void makeFeatures() {
		System.out.print("Making features...");
		timer.start();
		features = new Feature[featureCount];
		for (int f = 0; f < featureCount; f++) {
			features[f] = new Feature();
			features[f].bins = bins[f];
			features[f].upperBounds = valueDistributions[f];
			features[f].setName(featureAnalyzer.getFeatureName(f + 1));
			features[f].setMin(featureAnalyzer.min[f]);
			features[f].setMax(featureAnalyzer.max[f]);
			features[f].setFactor(featureAnalyzer.factor[f]);
			features[f].setOnLogScale(featureAnalyzer.onLogScale[f]);
		}
		System.out.println("  [Done in: " + timer.getElapsedSeconds() + " seconds.]");
	}

	protected void createBinFile() {
		writer = new BinaryFileWriter(binFile, features, targets);
	}

	private void writeBinFile() {
		timer.start();
		System.out.print("Creating bin file...");
		writer.write();
		writer.close();
		System.out.println("  [Done in: " + timer.getElapsedSeconds() + " seconds.]");
	}

	public void convert() throws Exception {
		if (new File(binFile).exists()) {
			System.out.println("File: " + binFile + " already exists. Skipping it.");
			return;
		}
		featureAnalyzer = new FeatureAnalyzer();
		featureAnalyzer.loadFeaturesFromFile(featuresStatFile);
		featureCount = featureAnalyzer.getFeatureCount();

		loadValueHashMaps();
		makeDistributions();
		makeBins();
		makeFeatures();
		createBinFile();
		writeBinFile();
	}

  /**
   * SISTA added code: Converts the discrete features of a datum to Feature objects suitable for classification
   * This functionality must be aligned with the convert() method, which works on an entire dataset
   * @param discreteSparseFeatures
   * @param featureAnalyzer
   * @return
   */
  public static Feature [] convert(FeatureValuePair [] discreteSparseFeatures, FeatureAnalyzer featureAnalyzer) {
    int featureCount = featureAnalyzer.getFeatureCount();
    List<HashMap<Integer, Integer>> valueHashMaps = mkValueHashMaps(featureCount, discreteSparseFeatures);
    int[][] valueDistributions = mkDistributions(valueHashMaps, featureCount);
    NumericArray[] bins = mkBins(valueDistributions, featureCount, discreteSparseFeatures);
    Feature[] features = mkFeatures(valueDistributions, bins, featureCount);
    return features;
  }

  /** SISTA added code; aligned with makeFeatures */
  private static Feature[] mkFeatures(
    int[][] valueDistributions,
    NumericArray[] bins,
    int featureCount) {

    Feature[] features = new Feature[featureCount];
    for (int f = 0; f < featureCount; f++) {
      features[f] = new Feature();
      features[f].bins = bins[f];
      features[f].upperBounds = valueDistributions[f];
    }
    return features;
  }

  /** SISTA added code; aligned with makeBins */
  private static NumericArray[] mkBins(
    int[][] valueDistributions,
    int featureCount,
    FeatureValuePair [] discreteSparseFeatures) {

    NumericArray[] bins = new NumericArray[featureCount];
    for (int i = 0; i < featureCount; i++) {
      int numValues = valueDistributions[i].length;
      if (numValues == 1 && valueDistributions[i][0] == 0) {
        bins[i] = NullNumericArray.getInstance();
      } else if (numValues <= 2) {
        bins[i] = new BitNumericArray(1);
      } else {
        throw new RuntimeException("One of your features have more than 2"
          + " distinct values. The support for this feature is not implemented yet.");
      }
    }

    for(int i = 0; i < discreteSparseFeatures.length; i ++) {
      int fidx = discreteSparseFeatures[i].featureIndex;
      double value = discreteSparseFeatures[i].featureValue;
      int index = Arrays.binarySearch(valueDistributions[fidx], (int) value);
      bins[fidx].set(0, index);
    }

    return bins;
  }

  /** SISTA added code; aligned with loadValueHashMaps */
  private static List<HashMap<Integer, Integer>> mkValueHashMaps(
    int featureCount,
    FeatureValuePair [] discreteSparseFeatures) {

    List<HashMap<Integer, Integer>> valueHashMaps =
      new ArrayList<HashMap<Integer, Integer>>(featureCount);
    for (int f = 0; f < featureCount; f++) {
      valueHashMaps.add(new HashMap<Integer, Integer>());
    }
    for(int i = 0; i < discreteSparseFeatures.length; i ++) {
      int idx = discreteSparseFeatures[i].featureIndex;
      int key = (int) discreteSparseFeatures[i].featureValue;
      HashMap<Integer, Integer> curMap = valueHashMaps.get(idx);
      Integer count = curMap.get(key);
      if (count == null) {
        count = 0;
      }
      count++;
      curMap.put(key, count);
    }
    return valueHashMaps;
  }

  /** SISTA added code; aligned with makeDistributions */
  private static int[][] mkDistributions(
    List<HashMap<Integer, Integer>> valueHashMaps,
    int featureCount) {

    int [][] valueDistributions = new int[featureCount][];
    List<Integer> valueMap = new ArrayList<Integer>();
    HashMap<Integer, Integer> curMap;
    for (int f = 0; f < featureCount; f++) {
      valueMap.clear();
      curMap = valueHashMaps.get(f);
      for (Entry<Integer, Integer> entry : curMap.entrySet()) {
        valueMap.add(entry.getKey());
      }
      if (!valueMap.contains(0)) {
        valueMap.add(0);
      }
      Collections.sort(valueMap);
      valueDistributions[f] = ArraysUtil.toArray(valueMap);
    }
    return valueDistributions;
  }
}
