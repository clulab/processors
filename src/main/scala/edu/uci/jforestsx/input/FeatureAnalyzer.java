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

import java.io.*;
import java.util.HashMap;
import java.util.TreeMap;

import edu.uci.jforestsx.input.sparse.FeatureMetaData;
import edu.uci.jforestsx.input.sparse.MetaData;
import edu.uci.jforestsx.input.sparse.MetaLineParser;
import edu.uci.jforestsx.input.sparse.SparseTextFileLine;
import edu.uci.jforestsx.input.sparse.SparseTextFileReader;

/**
 * Extracts feature ranges from sparse textual file.
 */

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class FeatureAnalyzer implements Serializable { // SISTA added Serializable

	private final static int MAX_FEATURE_VALUE = Short.MAX_VALUE - 1;
	
	private TreeMap<Integer, FeatureStatistics> fid2statistics;
	private HashMap<Integer, String> fid2name;
	
	public double[] min;
	public double[] max;
	public double[] factor;
	public boolean[] onLogScale;

	public FeatureAnalyzer() {
		fid2statistics = new TreeMap<Integer, FeatureStatistics>();
		fid2name = new HashMap<Integer, String>();
	}

	public void clear() {
		fid2statistics.clear();
		fid2name.clear();
	}

	public void processFolder(String folder, String extension) {
		File[] files = new File(folder).listFiles();
		for (File file : files) {
			if (file.getAbsolutePath().endsWith(extension)) {
				processFile(file.getAbsolutePath());
			}
		}
	}

	public void processFiles(String folder, String[] files) {
		for (String file : files) {
			processFile(folder + file);
		}
	}

	public void processFile(String inputFile) {
		System.out.println("Processing: " + inputFile);
		SparseTextFileReader reader = new SparseTextFileReader();
		reader.open(inputFile);
		SparseTextFileLine line = new SparseTextFileLine();
		FeatureStatistics stat;
		int count = 0;
		int maxFeatureIndex = 0;
		while (reader.loadNextLine(line)) {
			if (line.meta) {
				MetaData metaData = MetaLineParser.parse(line.content);
				if (metaData instanceof FeatureMetaData) {
					fid2name.put(((FeatureMetaData) metaData).id, ((FeatureMetaData) metaData).name);
				}
			} else {
				int prevIdx = 0;
				for (int i = 0; i < line.numPairs; i++) {
					FeatureValuePair pair = line.pairs[i];
					if (pair.featureIndex != (prevIdx + 1)) {
						for (int f = prevIdx + 1; f < pair.featureIndex; f++) {
							stat = fid2statistics.get(f);
							if (stat != null) {
								if (stat.maxValue < 0) {
									stat.maxValue = 0;
								}
								if (stat.minValue > 0) {
									stat.minValue = 0;
								}
							}
						}
					}
					stat = fid2statistics.get(pair.featureIndex);
					if (stat == null) {
						stat = new FeatureStatistics();
						fid2statistics.put(pair.featureIndex, stat);
						if (count > 0) {
							stat.minValue = 0;
							stat.maxValue = 0;
						}
						if (pair.featureIndex > maxFeatureIndex) {
							maxFeatureIndex = pair.featureIndex;
						}
					}
					if (Double.isInfinite(pair.featureValue)) {
						System.out.println(count + "\t" + pair.featureValue);
					}
					if (pair.featureValue > stat.maxValue) {
						stat.maxValue = pair.featureValue;
					}
					if (pair.featureValue < stat.minValue) {
						stat.minValue = pair.featureValue;
					}					
					prevIdx = pair.featureIndex;
				}
				if (prevIdx < maxFeatureIndex) {
					for (int f = prevIdx + 1; f <= maxFeatureIndex; f++) {
						stat = fid2statistics.get(f);
						if (stat != null) {
							if (stat.maxValue < 0) {
								stat.maxValue = 0;
							}
							if (stat.minValue > 0) {
								stat.minValue = 0;
							}
						}
					}
				}
				count++;
				if (count % 100000 == 0) {
					System.out.println("\t Processed: " + count);
					dumpStatistics(System.out);
				}
			}
		}
		reader.close();
		loadStatistics();		
	}
	
	private void loadStatistics() {
		int featureCount = getFeatureCount();
		min = new double[featureCount];
		max = new double[featureCount];
		factor = new double[featureCount];
		onLogScale = new boolean[featureCount];
		
		for (int f = 0; f < featureCount; f++) {
			FeatureStatistics stat = getStatistics(f + 1);
			if (stat == null) {
				max[f] = min[f] = 0;
			} else {
				min[f] = stat.minValue;
				max[f] = stat.maxValue;	
			}
			double range = max[f] - min[f];
			if (range < MAX_FEATURE_VALUE) {
				factor[f] = MAX_FEATURE_VALUE / range;
			} else {
				factor[f] = MAX_FEATURE_VALUE / Math.log(range + 1);
				onLogScale[f] = true;
			}
		}
	}

	public FeatureStatistics getStatistics(int fid) {
		return fid2statistics.get(fid);
	}
	
	public String getFeatureName(int fid) {
		return fid2name.get(fid);
	}

	public int getFeatureCount() {
		return fid2statistics.lastKey();
	}

	public void loadFeaturesFromFile(String filename) {
		try {
			BufferedReader reader = new BufferedReader(new FileReader(new File(filename)));
			String line = reader.readLine(); // ignore header
			while ((line = reader.readLine()) != null) {
				String[] parts = line.split("\t");
				int fid = Integer.parseInt(parts[0]);
				String name = parts[1];
				FeatureStatistics stat = new FeatureStatistics();
				stat.minValue = Double.parseDouble(parts[2]);
				stat.maxValue = Double.parseDouble(parts[3]);
				fid2statistics.put(fid, stat);
				fid2name.put(fid, name);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		loadStatistics();
	}

	public void dumpStatistics(PrintStream output) {
		output.println("FeatureIndex" + "\tName\tMin" + "\tMax");
		Integer key = fid2statistics.firstKey();
		do {
			FeatureStatistics stat = fid2statistics.get(key);
			String name = fid2name.get(key);
			output.println(key + "\t" + name + "\t" + stat.minValue + "\t" + stat.maxValue);
		} while ((key = fid2statistics.higherKey(key)) != null);
	}

	public void printStatistics() {
		dumpStatistics(System.out);
	}
}
