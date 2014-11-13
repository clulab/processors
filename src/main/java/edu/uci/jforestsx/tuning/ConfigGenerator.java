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

package edu.uci.jforestsx.tuning;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class ConfigGenerator {

	private List<String> configs;
	private String rootFolder;
	private int folds;
	private int fromRandomSeed;
	private int toRandomSeed;
	
	public ConfigGenerator(String rootFolder, int folds, int fromSeed, int toSeed) {
		this.rootFolder = rootFolder;
		this.folds = folds;
		configs = new ArrayList<String>();
		fromRandomSeed = fromSeed;
		toRandomSeed = toSeed;		
	}

	public ConfigGenerator(String rootFolder, int folds, String randomSeeds) {
		this.rootFolder = rootFolder;
		this.folds = folds;
		configs = new ArrayList<String>();
		String[] seedParts = randomSeeds.split(":");
		fromRandomSeed = Integer.parseInt(seedParts[0]);
		if (seedParts.length == 1) {
			toRandomSeed = fromRandomSeed;
		} else {
			toRandomSeed = Integer.parseInt(seedParts[1]);
		}
	}

	private void addLineToConfigs(String line) {
		for (int i = 0; i < configs.size(); i++) {
			configs.set(i, configs.get(i) + line + "\n");
		}
	}

	private void addSingleValueParam(String key, String value) {
		addLineToConfigs(key + "=" + value);
	}

	private void addMultiValueParam(String key, String[] values) {
		List<String> newConfigs = new ArrayList<String>();
		for (String value : values) {
			for (String config : configs) {
				newConfigs.add(config + key + "=" + value + "\n");
			}
		}
		configs = newConfigs;
	}

	public void generateExperimentsConfigs(InputStream inputStream) throws Exception {
		configs.clear();
		configs.add("");
		BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));
		String line;
		while ((line = reader.readLine()) != null) {
			if (line.trim().length() == 0 || line.startsWith("#")) {
				addLineToConfigs(line);
				continue;
			}
			String[] parts = line.trim().split("=");
			String key = parts[0].toLowerCase();
			String value = parts[1];
			if (!value.contains(";")) {
				addSingleValueParam(key, value);
			} else {
				String[] values = value.split(";");
				addMultiValueParam(key, values);
			}
		}
	}

	public List<TuningConfig> getConfigs(String trainFile, String validFile, String testFile, String featureNamesFile) throws Exception {
		List<TuningConfig> tuningConfigs = new ArrayList<TuningConfig>();
		for (int f = 1; f <= folds; f++) {
			tuningConfigs.addAll(getConfigsForFold(f, trainFile, validFile, testFile, featureNamesFile));
		}
		return tuningConfigs;
	}
	
	public List<TuningConfig> getConfigsForFold(int f, String trainFile, String validFile, String testFile, String featureNamesFile) throws Exception {
		List<TuningConfig> tuningConfigs = new ArrayList<TuningConfig>();
		for (int i = 0; i < configs.size(); i++) {
			for (int r = fromRandomSeed; r <= toRandomSeed; r++) {
				TuningConfig config = new TuningConfig();
				config.setId(i);
				config.setFold(f);
				config.setRandomSeed(r);
				config.setConfigKey(configs.get(i));
				String foldRoot;
				if (folds > 1) {
					foldRoot = rootFolder + "Fold" + f + "/";
				} else {
					foldRoot = rootFolder;
				}
				String configText = "id=" + i + "\ninput.train=" + foldRoot + trainFile;
				if (validFile != null) {
					configText += "\ninput.valid=" + foldRoot + validFile;
				}
				if (testFile != null) {
					configText += "\ninput.test=" + foldRoot + testFile;
				}
				if (featureNamesFile != null) {
					configText += "\ninput.train.feature-names-file=" + rootFolder + featureNamesFile;
				}
				configText += "\nparams.random-seed=" + r;
				config.setConfigText(configText + "\n" + configs.get(i));
				tuningConfigs.add(config);
			}
		}
		return tuningConfigs;
	}
}
