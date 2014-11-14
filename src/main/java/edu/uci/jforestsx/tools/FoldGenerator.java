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

package edu.uci.jforestsx.tools;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class FoldGenerator {

	public void generate(String inputFilename, int folds, String foldsFolder, boolean addTestSet) throws Exception {

		PrintStream[] trainingFiles = new PrintStream[folds];
		PrintStream[] validationFiles = new PrintStream[folds];
		PrintStream[] testFiles;
		if (addTestSet) {
			testFiles = new PrintStream[folds];
		} else {
			testFiles = null;
		}

		for (int f = 0; f < folds; f++) {
			String curFolder = foldsFolder + "/Fold" + (f + 1) + "/";
			new File(curFolder).mkdirs();
			trainingFiles[f] = new PrintStream(curFolder + "train.txt");
			validationFiles[f] = new PrintStream(curFolder + "valid.txt");
			if (addTestSet) {
				testFiles[f] = new PrintStream(curFolder + "test.txt");
			}
		}

		BufferedReader reader = new BufferedReader(new FileReader(new File(inputFilename)));
		String line;
		List<Integer> instanceIds = new ArrayList<Integer>();
		int curId = 0;
		while ((line = reader.readLine()) != null) {
			instanceIds.add(curId);
			curId++;
			if (curId % 10000 == 0) {
				System.out.println("Loaded " + curId + " lines.");
			}			
		}
		reader.close();
		Collections.shuffle(instanceIds);
		List<List<Integer>> partitions = new ArrayList<List<Integer>>();
		for (int f = 0; f < folds; f++) {
			partitions.add(new ArrayList<Integer>());
		}

		Map<Integer, Integer> instanceId2partition = new HashMap<Integer, Integer>();
		for (int i = 0; i < instanceIds.size(); i++) {
			int qid = instanceIds.get(i);
			int partition = i % folds;
			partitions.get(partition).add(qid);
			instanceId2partition.put(qid, partition);
		}

		curId = 0;
		reader = new BufferedReader(new FileReader(new File(inputFilename)));
		while ((line = reader.readLine()) != null) {
			int partition = instanceId2partition.get(curId);

			for (int f = 0; f < folds; f++) {
				if (addTestSet) {
					if (f % folds == partition) {
						validationFiles[f].println(line);
					} else if ((f + 1) % folds == partition) {
						testFiles[f].println(line);
					} else {
						trainingFiles[f].println(line);
					}
				} else {
					if (f % folds == partition) {
						validationFiles[f].println(line);
					} else {
						trainingFiles[f].println(line);
					}
				}
			}
			curId++;
			if (curId % 10000 == 0) {
				System.out.println("Dumped " + curId + " lines.");
			}
		}
		reader.close();

		for (int f = 0; f < folds; f++) {
			trainingFiles[f].close();
			validationFiles[f].close();
		}
	}

}
