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
import java.io.PrintStream;

import edu.uci.jforestsx.util.Util;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class Raw2BinConvertor {

	protected String inputFile;
	protected String featureStatsFile;
	protected String discreteFile;
	protected String binFile;

	protected BinaryFileGenerator getBinFileGenerator() {
		return new BinaryFileGenerator(discreteFile, featureStatsFile, binFile);
	}

	public void convert(String folder, String[] inputFileNames) throws Exception {
		if (!folder.endsWith("/")) {
			folder += "/";
		}
		featureStatsFile = folder + "jforests-feature-stats.txt";

		File featuresFile = new File(featureStatsFile);
		if (featuresFile.exists()) {
			System.out.println("File: " + featuresFile + " already exists. Skipping it.");
		} else {
			/*
			 * Extract feature statistics
			 */
			FeatureAnalyzer analyzer = new FeatureAnalyzer();
			for (String inputFileName : inputFileNames) {
				analyzer.processFile(folder + inputFileName);
			}
			analyzer.dumpStatistics(new PrintStream(featuresFile));
		}

		for (String inputFileName : inputFileNames) {
			inputFile = folder + inputFileName;
			discreteFile = folder + "jforests-discrete-" + inputFileName;
			binFile = folder + Util.getFileNameWithoutExtension(inputFileName) + ".bin";

			/*
			 * Convert to discrete sparse
			 */
			DiscreteSparseTextFileGenerator.convert(inputFile, featureStatsFile, discreteFile);

			/*
			 * Generate bin file
			 */
			BinaryFileGenerator binFileGenerator = getBinFileGenerator();
			binFileGenerator.convert();
		}
	}

}
