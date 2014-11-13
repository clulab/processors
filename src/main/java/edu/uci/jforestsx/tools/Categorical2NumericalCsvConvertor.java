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
import java.util.HashMap;
import java.util.Map;

/**
 * Sometimes an input file contains categorical features
 * where the name of categories is specified using string
 * names. Given that jforests only works with numeric values,
 * this tool can be used as a pre-processing step to convert
 * these values to ordinal values (0, 1, 2, ...)
 *
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class Categorical2NumericalCsvConvertor {
	
	Map<Integer, Map<String, Integer>> perFeatureMappings;
	
	private int getOrdinal(String value, int fIdx) {
		Map<String, Integer> featureMap = perFeatureMappings.get(fIdx);
		if (featureMap == null) {
			featureMap = new HashMap<String, Integer>();
			perFeatureMappings.put(fIdx, featureMap);
		}
		Integer ordinal = featureMap.get(value);
		if (ordinal == null) {
			int maxOrdinal = 0;
			for (int curOrdinal : featureMap.values()) {
				if (curOrdinal > maxOrdinal) {
					maxOrdinal = curOrdinal;
				}
			}
			ordinal = maxOrdinal + 1;
			featureMap.put(value, ordinal);
		}
		return ordinal;
	}

	public void convert(String inputFilename, String outputFilename, boolean hasHeader) {
		try {
			File outputFile = new File(outputFilename);
			if (outputFile.exists()) {
				System.out.println("File: " + outputFilename + " already exists. Skipping it.");
				return;
			}
			BufferedReader reader = new BufferedReader(new FileReader(new File(inputFilename)));
			PrintStream output = new PrintStream(outputFile);
			
			String line = reader.readLine();
			String header = null;
			int columnCount;
			if (hasHeader) {
				header = line;
				line = reader.readLine();
				columnCount = header.split(",").length;
				output.println(header);
			} else {
				String firstLine = line;
				if (firstLine.indexOf('#') >= 0) {
					firstLine = firstLine.substring(0, firstLine.indexOf('#'));
				}
				columnCount = firstLine.split(",").length;
			}		
			
			StringBuilder sb = new StringBuilder();
			int lineCount = 0;
			int curSize = 0;
			String[] parts;
			int commentStartIdx;
			perFeatureMappings = new HashMap<Integer, Map<String,Integer>>();
			do {
				String comment;
				commentStartIdx = line.indexOf('#');
				if (commentStartIdx >= 0) {
					comment = line.substring(commentStartIdx + 1).trim();
					line = line.substring(0, commentStartIdx).trim();
				} else {
					comment = null;
				}
				parts = line.split(",");
				for (int f = 1; f <= columnCount; f++) {
					if (f > 1) {
						sb.append(",");
					}
					try {
						Double.parseDouble(parts[f - 1]);
						sb.append(parts[f - 1]);
					} catch (Exception e) {
						sb.append(getOrdinal(parts[f - 1], f));						
					}
				}
				if (comment != null) {
					sb.append(" # " + comment);
				}
				sb.append("\n");
				curSize++;
				if (curSize == 10000) {
					output.print(sb.toString());
					curSize = 0;
					sb.setLength(0);
				}
				lineCount++;
				if (lineCount % 10000 == 0) {
					System.out.print("\rProcessed lines: " + lineCount);
				}
			} while ((line = reader.readLine()) != null);
			if (curSize > 0) {
				output.print(sb.toString());
			}
			System.out.println();
			reader.close();
			output.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
