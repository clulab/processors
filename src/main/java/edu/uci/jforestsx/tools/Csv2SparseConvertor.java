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

import edu.uci.jforestsx.input.sparse.FeatureMetaData;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class Csv2SparseConvertor {

	public enum TargetPosition {
		FIRST, LAST, NO_TARGET;
	}

	public static void convert(String inputFilename, String outputFilename, TargetPosition targetPosition,
			boolean hasHeader) {
		try {
			File outputFile = new File(outputFilename);
			if (outputFile.exists()) {
				System.out.println("File: " + outputFilename + " already exists. Skipping it.");
				return;
			}
			BufferedReader reader = new BufferedReader(new FileReader(new File(inputFilename)));
			
			String line = reader.readLine();
			String header = null;
			int columnCount;
			if (hasHeader) {
				header = line;				
				line = reader.readLine();
				columnCount = header.split(",").length;
			} else {
				String firstLine = line;
				if (firstLine.indexOf('#') >= 0) {
					firstLine = firstLine.substring(0, firstLine.indexOf('#'));
				}
				columnCount = firstLine.split(",").length;
			}
			int start;
			int end;
			int targetColumn;
			if (targetPosition == TargetPosition.FIRST) {
				start = 1;
				end = columnCount - 1;
				targetColumn = 0;
			} else if (targetPosition == TargetPosition.LAST) {
				start = 0;
				end = columnCount - 2;
				targetColumn = columnCount - 1;
			} else {
				start = 0;
				end = columnCount - 1;
				targetColumn = -1;
			}
			
			PrintStream output = new PrintStream(outputFile);
			if (hasHeader) {
				String[] colNames = header.split(",");
				HashMap<String, String> map = new HashMap<String, String>();
				for (int f = start; f <= end; f++) {
					int fIdx = f - start + 1;
					map.clear();
					map.put("id", "" + fIdx);
					map.put("name", colNames[f]);
					output.println(new FeatureMetaData(map).toString());
				}
			}
			StringBuilder sb = new StringBuilder();
			int lineCount = 0;
			int target, fIdx;
			double value;
			String valueStr;
			int curSize = 0;
			String[] parts;
			int commentStartIdx;
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
				target = 0;
				if (targetColumn >= 0) {
					target = Integer.parseInt(parts[targetColumn]);
				}
				sb.append(target);
				for (int f = start; f <= end; f++) {
					fIdx = f - start + 1;
					try {
						value = Double.parseDouble(parts[f]);
						if (value != 0) {
							valueStr = "" + value;
							if (valueStr.endsWith(".0")) {
								valueStr = valueStr.substring(0, valueStr.length() - 2);
							}
							sb.append(" " + fIdx + ":" + valueStr);
						}
					} catch (Exception e) {
						sb.append(" " + fIdx + ":" + parts[f]);
						System.out.println("Warning: feature: " + f + " had non-double value: " + parts[f]);
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
