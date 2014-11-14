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

import java.io.File;
import java.io.PrintStream;

import edu.uci.jforestsx.input.FeatureValuePair;
import edu.uci.jforestsx.input.sparse.FeatureMetaData;
import edu.uci.jforestsx.input.sparse.MetaData;
import edu.uci.jforestsx.input.sparse.MetaLineParser;
import edu.uci.jforestsx.input.sparse.SparseTextFileLine;
import edu.uci.jforestsx.input.sparse.SparseTextFileReader;

/**
 * Converts a file from the Sparse format to Arff format which 
 * can then be used in Weka package.
 *
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class Sparse2ArffConvertor {
	
	private SparseTextFileReader reader;
	private PrintStream output;
	private boolean dumpAsSparse;
	private SparseTextFileLine line;
	private int featureCount;
	private StringBuilder sb;
	private int targetColIdx;
	
	public void init(String inputFile, String outputFile, boolean dumpAsSparse) throws Exception {
		reader = new SparseTextFileReader();
		reader.open(inputFile);
		this.dumpAsSparse = dumpAsSparse;
		
		output = new PrintStream(new File(outputFile));
		output.println("@RELATION myrel");
		output.println();
		
		sb = new StringBuilder();
		line = new SparseTextFileLine();
	}
	
	public void dumpHeaderFromInputFile() {
		featureCount = 0;
		while (reader.loadNextLine(line)) {
			if (line.meta) {
				MetaData metaData = MetaLineParser.parse(line.content);
				if (metaData instanceof FeatureMetaData) {
					output.println("@ATTRIBUTE " + ((FeatureMetaData) metaData).name + " REAL");
					featureCount++;
				}
			} else {
				output.println("@ATTRIBUTE target REAL");
				output.println("");
				output.println("@DATA");
				targetColIdx = featureCount;
				processLine(line);
				break;
			}
		}
	}
	
	public void dumpHeader(int featureCount) {
		for (int f = 0; f < featureCount; f++) {
			output.println("@ATTRIBUTE f" + (f+1) + " REAL");
		}
		this.featureCount = featureCount;
		output.println("@ATTRIBUTE target REAL");
		output.println("");
		output.println("@DATA");
		targetColIdx = featureCount;
	}
	
	private void processLine(SparseTextFileLine line) {
		sb.setLength(0);
		if (dumpAsSparse) {
			sb.append("{");
			for (int i = 0; i < line.numPairs; i++) {
				FeatureValuePair pair = line.pairs[i];
				if (sb.length() > 1) {
					sb.append(",");
				}
				sb.append((pair.featureIndex - 1) + " " + pair.featureValue);
			}
			sb.append(", " + targetColIdx + " " + line.target);
			sb.append("}\n");
		} else {
			int prevFeatureIdx = 0;
			for (int i = 0; i < line.numPairs; i++) {
				FeatureValuePair pair = line.pairs[i];
				while (pair.featureIndex > prevFeatureIdx + 1) {
					if (sb.length() > 0) {
						sb.append(",");
					}
					sb.append("0");
					prevFeatureIdx++;
				}
				if (sb.length() > 0) {
					sb.append(",");
				}
				String fValue = "" + pair.featureValue;
				if (fValue.endsWith(".0")) {
					fValue = fValue.substring(0, fValue.length() - 2);
				}
				sb.append(fValue);
				prevFeatureIdx = pair.featureIndex;
			}
			for (int i = prevFeatureIdx; i < featureCount; i++) {
				sb.append(",0");
			}
			sb.append("," + line.target + "\n");
		}
		output.print(sb.toString());
	}

	public void convert() throws Exception {
		int count = 0;
		while (reader.loadNextLine(line)) {
			processLine(line);
			count++;
			if (count % 10000 == 0) {
				System.out.println("\t Processed: " + count);
			}
		}
		reader.close();
		output.close();
	}	
}
