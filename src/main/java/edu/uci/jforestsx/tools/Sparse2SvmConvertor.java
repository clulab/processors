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
 * Converts a file from the Sparse format to SVM-light format.
 *
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class Sparse2SvmConvertor {

	public static void convert(String inputFile, String outputFile) throws Exception {
		SparseTextFileReader reader = new SparseTextFileReader();
		reader.open(inputFile);
		SparseTextFileLine line = new SparseTextFileLine();
		int count = 0;
		PrintStream output = new PrintStream(new File(outputFile));
		StringBuilder sb = new StringBuilder();
		int featureCount = 0;
		while (reader.loadNextLine(line)) {
			if (line.meta) {
				MetaData metaData = MetaLineParser.parse(line.content);
				if (metaData instanceof FeatureMetaData) {
					featureCount++;
				}
			} else {
				sb.setLength(0);
				sb.append(line.target);
				for (int i = 0; i < line.numPairs; i++) {
					FeatureValuePair pair = line.pairs[i];
					sb.append(" " + pair.featureIndex + ":" + pair.featureValue);
				}				
				sb.append("\n");
				output.print(sb.toString());
				count++;
				if (count % 10000 == 0) {
					System.out.println("\t Processed: " + count);
				}
			}
		}
		reader.close();
	}

}
