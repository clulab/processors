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
import java.util.ArrayList;

import edu.uci.jforestsx.input.sparse.SparseTextFileLine;
import edu.uci.jforestsx.input.sparse.SparseTextFileReader;

/**
 * Converts continuous feature values to 2-byte integer values
 */

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class DiscreteSparseTextFileGenerator {

	public static void convert(String inputFilename, String featuresStatFile, String outputFilename) {
		
		if (new File(outputFilename).exists()) {
			System.out.println("File: " + outputFilename + " already exists. Skipping it.");
			return;
		}
		
		FeatureAnalyzer featureAnalyzer = new FeatureAnalyzer();
		featureAnalyzer.loadFeaturesFromFile(featuresStatFile);

		StringBuilder sb = new StringBuilder();
		double value;
		SparseTextFileReader reader = new SparseTextFileReader();
		SparseTextFileLine line = new SparseTextFileLine();
		reader.open(inputFilename);
		int intValue;
		try {
			PrintStream output = new PrintStream(new File(outputFilename));
			int count = 0;
			while (reader.loadNextLine(line)) {
				if (line.meta) {
					output.println(line.content);
					continue;
				}
				sb.setLength(0);
				sb.append(line.target);
				if (line.qid != null) {
					sb.append(" qid:" + line.qid);
				}


				// System.out.println("Converting line with qid:" + line.qid); // SISTA
				for (int i = 0; i < line.numPairs; i++) {
					FeatureValuePair pair = line.pairs[i];
					// System.out.println("\t" + pair.featureIndex + ":" + pair.featureValue); // SISTA
					value = pair.featureValue;
					int idx = pair.featureIndex - 1;
					if (featureAnalyzer.onLogScale[idx]) {
						value = (Math.log(value - featureAnalyzer.min[idx] + 1) * featureAnalyzer.factor[idx]);
					} else {
						value = (value - featureAnalyzer.min[idx]) * featureAnalyzer.factor[idx];
					}
					intValue = (int) Math.round(value);
					// System.out.println("\t\tdiscrete value: " + intValue); // SISTA
					if (intValue != 0) {
						sb.append(" " + pair.featureIndex + ":" + intValue);
					}
				}
				output.println(sb.toString());
				count++;
				if (count % 10000 == 0) {
					System.out.println(count);
				}
			}
			output.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		reader.close();
	}

  /**
   * SISTA added code: converts features with raw values to discrete for a single datum
   * This functionality must be similar to the other convert method, for the whole dataset
   * @param rawFeatures
   * @param featureAnalyzer
   * @return
   */
  public static FeatureValuePair[] convert(FeatureValuePair[] rawFeatures, FeatureAnalyzer featureAnalyzer) {
    ArrayList<FeatureValuePair> discreteFeatures = new ArrayList<FeatureValuePair>();
    for(int i = 0; i < rawFeatures.length; i ++) {
      double value = rawFeatures[i].featureValue;
      int idx = rawFeatures[i].featureIndex;
      if (featureAnalyzer.onLogScale[idx]) {
        value = (Math.log(value - featureAnalyzer.min[idx] + 1) * featureAnalyzer.factor[idx]);
      } else {
        value = (value - featureAnalyzer.min[idx]) * featureAnalyzer.factor[idx];
      }
      int intValue = (int) Math.round(value);
      if(intValue != 0) {
        FeatureValuePair nfv = new FeatureValuePair();
        nfv.featureIndex = idx;
        nfv.featureValue = intValue;
        discreteFeatures.add(nfv);
      }
    }
    return toArray(discreteFeatures);
  }
  private static FeatureValuePair[] toArray(ArrayList<FeatureValuePair> in) {
    FeatureValuePair[] out = new FeatureValuePair[in.size()];
    for(int i = 0; i < in.size(); i ++) out[i] = in.get(i);
    return out;
  }
}
