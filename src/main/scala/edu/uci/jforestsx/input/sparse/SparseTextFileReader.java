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

package edu.uci.jforestsx.input.sparse;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.StringTokenizer;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class SparseTextFileReader {

	BufferedReader reader;

	public void open(String filename) {
		try {
			reader = new BufferedReader(new FileReader(new File(filename)));
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
	}

	public boolean loadNextLine(SparseTextFileLine line) {
		try {
			String str = reader.readLine();
			if (str != null) {
				str = str.trim();
				int commentIdx = str.indexOf('#');
				if (commentIdx >= 0) {
					str = str.substring(0, commentIdx).trim();
				}
				if (str.startsWith("@")) {
					line.meta = true;
					line.content = str;
				} else {
					line.meta = false;
					line.content = null;
					StringTokenizer st = new StringTokenizer(str, " ");
					line.target = Integer.parseInt(st.nextToken());
					line.numPairs = 0;
					while (st.hasMoreTokens()) {
						String[] parts = st.nextToken().split(":");
						if (parts[0].equals("qid")) {
							line.qid = parts[1];
						} else {
							line.ensureCapacity(line.numPairs + 1);
							line.pairs[line.numPairs].featureIndex = Integer.parseInt(parts[0]);
							line.pairs[line.numPairs].featureValue = Double.parseDouble(parts[1]);
							line.numPairs++;
						}
					}
				}
				return true;
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		return false;
	}

	public void close() {
		try {
			reader.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
}
