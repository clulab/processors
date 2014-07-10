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

import edu.uci.jforestsx.input.FeatureValuePair;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class SparseTextFileLine {
	
	private final static int DEFAULT_MAX_PAIRS = 1000;
	
	public int target;
	public String qid;
	public FeatureValuePair[] pairs;
	public int numPairs;
	public boolean meta;
	public String content;
	
	public SparseTextFileLine() {
		 pairs = new FeatureValuePair[DEFAULT_MAX_PAIRS];
		 for (int f = 0; f < DEFAULT_MAX_PAIRS; f++) {
			 pairs[f] = new FeatureValuePair();
		 }
		 numPairs = 0;
	}
	
	public void ensureCapacity(int size) {
		if (pairs.length < size) {
			int newSize = pairs.length * 2;
			if (size > newSize) {
				newSize = size;
			}
			FeatureValuePair[] newPairs = new FeatureValuePair[newSize];
			for (int f = 0; f < pairs.length; f++) {
				newPairs[f] = pairs[f];
			}
			for (int f = pairs.length; f < newSize; f++) {
				newPairs[f] = new FeatureValuePair();
			}
			pairs = newPairs;
		}
	}
	
}
