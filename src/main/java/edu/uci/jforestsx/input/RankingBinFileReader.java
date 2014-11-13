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

import java.io.InputStream;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class RankingBinFileReader extends BinaryFileReader {

	private int[] queryBoundaries;
	private int numQueries;
	private int maxDocsPerQuery;

	public RankingBinFileReader(InputStream in) {
		super(in);
	}

	public int[] getQueryBoundaries() {
		return queryBoundaries;
	}
	
	public int getMaximumDocsPerQuery() {
		return maxDocsPerQuery;
	}

	@Override
	protected void readHeader() throws Exception {
		super.readHeader();
		numQueries = readInt();
		queryBoundaries = new int[numQueries + 1];
	}

	@Override
	public void read() throws Exception {
		super.read();
		for (int q = 0; q < numQueries + 1; q++) {
			queryBoundaries[q] = readInt();
		}
		maxDocsPerQuery = 0;
		for (int q = 0; q < numQueries; q++) {
			int queryDocs = queryBoundaries[q + 1] - queryBoundaries[q];
			if (queryDocs > maxDocsPerQuery) {
				maxDocsPerQuery = queryDocs;
			}
		}
	}
	
}
