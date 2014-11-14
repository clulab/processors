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

import java.util.ArrayList;
import java.util.List;

import edu.uci.jforestsx.input.sparse.SparseTextFileLine;
import edu.uci.jforestsx.util.ArraysUtil;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class RankingBinFileGenerator extends BinaryFileGenerator {

	private String prevQid;
	private int lineIndex;
	private List<Integer> queryBoundaries;
	
	public RankingBinFileGenerator(String textFile, String featuresStatFile, String binFile) {
		super(textFile, featuresStatFile, binFile);
		prevQid = null;
		lineIndex = 0;
		queryBoundaries = new ArrayList<Integer>();
	}
	
	@Override
	protected void handle(SparseTextFileLine line) {
		if (!line.qid.equals(prevQid)) {
			queryBoundaries.add(lineIndex);
		}
		prevQid = line.qid;
		lineIndex++;
	}

	@Override
	protected void loadValueHashMaps() {
		super.loadValueHashMaps();
		queryBoundaries.add(lineIndex);
	}
	
	@Override
	protected void createBinFile() {
		writer = new RankingBinFileWriter(binFile, features, targets, ArraysUtil.toArray(queryBoundaries));
	}
	
}
