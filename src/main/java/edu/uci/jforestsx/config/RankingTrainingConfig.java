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

package edu.uci.jforestsx.config;

import java.util.Map.Entry;

import edu.uci.jforestsx.util.ConfigHolder;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class RankingTrainingConfig extends TrainingConfig {

	/**
	 * If this optional parameter is set to a filename, then
	 * the training file will be limited to queries that their
	 * ids are listed in this file (one per line).
	 */
	public String trainQidsFilename = null;
	private final static String TRAIN_QIDS_FILENAME = "input.train.qids-filename";

	public int validNDCGTruncation = 3;
	private final static String VALID_NDCG_TRUNCATION = "ranking.valid-ndcg-truncation";	

	public boolean augmentationDocSamplingEnabled = false;
	private final static String RANKING_AUGMENTATION_DOCSAMPLING_ENABLED = "ranking.augmentation.doc-sampling-enabled";	

	public int augmentationDocSamplingTimes = 3;
	private final static String RANKING_AUGMENTATION_DOCSAMPLING_TIMES = "ranking.augmentation.doc-sampling-times";	

	public double augmentationDocSamplingRate = 0.5;
	private final static String RANKING_AUGMENTATION_DOCSAMPLING_RATE = "ranking.augmentation.doc-sampling-rate";	

	public void init(ConfigHolder config) {
		super.init(config);
		for (Entry<Object, Object> entry : config.getEntries()) {
			String key = ((String) entry.getKey()).toLowerCase();
			String value = (String) entry.getValue();

			if (key.equals(TRAIN_QIDS_FILENAME)) {
				trainQidsFilename = value;
			} else if (key.equals(VALID_NDCG_TRUNCATION)) {
				validNDCGTruncation = Integer.parseInt(value);
			} else if (key.equals(RANKING_AUGMENTATION_DOCSAMPLING_ENABLED)) {
				augmentationDocSamplingEnabled = Boolean.parseBoolean(value);
			} else if (key.equals(RANKING_AUGMENTATION_DOCSAMPLING_TIMES)) {
				augmentationDocSamplingTimes = Integer.parseInt(value);
			} else if (key.equals(RANKING_AUGMENTATION_DOCSAMPLING_RATE)) {
				augmentationDocSamplingRate = Double.parseDouble(value);
			}
		}
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(super.toString() + "\n");
		sb.append(VALID_NDCG_TRUNCATION + ": " + validNDCGTruncation + "\n");
		sb.append(RANKING_AUGMENTATION_DOCSAMPLING_ENABLED + ": " + augmentationDocSamplingEnabled + "\n");
		sb.append(RANKING_AUGMENTATION_DOCSAMPLING_TIMES + ": " + augmentationDocSamplingTimes + "\n");
		sb.append(RANKING_AUGMENTATION_DOCSAMPLING_RATE + ": " + augmentationDocSamplingRate + "\n");
		return sb.toString();
	}
}
