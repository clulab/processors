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
import edu.uci.jforestsx.util.IOUtils;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class TrainingConfig extends ComponentConfig {

	/**
	 * Training file name
	 */	
	public String trainFilename = null;
	public final static String TRAIN_FILENAME = "input.train";

	/**
	 * Validation file name
	 */
	public String validFilename = null;
	public final static String VALID_FILENAME = "input.valid";	

	/**
	 * If feature names are needed and they are not stored in the training file,
	 * they can be loaded from this external file. Each line in the file contains
	 * one feature name.
	 */
	public String featureNamesFilename = null;
	private final static String FEATURENAMES_FILENAME = "input.train.feature-names-file";	

	/**
	 * The name of the algorithm to be used for training.
	 * For example, the value of "Bagging-RegressionTree" means that we want Bagging wrapped
	 * around Regression trees.
	 */
	public String learningAlgorithm = null;
	private final static String LEARNING_ALGORITHM = "learning.algorithm";	

	/**
	 * The name of the evaluation metric to be used during training.
	 * Default is AUC.
	 * Other currently implemented metrics include RMSE and BalancedYoundenIndex.
	 */
	public String evaluationMetric = "AUC";
	private final static String LEARNING_EVALUATION_METRIC = "learning.evaluation-metric";	

	/**
	 * If this parameter is set to a value less than 1.0, only a fraction of
	 * the training data will be used for training.
	 */
	public double trainFraction = 1.0;
	private final static String TRAIN_FRACTION = "input.train-fraction";

	
	/**
	 * If this parameter is set to a value less than 1.0, only a fraction of
	 * the validation data will be used for validation.
	 */
	public double validFraction = 1.0;
	private final static String VALID_FRACTION = "input.valid-fraction";	

	/**
	 * If for training only a fraction of data is used and this parameter is set to
	 * true, then for validation we will use the data in training input file which
	 * is left out of training.
	 */
	public boolean validOutOfTrain = false;
	private final static String VALID_OUT_OF_TRAIN = "input.valid.out-of-train";	

	/**
	 * Number of threads to use. By default this is set to the number of processors
	 * on the machine. However, for debugging, sometimes it is needed to set it to 1.
	 */
	public int numThreads = Runtime.getRuntime().availableProcessors();
	private final static String NUM_THREADS = "params.num-threads";	

	/**
	 * The random seed to be used for training.
	 */
	public int randomSeed = 1;
	private final static String RANDOM_SEED = "params.random-seed";	

	/**
	 * If this parameter is set to true, the performance on validation data is
	 * printed during the iterations of training.
	 */
	public boolean printIntermediateValidMeasurements = false;
	private final static String PRINT_INTERMEDIATE_VALID_MEASUREMENTS = "params.print-intermediate-valid-measurements";	

	public void init(ConfigHolder config) {
		for (Entry<Object, Object> entry : config.getEntries()) {
			String key = ((String) entry.getKey()).toLowerCase();
			String value = (String) entry.getValue();

			if (key.equals(TRAIN_FILENAME)) {
				trainFilename = value;
			} else if (key.equals(VALID_FILENAME)) {
				validFilename = value;
			} else if (key.equals(FEATURENAMES_FILENAME)) {
				featureNamesFilename = value;
			} else if (key.equals(LEARNING_ALGORITHM)) {
				learningAlgorithm = value;
			} else if (key.equals(LEARNING_EVALUATION_METRIC)) {
				evaluationMetric = value;
			} else if (key.equals(TRAIN_FRACTION)) {
				trainFraction = Double.parseDouble(value);
			} else if (key.equals(VALID_FRACTION)) {
				validFraction = Double.parseDouble(value);
			} else if (key.equals(NUM_THREADS)) {
				numThreads = Integer.parseInt(value);
			} else if (key.equals(RANDOM_SEED)) {
				randomSeed = Integer.parseInt(value);
			} else if (key.equals(VALID_OUT_OF_TRAIN)) {
				validOutOfTrain = Boolean.parseBoolean(value);
			} else if (key.equals(PRINT_INTERMEDIATE_VALID_MEASUREMENTS)) {
				printIntermediateValidMeasurements = Boolean.parseBoolean(value);
			} 
		}
	}

	public boolean validate(IOUtils ioUtils) {
		if (trainFilename == null) {
			errorMessage = "Training file is not set";
			return false;
		}
		if (!ioUtils.exists(trainFilename)) {
			errorMessage = "Training file: " + trainFilename + " does not exists.";
			return false;
		}
		if (validFilename != null && !ioUtils.exists(validFilename)) {
			errorMessage = "Validation file: " + validFilename + " does not exists.";
			return false;
		}		
		if (learningAlgorithm == null) {
			errorMessage = "Learning algorithm is not specified.";
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(TRAIN_FILENAME + ": " + trainFilename + "\n");
		sb.append(VALID_FILENAME + ": " + validFilename + "\n");
		sb.append(FEATURENAMES_FILENAME + ": " + featureNamesFilename + "\n");
		sb.append(LEARNING_ALGORITHM + ": " + learningAlgorithm + "\n");
		sb.append(TRAIN_FRACTION + ": " + trainFraction + "\n");
		sb.append(VALID_FRACTION + ": " + validFraction + "\n");
		sb.append(NUM_THREADS + ": " + numThreads + "\n");
		sb.append(RANDOM_SEED + ": " + randomSeed + "\n");
		sb.append(PRINT_INTERMEDIATE_VALID_MEASUREMENTS + ": " + printIntermediateValidMeasurements + "\n");
		return sb.toString();
	}
}
