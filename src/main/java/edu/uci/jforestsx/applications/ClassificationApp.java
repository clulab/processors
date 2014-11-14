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

package edu.uci.jforestsx.applications;

import java.io.InputStream;
import java.util.Properties;
import java.util.Random;

import edu.uci.jforestsx.config.TrainingConfig;
import edu.uci.jforestsx.dataset.Dataset;
import edu.uci.jforestsx.dataset.DatasetLoader;
import edu.uci.jforestsx.eval.AUC;
import edu.uci.jforestsx.eval.Accuracy;
import edu.uci.jforestsx.eval.BalancedYoundenIndex;
import edu.uci.jforestsx.eval.EvaluationMetric;
import edu.uci.jforestsx.eval.RMSE;
import edu.uci.jforestsx.learning.LearningModule;
import edu.uci.jforestsx.learning.LearningProgressListener;
import edu.uci.jforestsx.learning.boosting.GradientBoosting;
import edu.uci.jforestsx.learning.classification.GradientBoostingBinaryClassifier;
import edu.uci.jforestsx.learning.trees.Ensemble;
import edu.uci.jforestsx.learning.trees.decision.RandomForest;
import edu.uci.jforestsx.learning.trees.regression.RegressionTreeLearner;
import edu.uci.jforestsx.sample.Sample;
import edu.uci.jforestsx.util.ConfigHolder;
import edu.uci.jforestsx.util.Constants;
import edu.uci.jforestsx.util.IOUtils;
import edu.uci.jforestsx.util.Timer;
import edu.uci.jforestsx.util.concurrency.BlockingThreadPoolExecutor;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class ClassificationApp {

	protected Dataset trainDataset;
	protected Dataset validDataset;
	protected LearningModule topLearner;

	protected Sample trainSet;
	protected Sample validSet;

	protected IOUtils ioUtils;
	protected EvaluationMetric evaluationMetric;
	protected Random rnd;

	protected TrainingConfig trainingConfig;
	protected ConfigHolder configHolder;
	
	protected LearningProgressListener progressListener = null;
	
	public ClassificationApp() {
		initIOUtils();
	}

	public void setProgressListener(LearningProgressListener progressListener) {
		this.progressListener = progressListener;
	}

	protected void initIOUtils() {
		if (ioUtils == null) {
			ioUtils = new IOUtils();
		}
	}

	protected void loadConfig() {
		trainingConfig = new TrainingConfig();
		trainingConfig.init(configHolder);
	}

	protected void init() throws Exception {
		BlockingThreadPoolExecutor.init(trainingConfig.numThreads);

		initDataset(trainDataset);
		if (validSet != null) {
			initDataset(validSet.dataset);
		}

		if (trainingConfig.featureNamesFilename != null) {
			trainDataset.loadFeatureNamesFromExternalResource(ioUtils.getInputStream(trainingConfig.featureNamesFilename));
		}
	}

	protected LearningModule getLearningModule(String name) throws Exception {
		int maxNumTrainInstances = trainDataset.numInstances;
		int maxNumValidInstances = (validDataset != null ? validDataset.numInstances : trainDataset.numInstances);
		if (name.equals("GradientBoostingBinaryClassifier")) {
			GradientBoostingBinaryClassifier learner = new GradientBoostingBinaryClassifier();
			learner.init(configHolder, maxNumTrainInstances, maxNumValidInstances, evaluationMetric);
			return learner;
		} else if (name.equals("GradientBoosting")) {
			GradientBoosting learner = new GradientBoosting();
			learner.init(configHolder, maxNumTrainInstances, maxNumValidInstances, evaluationMetric);
			return learner;
		} else if (name.equals("RegressionTree")) {
			RegressionTreeLearner learner = new RegressionTreeLearner();
			learner.init(trainDataset, configHolder, maxNumTrainInstances);
			return learner;
		} else if (name.equals("RandomForest")) {
			RandomForest learner = new RandomForest();
			learner.init(trainDataset, configHolder, maxNumTrainInstances, maxNumValidInstances, evaluationMetric);
			return learner;
		} else {
			throw new Exception("Unknown algorithm: " + name);
		}
	}

	protected EvaluationMetric getEvaluationMetric(String name) throws Exception {
		if (name.equals("AUC")) {
			return new AUC();
		} else if (name.equals("RMSE")) {
			return new RMSE();
		} else if (name.equals("Accuracy")) {
			return new Accuracy();
		} else if (name.equals("BalancedYoundenIndex")) {
			return new BalancedYoundenIndex();
		} else {
			throw new Exception("Unknown evaluation metric: " + name);
		}
	}

	protected void createLearner() throws Exception {
		String[] parts = trainingConfig.learningAlgorithm.split("-");
		topLearner = getLearningModule(parts[0]);
		if (progressListener != null) {
			topLearner.setProgressListener(progressListener);
		}
		LearningModule curModule = topLearner;
		for (int i = 1; i < parts.length; i++) {
			LearningModule newModule = getLearningModule(parts[i]);
			if (progressListener != null) {
				newModule.setProgressListener(progressListener);
			}
			curModule.setSubModule(newModule);
			curModule = newModule;
		}
	}

	protected void loadDataset(InputStream in, Dataset dataset) throws Exception {
		DatasetLoader.load(in, dataset);
	}

	public void loadDataset(String uri, Dataset dataset) throws Exception {
		if (dataset != null && dataset.uri != null && dataset.uri.equals(uri)) {
			// This data set is already loaded.
			dataset.needsInitialization = false;
			return;
		}
		InputStream in = ioUtils.getInputStream(uri);
		loadDataset(in, dataset);
		dataset.uri = uri;
		dataset.needsInitialization = true;
		in.close();
	}

	protected double getMeasurement(double[] scores, Sample sample) throws Exception {
		return sample.evaluate(scores, evaluationMetric, 1.0);
	}

	protected Dataset newDataset() {
		return new Dataset();
	}

	protected void initDataset(Dataset dataset) throws Exception {
		// Will be overridden by subclasses
	}

	protected Sample createSample(Dataset dataset, boolean trainSample) {
		return new Sample(dataset);
	}

	protected int getMaxTrainInstances() {
		return trainDataset.numInstances;
	}

	public Ensemble run(Properties configProperties) {
		try {
			configHolder = new ConfigHolder(configProperties);
			loadConfig();
			if (!trainingConfig.validate(ioUtils)) {
				System.out.println("Error: " + trainingConfig.getErrorMessage());
				return null;
			}
			rnd = new Random(trainingConfig.randomSeed);

			System.out.println("Loading datasets...");
			if (trainDataset == null) {
				trainDataset = newDataset();
			}
			loadDataset(trainingConfig.trainFilename, trainDataset);
			int maxInstances = getMaxTrainInstances();

			if (trainingConfig.validFilename != null) {
				if (validDataset == null) {
					validDataset = newDataset();
				}
				loadDataset(trainingConfig.validFilename, validDataset);
				if (validDataset.numInstances > maxInstances) {
					maxInstances = validDataset.numInstances;
				}
			} else {
				validDataset = null;
			}
			System.out.println("Finished loading datasets.");

			Constants.init(maxInstances);

			Sample allTrainSample = createSample(trainDataset, true);
			trainSet = allTrainSample.getRandomSubSample(trainingConfig.trainFraction, rnd);

			if (validDataset != null) {
				validSet = createSample(validDataset, false);
				if (trainingConfig.validFraction < 1.0) {
					validSet = validSet.getRandomSubSample(trainingConfig.validFraction, rnd);
				}
			} else if (trainingConfig.validOutOfTrain) {
				validSet = allTrainSample.getOutOfSample(trainSet);
			}

			init();

			evaluationMetric = getEvaluationMetric(trainingConfig.evaluationMetric);

			createLearner();

			Timer timer = new Timer();
			timer.start();
			Ensemble ensemble = topLearner.learn(trainSet, validSet);
			System.out.println("Time taken to build model: " + (timer.getElapsedMillis() / 1000.0) + " seconds.");
			return ensemble;

		} catch (Exception e) {
			e.printStackTrace();
		}

		return null;
	}

	public int getTrainingRandomSeed() {
		return trainingConfig.randomSeed;
	}

	public static void shutdown() {
		BlockingThreadPoolExecutor executor = BlockingThreadPoolExecutor.getInstance();
		if (executor != null && !executor.isShutdown()) {
			executor.shutdownNow();
		}
	}

	public EvaluationMetric getEvaluationMetric() {
		return evaluationMetric;
	}

	public double getValidMeasurement() throws Exception {
		return topLearner.getValidationMeasurement();
	}

	public Sample getTrainSample() {
		return trainSet;
	}

	public Sample getValidSample() {
		return validSet;
	}
	
	public ConfigHolder getConfigHolder() {
		return configHolder;
	}
	
	public IOUtils getIOUtils() {
		return ioUtils;
	}
	
	public LearningProgressListener getProgressListener() {
		return progressListener;
	}
}
