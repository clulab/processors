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

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.List;
import java.util.Properties;

import joptsimple.OptionParser;
import joptsimple.OptionSet;
import edu.uci.jforestsx.config.TrainingConfig;
import edu.uci.jforestsx.dataset.Dataset;
import edu.uci.jforestsx.dataset.DatasetLoader;
import edu.uci.jforestsx.dataset.RankingDataset;
import edu.uci.jforestsx.dataset.RankingDatasetLoader;
import edu.uci.jforestsx.input.RankingRaw2BinConvertor;
import edu.uci.jforestsx.input.Raw2BinConvertor;
import edu.uci.jforestsx.learning.LearningUtils;
import edu.uci.jforestsx.learning.trees.Ensemble;
import edu.uci.jforestsx.learning.trees.decision.DecisionTree;
import edu.uci.jforestsx.learning.trees.regression.RegressionTree;
import edu.uci.jforestsx.sample.RankingSample;
import edu.uci.jforestsx.sample.Sample;
import edu.uci.jforestsx.util.IOUtils;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class Runner {

	@SuppressWarnings("unchecked")
	private static void generateBin(OptionSet options) throws Exception {
		if (!options.has("folder")) {
			System.err.println("The input folder is not specified.");
			return;
		}

		if (!options.has("file")) {
			System.err.println("Input files are not specified.");
			return;
		}

		String folder = (String) options.valueOf("folder");
		List<String> filesList = (List<String>) options.valuesOf("file");
		String[] files = new String[filesList.size()];
		for (int i = 0; i < files.length; i++) {
			files[i] = filesList.get(i);
		}

		if (options.has("ranking")) {
			System.out.println("Generating binary files for ranking data sets...");
			new RankingRaw2BinConvertor().convert(folder, files);
		} else {
			System.out.println("Generating binary files...");
			new Raw2BinConvertor().convert(folder, files);
		}
	}

	private static void train(OptionSet options) throws Exception {
		if (!options.has("config-file")) {
			System.err.println("The configurations file is not specified.");
			return;
		}

		InputStream configInputStream = new FileInputStream((String) options.valueOf("config-file"));
		Properties configProperties = new Properties();
		configProperties.load(configInputStream);

		if (options.has("train-file")) {
			configProperties.put(TrainingConfig.TRAIN_FILENAME, options.valueOf("train-file"));
		}

		if (options.has("validation-file")) {
			configProperties.put(TrainingConfig.VALID_FILENAME, options.valueOf("validation-file"));
		}

		Ensemble ensemble;

		if (options.has("ranking")) {
			RankingApp app = new RankingApp();
			ensemble = app.run(configProperties);
		} else {
			ClassificationApp app = new ClassificationApp();
			ensemble = app.run(configProperties);
		}

		/*
		 * Dump the output model if requested.
		 */
		if (options.has("output-model")) {
			String outputModelFile = (String) options.valueOf("output-model");
			File file = new File(outputModelFile);
			PrintStream ensembleOutput = new PrintStream(file);
			ensembleOutput.println(ensemble);
			ensembleOutput.close();
		}

	}

	private static void predict(OptionSet options) throws Exception {

		if (!options.has("model-file")) {
			System.err.println("Model file is not specified.");
			return;
		}

		if (!options.has("tree-type")) {
			System.err.println("Types of trees in the ensemble is not specified.");
			return;
		}

		if (!options.has("test-file")) {
			System.err.println("Test file is not specified.");
			return;
		}

		/*
		 * Load the ensemble
		 */
		File modelFile = new File((String) options.valueOf("model-file"));
		Ensemble ensemble = new Ensemble();
		if (options.valueOf("tree-type").equals("RegressionTree")) {
			ensemble.loadFromFile(RegressionTree.class, modelFile);
		} else if (options.valueOf("tree-type").equals("DecisionTree")) {
			ensemble.loadFromFile(DecisionTree.class, modelFile);
		} else {
			System.err.println("Unknown tree type: " + options.valueOf("tree-type"));
		}

		/*
		 * Load the data set
		 */
		InputStream in = new IOUtils().getInputStream((String) options.valueOf("test-file"));
		Sample sample;
		if (options.has("ranking")) {
			RankingDataset dataset = new RankingDataset();
			RankingDatasetLoader.load(in, dataset);
			sample = new RankingSample(dataset);
		} else {
			Dataset dataset = new Dataset();
			DatasetLoader.load(in, dataset);
			sample = new Sample(dataset);
		}
		in.close();

		double[] predictions = new double[sample.size];
		LearningUtils.updateScores(sample, predictions, ensemble);

		PrintStream output;
		if (options.has("output-file")) {
			output = new PrintStream(new File((String) options.valueOf("output-file")));
		} else {
			output = System.out;
		}
		
		for (int i = 0; i < sample.size; i++) {
			output.println(predictions[i]);
		}

	}

	public static void main(String[] args) throws Exception {

		OptionParser parser = new OptionParser();

		parser.accepts("cmd").withRequiredArg();
		parser.accepts("ranking");

		/*
		 * Bin generation arguments
		 */
		parser.accepts("folder").withRequiredArg();
		parser.accepts("file").withRequiredArg();

		/*
		 * Training arguments
		 */
		parser.accepts("config-file").withRequiredArg();
		parser.accepts("train-file").withRequiredArg();
		parser.accepts("validation-file").withRequiredArg();
		parser.accepts("output-model").withRequiredArg();

		/*
		 * Prediction arguments
		 */
		parser.accepts("model-file").withRequiredArg();
		parser.accepts("tree-type").withRequiredArg();
		parser.accepts("test-file").withRequiredArg();
		parser.accepts("output-file").withRequiredArg();

		OptionSet options = parser.parse(args);

		if (!options.has("cmd")) {
			System.err.println("You must specify the command through 'cmd' parameter.");
			return;
		}

		if (options.valueOf("cmd").equals("generate-bin")) {
			generateBin(options);
		} else if (options.valueOf("cmd").equals("train")) {
			train(options);
		} else if (options.valueOf("cmd").equals("predict")) {
			predict(options);
		} else {
			System.err.println("Unknown command: " + options.valueOf("cmd"));
		}

		/*
		 * Make sure that thread pool is terminated.
		 */
		ClassificationApp.shutdown();
	}
}
