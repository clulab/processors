package org.thunlp.thulac;

import org.thunlp.thulac.cb.CBTaggingDecoder;
import org.thunlp.thulac.data.POCGraph;
import org.thunlp.thulac.data.TaggedWord;
import org.thunlp.thulac.io.IInputProvider;
import org.thunlp.thulac.io.IOutputHandler;
import org.thunlp.thulac.io.StringOutputHandler;
import org.thunlp.thulac.postprocess.*;
import org.thunlp.thulac.preprocess.ConvertT2SPass;
import org.thunlp.thulac.preprocess.IPreprocessPass;
import org.thunlp.thulac.preprocess.PreprocessPass;
import org.thunlp.thulac.util.IOUtils;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

/**
 * The central class which acts as core of the THULAC API. It provides several
 * convenient methods make things easier for users.
 */
public class Thulac {
	/**
	 * Run the segmentation program with argument {@code segOnly}, taking input from the
	 * given {@link String} and return the segmented output as a {@link String}.
	 *
	 * @param input
	 * 		The input {@link String}.
	 * @param segOnly
	 * 		Whether to output only segments.
	 *
	 * @return The segmented output as a {@link String}.
	 *
	 * @throws IOException
	 * 		If one of the model files fails to load.
	 */
	public static String split(String input, boolean segOnly) throws IOException {
		StringOutputHandler outputProvider = IOUtils.outputToString();
		IInputProvider inputProvider = IOUtils.inputFromString(input);
		split(inputProvider, outputProvider, segOnly);
		return outputProvider.getString();
	}

	/**
	 * Run the segmentation program with argument {@code segOnly}, taking input from the
	 * given {@link File} and output the segmented return to a given {@link File}.<br>
	 * This method returns directly if either {@code inputFile} or {@code outputFile}
	 * is null.
	 *
	 * @param inputFile
	 * 		The name of the input file.
	 * @param outputFile
	 * 		The name of the output file.
	 * @param segOnly
	 * 		Whether to output only segments.
	 *
	 * @throws IOException
	 * 		If one of the model files fails to load or either the input file or the output
	 * 		file is {@code null}.
	 */
	public static void split(String inputFile, String outputFile, boolean segOnly)
			throws IOException {
		if (inputFile == null || outputFile == null) return;
		IInputProvider input = IOUtils.inputFromFile(inputFile);
		IOutputHandler output = IOUtils.outputToFile(outputFile);
		split(input, output, segOnly);
	}

	/**
	 * Run the segmentation program with argument {@code segOnly}, taking input from the
	 * given {@link File} and output the segmented return to a given {@link File}.
	 *
	 * @param input
	 * 		The input {@link File}.
	 * @param output
	 * 		The output {@link File}.
	 * @param segOnly
	 * 		Whether to output only segments.
	 *
	 * @throws IOException
	 * 		If one of the model files fails to load or either the input file or the output
	 * 		file is {@code null}.
	 */
	public static void split(File input, File output, boolean segOnly)
			throws IOException {
		if (input == null) throw new FileNotFoundException("input == null!");
		if (output == null) throw new FileNotFoundException("output == null!");
		IInputProvider inputProvider = IOUtils.inputFromFile(input);
		IOutputHandler outputHandler = IOUtils.outputToFile(output);
		split(inputProvider, outputHandler, segOnly);
	}

	/**
	 * Run the segmentation program with argument {@code segOnly} and default values
	 * for all others.
	 *
	 * @param input
	 * 		The {@link IInputProvider} instance to provide input.
	 * @param output
	 * 		The {@link IOutputHandler} instance to handle output.
	 * @param segOnly
	 * 		Whether to output only segments.
	 *
	 * @throws IOException
	 * 		If I/O of either {@code input}, {@code output} or one of the model files
	 * 		resulted in an exception.
	 */
	public static void split(IInputProvider input, IOutputHandler output, boolean segOnly)
			throws IOException {
		split("models/", '_', null, false, segOnly, false, input, output);
	}

	/**
	 * Run the segmentation program with full arguments.
	 *
	 * @param modelDir
	 * 		The directory under which the model files are located.
	 * @param separator
	 * 		The separator to use to separate words and tags.
	 * @param userDict
	 * 		The optional file name of the user-specified dictionary.
	 * @param useT2S
	 * 		Whether to transfer traditional Chinese to simplified Chinese before
	 * 		segmentation.
	 * @param segOnly
	 * 		Whether to output only segments.
	 * @param useFilter
	 * 		Whether to use filters while processing.
	 * @param input
	 * 		The {@link IInputProvider} instance to provide input.
	 * @param output
	 * 		The {@link IOutputHandler} instance to handle output.
	 *
	 * @throws IOException
	 * 		If I/O of either {@code input}, {@code output} or one of the model files
	 * 		resulted in an exception.
	 */
	public static void split(
			String modelDir, char separator, String userDict,
			boolean useT2S, boolean segOnly, boolean useFilter,
			IInputProvider input, IOutputHandler output) throws IOException {
		try {
			input.onProgramStart();
			output.onProgramStart();

			// segmentation
			CBTaggingDecoder taggingDecoder = new CBTaggingDecoder();
			taggingDecoder.threshold = segOnly ? 0 : 10000;
			String prefix = modelDir + (segOnly ? "cws_" : "model_c_");
			taggingDecoder.loadFiles(prefix + "model.bin",
					prefix + "dat.bin",
					prefix + "label.txt");
			taggingDecoder.setLabelTrans();

			// preprocess passes
			List<IPreprocessPass> pre = new ArrayList<>();
			pre.add(new PreprocessPass());
			if (useT2S) pre.add(new ConvertT2SPass(modelDir + "t2s.dat"));

			// postprocess passes
			List<IPostprocessPass> post = new ArrayList<>();
			post.add(new DictionaryPass(modelDir + "ns.dat", "ns", false));
			post.add(new DictionaryPass(modelDir + "idiom.dat", "i", false));
			post.add(new DictionaryPass(modelDir + "singlepun.dat", "w", false));
			post.add(new TimeWordPass());
			post.add(new DoubleWordPass());
			post.add(new SpecialPass());
			post.add(new NegWordPass(modelDir + "neg.dat"));
			if (userDict != null) post.add(new DictionaryPass(userDict, "uw", true));
			if (useFilter)
				post.add(new FilterPass(modelDir + "xu.dat", modelDir + "time.dat"));

			// main loop
			List<TaggedWord> words = new Vector<>();
			POCGraph graph = new POCGraph();
			for (List<String> lineSegments = input.provideInput();
				 lineSegments != null;
				 lineSegments = input.provideInput()) {
				output.handleLineStart();
				for (String raw : lineSegments) {
					for (IPreprocessPass pass : pre) raw = pass.process(raw, graph);
					taggingDecoder.segment(raw, graph, words);
					for (IPostprocessPass pass : post) pass.process(words);

					output.handleLineSegment(words, segOnly, separator);
				}
				output.handleLineEnd();
			}
		} finally { // close resources even when program crashes
			input.onProgramEnd();
			output.onProgramEnd();
		}
	}
}
