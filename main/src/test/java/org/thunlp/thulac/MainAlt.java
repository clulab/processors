package org.thunlp.thulac;

import joptsimple.OptionException;
import joptsimple.OptionParser;
import joptsimple.OptionSet;
import joptsimple.OptionSpec;
import org.thunlp.thulac.io.IInputProvider;
import org.thunlp.thulac.io.IOutputHandler;
import org.thunlp.thulac.util.IOUtils;

import java.io.IOException;

import static java.util.Arrays.asList;

/**
 * A test class of the CLI (Command Line Interface), using
 * <a href="http://pholser.github.io/jopt-simple/">Jopt Simple</a> to parse command
 * line input.
 */
public class MainAlt {
	private static final String SEG_ONLY_DESC = "Output segments only";
	private static final String T2S_DESC = "Convert traditional to simplified Chinese";
	private static final String FILTER_DESC = "Use filter for output";
	private static final String INPUT_DESC = "Path to the input file";
	private static final String OUTPUT_DESC = "Path to the output file";
	private static final String USER_DICT_DESC = "The user-specified dictionary";
	private static final String DELIMITER_DESC = "The separator between words and tags";
	private static final String MODEL_DIR_DESC = "Path for models directory";
	private static final String HELP_DESC = "Show help";

	public static void main(String[] args) throws IOException {
		OptionParser parser = new OptionParser();

		parser.accepts("seg_only", SEG_ONLY_DESC);
		parser.accepts("t2s", T2S_DESC);
		parser.accepts("filter", FILTER_DESC);
		OptionSpec<String> iOpt = parser.acceptsAll(
				asList("input", "i"), INPUT_DESC).withRequiredArg();
		OptionSpec<String> oOpt = parser.acceptsAll(
				asList("output", "o"), OUTPUT_DESC).withRequiredArg();
		OptionSpec<String> userDictOpt = parser.acceptsAll(
				asList("user_dict", "dict", "user"), USER_DICT_DESC).withRequiredArg();
		OptionSpec<String> dOpt = parser.acceptsAll(
				asList("delimiter", "delim", "deli"), DELIMITER_DESC).withRequiredArg();
		OptionSpec<String> modelDirOpt = parser.acceptsAll(
				asList("model_dir", "model"), MODEL_DIR_DESC).withRequiredArg();
		parser.acceptsAll(asList("help", "?", "h"), HELP_DESC).forHelp();

		OptionSet opts = parser.parse(args);

		if (opts.has("help")) parser.printHelpOn(System.out);
		else try {
			char separator = opts.valueOf(dOpt).charAt(0);
			boolean segOnly = opts.has("seg_only");
			boolean useT2S = opts.has("t2s");
			boolean useFilter = opts.has("filter");

			IInputProvider input;
			if (opts.has(iOpt)) input = IOUtils.inputFromFile(opts.valueOf(iOpt));
			else input = IOUtils.inputFromConsole();
			IOutputHandler output;
			if (opts.has(oOpt)) output = IOUtils.outputToFile(opts.valueOf(oOpt));
			else output = IOUtils.outputToConsole();

			String userDict = opts.valueOf(userDictOpt);
			String modelDir = opts.valueOf(modelDirOpt);

			Thulac.split(modelDir, separator, userDict,
					useT2S, segOnly, useFilter, input, output);
		} catch (OptionException e) {
			parser.printHelpOn(System.out);
		}
	}
}
