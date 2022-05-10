package org.thunlp.thulac.main;

import org.thunlp.thulac.Thulac;
import org.thunlp.thulac.io.IInputProvider;
import org.thunlp.thulac.io.IOutputHandler;
import org.thunlp.thulac.util.IOUtils;

import java.io.IOException;

/**
 * The program entrance which deals with command line arguments.
 */
public class Main {
	public static void main(String[] args) throws IOException {
		String modelDir = "models/";
		char separator = '_';
		String userDict = null;
		boolean useT2S = false;
		boolean segOnly = false;
		boolean useFilter = false;
		IInputProvider input = null;
		IOutputHandler output = null;

		for (int c = 0; c < args.length; ++c)
			switch (args[c]) {
				case "-t2s":
					useT2S = true;
					break;
				case "-user":
					userDict = args[++c];
					break;
				case "-deli":
					separator = args[++c].charAt(0);
					break;
				case "-seg_only":
					segOnly = true;
					break;
				case "-filter":
					useFilter = true;
					break;
				case "-model_dir":
					modelDir = args[++c];
					if (modelDir.charAt(modelDir.length() - 1) != '/')
						modelDir += '/';
					break;
				case "-input":
					input = IOUtils.inputFromFile(args[++c]); // use UTf-8
					break;
				case "-output":
					output = IOUtils.outputToFile(args[++c]); // use UTF-8
					break;
			}
		if (input == null) input = IOUtils.inputFromConsole();
		if (output == null) output = IOUtils.outputToConsole();

		Thulac.split(modelDir, separator, userDict, useT2S, segOnly, useFilter,
				input, output);
	}
}
