package org.thunlp.thulac;

import org.thunlp.thulac.io.IInputProvider;
import org.thunlp.thulac.io.IOutputHandler;
import org.thunlp.thulac.util.StringUtils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * Helper class for THULAC tests.
 */
public class TestHelper {
	/**
	 * Run the segmentation program, write the output to the given position and
	 * calculate the accuracy of the program.
	 *
	 * @param inputFile
	 * 		The {@link IInputProvider} used as input.
	 * @param compareFile
	 * 		The {@link IInputProvider} used as answer.
	 * @param outputFile
	 * 		The {@link IOutputHandler} used as output.
	 *
	 * @throws IOException
	 * 		If an error occurs while I/O.
	 */
	public static void testSuite(
			IAccessible inputFile, IAccessible compareFile, IAccessible outputFile)
			throws IOException {
		run(inputFile, outputFile, true);
		compare(inputFile, compareFile, outputFile);
	}

	/**
	 * Runs the segmentation program with given input and output and the {@code
	 * segOnly} flag and output execution time.
	 *
	 * @param input
	 * 		The {@link IAccessible} used as input.
	 * @param output
	 * 		The {@link IAccessible} used as output.
	 * @param segOnly
	 * 		Whether to output segments only.
	 *
	 * @throws IOException
	 * 		If one of the model files failed to load.
	 */
	public static void run(IAccessible input, IAccessible output, boolean segOnly)
			throws IOException {
		IInputProvider inputProvider = input.toInputProvider();
		IOutputHandler outputHandler = output.toOutputHandler();
		run(inputProvider, outputHandler, segOnly);
	}

	/**
	 * Runs the segmentation program with given input and output and the {@code
	 * segOnly} flag and output execution time.
	 *
	 * @param input
	 * 		The {@link IInputProvider} used as input.
	 * @param output
	 * 		The {@link IOutputHandler} used as output.
	 * @param segOnly
	 * 		Whether to output segments only.
	 *
	 * @throws IOException
	 * 		If one of the model files failed to load.
	 */
	public static void run(IInputProvider input, IOutputHandler output, boolean segOnly)
			throws IOException {
		long time = -System.currentTimeMillis();
		Thulac.split(input, output, segOnly);
		time += System.currentTimeMillis();
		System.out.printf("Time elapsed: %dms\n", time);
	}

	/**
	 * Runs the segmentation program in profiler mode, that is, provide fastest input
	 * and output to measure the actual time consumed by the program. Note that this
	 * method does not output the result, use {@link #run(IInputProvider, IOutputHandler,
	 * boolean)} or {@link #run(IAccessible, IAccessible, boolean)} if the result must be
	 * used afterwards.
	 *
	 * @param input
	 * 		The {@link IAccessible} used as input.
	 * @param segOnly
	 * 		Whether to output segments only.
	 *
	 * @throws IOException
	 * 		If one of the model files failed to load.
	 */
	public static void runProfiler(IAccessible input, boolean segOnly)
			throws IOException {
		run(new ProfilerInputProvider(input.toInputProvider()),
				new ProfilerOutputHandler(), segOnly);
	}

	/**
	 * Compare the output file and the answer file ({@code compareFile}) and calculate
	 * accuracy.<br>
	 * The comparison is done in such a way that, extracting split results from the
	 * files, the number of split positions in the output file which also exist in
	 * the compare file are counted.<br>
	 * This method requires outputFile to be generated with flag -seg_only
	 *
	 * @param inputFile
	 * 		The {@link IAccessible} used as input.
	 * @param compareFile
	 * 		The {@link IAccessible} used as answer.
	 * @param outputFile
	 * 		The {@link IAccessible} used as output.
	 *
	 * @throws IOException
	 * 		If an exception was thrown while reading the lines from {@code inputFile},
	 * 		{@code compareFile} or {@code outputFile}.
	 */
	public static void compare(
			IAccessible inputFile, IAccessible compareFile, IAccessible outputFile)
			throws IOException {
		// ADDITIONAL TO JAVADOC: ( *XXX* means XXX is a variable )
		// In other words, set *matches* to 0 initially. If THULAC splits input at
		// point A and so will a human, increase *matches* by one.
		// *total* is the number of total split segments in the answer, while
		// *segments* is that of the output from THULAC.
		// Accuracy is computed dividing *matches* by *total*, that is,
		//    accuracy = matches / total * 100%
		// *segments* is strictly greater than *matches*, therefore
		//    segments - matches
		// represent the number of wrongly split segments.

		List<String> input = inputFile.getLines();
		List<String> output = outputFile.getLines();
		List<String> compare = compareFile.getLines();

		int lines = input.size();
		List<List<Integer>> outputSeg = extractSegments(input, output);
		List<List<Integer>> compareSeg = extractSegments(input, compare);
		int matches = 0, segments = outputSeg.stream().mapToInt(List::size).sum(),
				total = compareSeg.stream().mapToInt(List::size).sum();
		for (int i = 0; i < lines; ++i) {
			List<Integer> outputLine = outputSeg.get(i);
			List<Integer> compareLine = compareSeg.get(i);
			matches += outputLine.stream().filter(compareLine::contains).count();
		}

		System.out.printf("Result: %d total, %d segments, %d matches, %.2f%% accuracy\n",
				total, segments, matches, 100f * matches / total);
	}

	private static List<List<Integer>> extractSegments(
			List<String> input, List<String> result) {
		List<List<Integer>> segments = new ArrayList<>();
		assertEquals("Line count of input and result doesn't match",
				input.size(), result.size());
		for (int i = 0, size = input.size(); i < size; ++i)
			segments.add(extractSegments(input.get(i), result.get(i)));
		return segments;
	}

	private static List<Integer> extractSegments(
			String input, String result) {
		// It is required that the result contains all the characters (code points)
		// that exist in the input. This also means that the input should not contain
		// whitespaces (ASCII space U+0020 and Chinese fullwidth space U+3000),
		// otherwise the behavior of the program is undefined.
		// If a character in the input if not found in the output, than an
		// AssertionError is thrown with a message which provides more details.

		// In addition, the result of splitting the input is represent by a list of
		// integers, each one, say N, means that the program finds it appropriate to
		// split the input after the Nth code Point.
		// To make it easier to understand, if N and M are two adjacent integers in the
		// returned list, then the Nth (inclusive) to the Mth (exclusive) code points
		// of the input together make a Chinese word.

		List<Integer> segments = new ArrayList<>();
		int[] cp1 = StringUtils.toCodePoints(input),
				cp2 = StringUtils.toCodePoints(result);
		int pointer = 0, len1 = cp1.length, len2 = cp2.length;
		assertTrue("Result shorter than input!", len1 <= len2);

		int i = 0;
		for (; i < len1 && pointer < len2; ++i, ++pointer) {
			int c = cp1[i];
			if (cp2[pointer] == c) continue;
			segments.add(i);
			for (; pointer < len2 && cp2[pointer] != c; ++pointer) ;
			if (pointer == len2) throw new AssertionError(
					new StringBuilder("Character '").appendCodePoint(c)
							.append("' not found in result string!\n")
							.append("Input: ").append(input)
							.append("Result: ").append(result).toString());
		}
		if (i != len1) throw new AssertionError(
				new StringBuilder("Character '").appendCodePoint(cp1[i])
						.append("' not found in result string!\n")
						.append("Input: ").append(input)
						.append("Result: ").append(result).toString());

		return segments;
	}

	private static final String RESOURCES_DIRECTORY = "/";
	// the temp directory used to store output files
	private static final String TEMP_DIRECTORY = "build/tmp/tests/";

	static {
		try { // create tmp directory, otherwise IOException would be thrown
			Files.createDirectories(Paths.get(TEMP_DIRECTORY));
		} catch (IOException e) {
			throw new RuntimeException("Unable to create temp directory!", e);
		}
	}

	public static IAccessible fileAt(String name) {
		return IAccessible.fileAt(name);
	}

	public static IAccessible tempAt(String name) {
		return fileAt(TEMP_DIRECTORY + name);
	}

	public static IAccessible resourceAt(String name) {
		return IAccessible.resourceAt(RESOURCES_DIRECTORY + name);
	}
}