package org.thunlp.thulac.util;

import org.thunlp.thulac.io.*;

import java.io.*;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.charset.UnsupportedCharsetException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A class which provides static utility methods used dealing with {@link IInputProvider}
 * and {@link IOutputHandler}. Some of them construct instances of {@link IInputProvider}
 * and {@link IOutputHandler}, hiding the implementation details from the user. Others
 * can be used within implementations of {@link IInputProvider} and
 * {@link IOutputHandler}, avoiding code duplicates.
 *
 * @see IInputProvider
 * @see IOutputHandler
 */
public class IOUtils {
	/**
	 * Creates an instance of {@link IInputProvider} which retrieves input from
	 * {@link System#in}, using the default charset as the input encoding.
	 *
	 * @return The {@link IInputProvider} created.
	 */
	public static IInputProvider inputFromConsole() {
		return inputFromInputStream(System.in); // use default charset for System.in
	}

	/**
	 * Creates an instance of {@link IInputProvider} which retrieves input from a given
	 * {@link InputStream} using UTF-8 as encoding.<br>
	 * It is recommended to use {@link #inputFromFile(File, Charset)} when reading
	 * input from files, since it takes better advantage of Java NIO and have better
	 * performances.
	 *
	 * @param in
	 * 		The {@link InputStream} to retrieve input from.
	 *
	 * @return The {@link IInputProvider} created.
	 */
	public static IInputProvider inputFromInputStream(InputStream in) {
		return inputFromInputStream(in, (Charset) null);
	}

	/**
	 * Creates an instance of {@link IInputProvider} which retrieves input from a given
	 * {@link InputStream} using a given charset as encoding.<br>
	 * It is recommended to use {@link #inputFromFile(File, Charset)} when reading
	 * input from files, since it takes better advantage of Java NIO and have better
	 * performances.
	 *
	 * @param in
	 * 		The {@link InputStream} to retrieve input from.
	 * @param charsetName
	 * 		The optional name of the charset to use, defaulted to "UTF-8".
	 *
	 * @return The {@link IInputProvider} created.
	 */
	public static IInputProvider inputFromInputStream(InputStream in, String charsetName)
			throws UnsupportedCharsetException {
		return inputFromInputStream(in, forName(charsetName));
	}

	/**
	 * Creates an instance of {@link IInputProvider} which retrieves input from a given
	 * {@link InputStream} using a given charset as encoding.<br>
	 * It is recommended to use {@link #inputFromFile(File, Charset)} when reading
	 * input from files, since it takes better advantage of Java NIO and have better
	 * performances.
	 *
	 * @param in
	 * 		The {@link InputStream} to retrieve input from.
	 * @param charset
	 * 		The optional charset to use, defaulted to UTF-8.
	 *
	 * @return The {@link IInputProvider} created.
	 */
	public static IInputProvider inputFromInputStream(InputStream in, Charset charset) {
		return new ReaderInputProvider(new BufferedReader(
				new InputStreamReader(in, getOrDefault(charset))));
	}

	/**
	 * Creates an instance of {@link IInputProvider} which retrieves input from the
	 * given file using UTF-8 as file encoding.
	 *
	 * @param filename
	 * 		The name of the file to retrieve input from.
	 *
	 * @return The {@link IInputProvider} created.
	 *
	 * @throws IOException
	 * 		If the file does not exist or is not readable.
	 */
	public static IInputProvider inputFromFile(String filename) throws IOException {
		return inputFromFile(filename, (Charset) null);
	}

	/**
	 * Creates an instance of {@link IInputProvider} which retrieves input from the
	 * given file using UTF-8 as file encoding.
	 *
	 * @param file
	 * 		The file to retrieve input from.
	 *
	 * @return The {@link IInputProvider} created.
	 *
	 * @throws IOException
	 * 		If the file does not exist or is not readable.
	 */
	public static IInputProvider inputFromFile(File file) throws IOException {
		return inputFromFile(file, (Charset) null);
	}

	/**
	 * Creates an instance of {@link IInputProvider} which retrieves input from the
	 * given file using a given charset as encoding.
	 *
	 * @param filename
	 * 		The name of the file to retrieve input from.
	 * @param charsetName
	 * 		The optional name of the charset to use, defaulted to "UTF-8".
	 *
	 * @return The {@link IInputProvider} created.
	 *
	 * @throws IOException
	 * 		If the file does not exist or is not readable.
	 * @throws UnsupportedCharsetException
	 * 		If the charset referred to by the given name is not supported.
	 */
	public static IInputProvider inputFromFile(String filename, String charsetName)
			throws IOException, UnsupportedCharsetException {
		return inputFromFile(filename, forName(charsetName));
	}

	/**
	 * Creates an instance of {@link IInputProvider} which retrieves input from the
	 * given file using a given charset as encoding.
	 *
	 * @param filename
	 * 		The file to retrieve input from.
	 * @param charset
	 * 		The optional file encoding to use, defaulted to UTF-8.
	 *
	 * @return The {@link IInputProvider} created.
	 *
	 * @throws IOException
	 * 		If the file does not exist or is not readable.
	 */
	public static IInputProvider inputFromFile(String filename, Charset charset)
			throws IOException {
		if (filename == null) return null; // new File(null) throws NPE
		return inputFromFile(new File(filename), charset);
	}

	/**
	 * Creates an instance of {@link IInputProvider} which retrieves input from the
	 * given file using a given charset as encoding.
	 *
	 * @param file
	 * 		The name of the file to retrieve input from.
	 * @param charsetName
	 * 		The optional name of the file encoding to use, defaulted to UTF-8.
	 *
	 * @return The {@link IInputProvider} created.
	 *
	 * @throws IOException
	 * 		If the file does not exist or is not readable.
	 * @throws UnsupportedCharsetException
	 * 		If the charset referred to by the given	name is not supported.
	 */
	public static IInputProvider inputFromFile(File file, String charsetName)
			throws IOException, UnsupportedCharsetException {
		return inputFromFile(file, forName(charsetName));
	}

	/**
	 * Creates an instance of {@link IInputProvider} which retrieves input from the
	 * given file using a given charset as encoding.
	 *
	 * @param file
	 * 		The name of the file to retrieve input from.
	 * @param charset
	 * 		The optional file encoding to use, defaulted to UTF-8.
	 *
	 * @return The {@link IInputProvider} created.
	 *
	 * @throws IOException
	 * 		If the file does not exist or is not readable.
	 */
	public static IInputProvider inputFromFile(File file, Charset charset)
			throws IOException {
		if (file == null) return null;
		return new ReaderInputProvider(
				Files.newBufferedReader(Paths.get(file.toURI()), getOrDefault(charset)));
	}

	/**
	 * Creates an instance of {@link IInputProvider} which retrieves input from the
	 * given {@link String}.
	 *
	 * @param input
	 * 		The input string.
	 *
	 * @return The {@link IInputProvider} created.
	 */
	public static IInputProvider inputFromString(String input) {
		if (input == null) return null;
		return new StringInputProvider(input);
	}

	/**
	 * Creates an instance of {@link IOutputHandler} which writes output to
	 * {@link System#out}, using the default charset as the output encoding.
	 *
	 * @return The {@link IOutputHandler} created.
	 */
	public static IOutputHandler outputToConsole() {
		return new WriterOutputHandler(new BufferedWriter(
				new OutputStreamWriter(System.out)));
	}

	/**
	 * Creates an instance of {@link IOutputHandler} which writes output to a given
	 * {@link OutputStream} using UTF-8 as encoding.<br>
	 * It is recommended to use {@link #outputToFile(File, String)} when writing
	 * output to files, since it takes better advantage of Java NIO and have better
	 * performances.
	 *
	 * @param out
	 * 		The {@link OutputStream} to write output to.
	 *
	 * @return The {@link IOutputHandler} created.
	 */
	public static IOutputHandler outputToOutputStream(OutputStream out) {
		return outputToOutputStream(out, (Charset) null);
	}

	/**
	 * Creates an instance of {@link IOutputHandler} which writes output to a given
	 * {@link OutputStream} using a given charset as encoding.<br>
	 * It is recommended to use {@link #outputToFile(File, String)} when writing
	 * output to files, since it takes better advantage of Java NIO and have better
	 * performances.
	 *
	 * @param out
	 * 		The {@link OutputStream} to write output to.
	 * @param charsetName
	 * 		The optional name of the charset to use, defaulted to UTF-8.
	 *
	 * @return The {@link IOutputHandler} created.
	 *
	 * @throws UnsupportedCharsetException
	 * 		If the charset referred to by the name is not supported.
	 */
	public static IOutputHandler outputToOutputStream(
			OutputStream out, String charsetName) throws UnsupportedCharsetException {
		return outputToOutputStream(out, forName(charsetName));
	}

	/**
	 * Creates an instance of {@link IOutputHandler} which writes output to a given
	 * {@link OutputStream} using a given charset as encoding.<br>
	 * It is recommended to use {@link #outputToFile(File, String)} when writing
	 * output to files, since it takes better advantage of Java NIO and have better
	 * performances.
	 *
	 * @param out
	 * 		The {@link OutputStream} to write output to.
	 * @param charset
	 * 		The optional charset to use, defaulted to UTF-8.
	 *
	 * @return The {@link IOutputHandler} created.
	 */
	public static IOutputHandler outputToOutputStream(OutputStream out, Charset charset) {
		return new WriterOutputHandler(new BufferedWriter(
				new OutputStreamWriter(out, getOrDefault(charset))));
	}

	/**
	 * Creates an instance of {@link IOutputHandler} which writes output to the
	 * given file using UTF-8 as file encoding.
	 *
	 * @param filename
	 * 		The name of the file to output to.
	 *
	 * @return The {@link IOutputHandler} created.
	 *
	 * @throws IOException
	 * 		If the file cannot be created or is not writable.
	 */
	public static IOutputHandler outputToFile(String filename) throws IOException {
		return outputToFile(filename, (Charset) null);
	}

	/**
	 * Creates an instance of {@link IOutputHandler} which writes output to the
	 * given file using UTF-8 as file encoding.
	 *
	 * @param file
	 * 		The file to output to.
	 *
	 * @return The {@link IOutputHandler} created.
	 *
	 * @throws IOException
	 * 		If the file cannot be created or is not writable.
	 */
	public static IOutputHandler outputToFile(File file) throws IOException {
		return outputToFile(file, (Charset) null);
	}

	/**
	 * Creates an instance of {@link IOutputHandler} which writes output to the
	 * given file using a given charset as encoding.
	 *
	 * @param filename
	 * 		The name of the file to output to.
	 * @param charsetName
	 * 		The optional name of the charset to use, defaulted to "UTF-8".
	 *
	 * @return The {@link IOutputHandler} created.
	 *
	 * @throws IOException
	 * 		If the file cannot be created or is not writable.
	 * @throws UnsupportedCharsetException
	 * 		If the charset referred to by the given name is not supported.
	 */
	public static IOutputHandler outputToFile(String filename, String charsetName)
			throws IOException, UnsupportedCharsetException {
		return outputToFile(filename, forName(charsetName));
	}

	/**
	 * Creates an instance of {@link IOutputHandler} which writes output to the
	 * given file using a given charset as encoding.
	 *
	 * @param filename
	 * 		The name of the file to output to.
	 * @param charset
	 * 		The optional file encoding to use, defaulted to UTF-8.
	 *
	 * @return The {@link IOutputHandler} created.
	 *
	 * @throws IOException
	 * 		If the file cannot be created or is not writable.
	 */
	public static IOutputHandler outputToFile(String filename, Charset charset)
			throws IOException {
		if (filename == null) return null; // new File(null) throws NPE
		return outputToFile(new File(filename), charset);
	}

	/**
	 * Creates an instance of {@link IOutputHandler} which writes output to the
	 * given file using a given charset as encoding.
	 *
	 * @param file
	 * 		The file to output to.
	 * @param charsetName
	 * 		The optional name of the file encoding to use, defaulted to "UTF-8".
	 *
	 * @return The {@link IOutputHandler} created.
	 *
	 * @throws IOException
	 * 		If the file cannot be created or is not writable.
	 * @throws UnsupportedCharsetException
	 * 		If the charset referred to by the given name is not supported.
	 */
	public static IOutputHandler outputToFile(File file, String charsetName)
			throws IOException, UnsupportedCharsetException {
		return outputToFile(file, forName(charsetName));
	}

	/**
	 * Creates an instance of {@link IOutputHandler} which writes output to the
	 * given file using a given charset as encoding.
	 *
	 * @param file
	 * 		The file to output to.
	 * @param charset
	 * 		The optional file encoding to use, defaulted to UTF-8.
	 *
	 * @return The {@link IOutputHandler} created.
	 *
	 * @throws IOException
	 * 		If the file cannot be created or is not writable.
	 */
	public static IOutputHandler outputToFile(File file, Charset charset)
			throws IOException {
		if (file == null) return null;
		return new WriterOutputHandler(
				Files.newBufferedWriter(Paths.get(file.toURI()), getOrDefault(charset)));
	}

	/**
	 * Creates an instance of {@link StringOutputHandler} which writes output to an
	 * {@link String} in memory.<br>
	 * It is typical to use this method like this:
	 * <pre><code>
	 * StringOutputHandler output = IOUtils.outputToString();
	 * Thulac.split(input, output, segOnly); // or anything else
	 * String outputStr = output.getString();
	 * </code></pre>
	 *
	 * @return The {@link StringOutputHandler} created.
	 */
	public static StringOutputHandler outputToString() {
		return new StringOutputHandler();
	}

	private static final int MAX_LENGTH = 20000;
	private static final Pattern SPLIT_PATTERN =
			Pattern.compile(".*([\u3002\uff1f\uff01\uff1b;!?]|$)");

	/**
	 * Split a given line into a list of line segments if the line is too long. It is
	 * promised that each line segment either is the last one or ends with an
	 * punctuation character.
	 *
	 * @param line
	 * 		The line to split into line segments.
	 *
	 * @return The list of line segments split.
	 */
	public static List<String> getLineSegments(String line) {
		List<String> lineSegments = new ArrayList<>();
		if (line.length() < MAX_LENGTH) lineSegments.add(line);
		else { // split the line into short line segments
			Matcher matcher = SPLIT_PATTERN.matcher(line);
			while (matcher.find()) lineSegments.add(matcher.group());
		}
		return lineSegments;
	}

	/**
	 * Returns a {@link Charset} wich name {@code charset}. This methods differs from
	 * the {@link Charset#forName(String)} when {@code charset} is {@code null}, with
	 * this method returning {@code null} while {@link Charset#forName(String)} throws
	 * an NPE.
	 *
	 * @param charset
	 * 		The name of the {@link Charset}.
	 *
	 * @return The {@link Charset} with name {@code charset}.
	 *
	 * @throws UnsupportedCharsetException
	 * 		If the charset referred to by the given name is not supported.
	 */
	private static Charset forName(String charset) throws UnsupportedCharsetException {
		if (charset == null) return null;
		return Charset.forName(charset);
	}

	/**
	 * Returns the given {@link Charset} when non-null, or
	 * {@link StandardCharsets#UTF_8} otherwise, since many applications using
	 * {@link Charset} throws NPE if charset is {@code null}.
	 *
	 * @param charset
	 * 		The given {@link Charset}.
	 *
	 * @return {@code charset} when non-null, {@link StandardCharsets#UTF_8} otherwise.
	 */
	private static Charset getOrDefault(Charset charset) {
		return charset == null ? StandardCharsets.UTF_8 : charset;
	}
}
