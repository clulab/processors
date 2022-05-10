package org.thunlp.thulac;

import org.thunlp.thulac.io.IInputProvider;
import org.thunlp.thulac.io.IOutputHandler;
import org.thunlp.thulac.util.IOUtils;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * An interface which provides a set of common actions for resources and files used in
 * {@link TestHelper}. In practice, an {@code abstract class} is used instead of an
 * {@code interface} because interfaces does not allow private nested classes. Despite
 * of this, this class can be used just like an interface.
 */
public abstract class IAccessible {
	/**
	 * Create an instance of {@link IAccessible} with the given resource name.
	 *
	 * @param name
	 * 		The resource name.
	 *
	 * @return The {@link IAccessible} created.
	 *
	 * @see <a href="http://docs.oracle.com/javase/6/docs/technotes/guides/lang/resources.html"
	 * >Resources</a>
	 */
	public static IAccessible resourceAt(String name) {
		return new AccessibleResource(name);
	}

	/**
	 * Create an instance of {@link IAccessible} with the given file name.
	 *
	 * @param name
	 * 		The file name.
	 *
	 * @return The {@link IAccessible} created.
	 */
	public static IAccessible fileAt(String name) {
		return new AccessibleFiles(name);
	}

	/**
	 * Trim lines and remove empty ones.
	 *
	 * @param lines
	 * 		The raw lines as {@link Stream<String>}.
	 *
	 * @return The trimmed and non-empty lines as {@link List<String>}.
	 */
	private static List<String> getLines(Stream<String> lines) {
		return lines.map(String::trim)
				.filter(line -> !line.isEmpty())
				.collect(Collectors.toList());
	}

	/**
	 * Implementation of {@link IAccessible} reading from resources.
	 */
	private static class AccessibleResource extends IAccessible {
		private URI uri;
		private URL url;

		public AccessibleResource(String resourceName) {
			this.url = AccessibleResource.class.getResource(resourceName);
			try {
				this.uri = this.url.toURI();
			} catch (URISyntaxException ignored) { // should not happen
			}
		}

		@Override
		public List<String> getLines() throws IOException {
			return IAccessible.getLines(Files.lines(Paths.get(this.uri)));
		}

		@Override
		public IOutputHandler toOutputHandler() throws IOException {
			throw new UnsupportedOperationException("Output not supported on resources!");
		}

		@Override
		public InputStream toInputStream() throws IOException {
			return this.url.openStream();
		}
	}

	/**
	 * Implementation of {@link IAccessible} reading from and writing to files.
	 */
	private static class AccessibleFiles extends IAccessible {
		private String filename;

		public AccessibleFiles(String filename) {
			this.filename = filename;
		}

		@Override
		public List<String> getLines() throws IOException {
			return Files.readAllLines(Paths.get(this.filename));
		}

		@Override
		public IInputProvider toInputProvider() throws IOException {
			return IOUtils.inputFromFile(this.filename);
		}

		@Override
		public IOutputHandler toOutputHandler() throws IOException {
			return IOUtils.outputToFile(this.filename);
		}

		@Override
		public InputStream toInputStream() throws IOException {
			return new FileInputStream(this.filename);
		}
	}

	/**
	 * Return the content of this resource / file separated into individual lines.
	 *
	 * @return Content of this resource / file as a list of strings.
	 */
	public abstract List<String> getLines() throws IOException;

	/**
	 * Create a {@link IInputProvider} with this resource / file.
	 *
	 * @return The {@link IInputProvider} created.
	 */
	public IInputProvider toInputProvider() throws IOException {
		return IOUtils.inputFromInputStream(this.toInputStream());
	}

	/**
	 * Create a {@link IOutputHandler} with this resource / file.
	 *
	 * @return The {@link IOutputHandler} created.
	 */
	public abstract IOutputHandler toOutputHandler() throws IOException;

	/**
	 * Create a {@link InputStream} with this resource / file.
	 *
	 * @return The {@link InputStream} created.
	 */
	public abstract InputStream toInputStream() throws IOException;
}
