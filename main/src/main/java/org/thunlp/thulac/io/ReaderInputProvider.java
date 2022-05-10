package org.thunlp.thulac.io;

import org.thunlp.thulac.util.IOUtils;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.List;

/**
 * An implementation of {@link IInputProvider} which retrieves input from a
 * {@link BufferedReader}.
 */
public class ReaderInputProvider implements IInputProvider {
	private BufferedReader reader;

	public ReaderInputProvider(BufferedReader reader) {
		// reader must be non-null
		if (reader == null) throw new IllegalArgumentException("reader == null!");
		this.reader = reader;
	}

	@Override
	public List<String> provideInput() throws IOException {
		String line = this.reader.readLine();
		if (line == null) return null;
		return IOUtils.getLineSegments(line);
	}

	@Override
	public void onProgramStart() {
	}

	@Override
	public void onProgramEnd() {
		try {
			this.reader.close(); // release system resources
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
