package org.thunlp.thulac.io;

import org.thunlp.thulac.util.IOUtils;

import java.io.IOException;
import java.util.List;

/**
 * An implementation of {@link IInputProvider} which retrieves input from a {@link
 * String}.
 */
public class StringInputProvider implements IInputProvider {
	private String[] lines;
	private int pointer;

	public StringInputProvider(String input) {
		// input must be non-null
		if (input == null) throw new IllegalArgumentException("input == null!");
		this.lines = input.split("\n"); // empty lines are discarded
		this.pointer = 0;
	}

	@Override
	public void onProgramStart() {
	}

	@Override
	public void onProgramEnd() {
	}

	@Override
	public List<String> provideInput() throws IOException {
		if (this.pointer == this.lines.length) return null;
		return IOUtils.getLineSegments(this.lines[pointer++]);
	}
}
