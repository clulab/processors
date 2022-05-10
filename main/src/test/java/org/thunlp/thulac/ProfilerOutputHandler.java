package org.thunlp.thulac;

import org.thunlp.thulac.data.TaggedWord;
import org.thunlp.thulac.io.IOutputHandler;

import java.io.IOException;
import java.util.List;

/**
 * An empty {@link IOutputHandler}, used in profiler to reduce time consumed by IO
 * operations.
 */
public class ProfilerOutputHandler implements IOutputHandler {
	@Override
	public void onProgramStart() {
	}

	@Override
	public void onProgramEnd() {
	}

	@Override
	public void handleLineSegment(List<TaggedWord> words,
								  boolean segOnly, char separator) {
	}

	@Override
	public void handleLineStart() throws IOException {
	}

	@Override
	public void handleLineEnd() throws IOException {
	}
}
