package org.thunlp.thulac;

import org.thunlp.thulac.io.IInputProvider;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * An implementation of {@link IInputProvider}, used in profiler to reduce time
 * consumed by IO operations, wrapping outside another {@link IInputProvider}, reading
 * the lines provided in advance and store them in memory. Note that they might lead to
 * high memory usage for large files.
 */
public class ProfilerInputProvider implements IInputProvider {
	private Iterator<List<String>> linesIterator;

	public ProfilerInputProvider(IInputProvider inputProvider) throws IOException {
		List<List<String>> lines = new ArrayList<>();
		for (List<String> lineSegments = inputProvider.provideInput();
			 lineSegments != null; lineSegments = inputProvider.provideInput())
			lines.add(lineSegments);
		this.linesIterator = lines.iterator();
	}

	@Override
	public void onProgramStart() {
	}

	@Override
	public void onProgramEnd() {
	}

	@Override
	public List<String> provideInput() throws IOException {
		if (this.linesIterator.hasNext()) return this.linesIterator.next();
		else return null;
	}
}
