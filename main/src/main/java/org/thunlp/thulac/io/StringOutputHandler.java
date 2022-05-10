package org.thunlp.thulac.io;

import org.thunlp.thulac.data.TaggedWord;

import java.io.IOException;
import java.util.List;

/**
 * An implementation of {@link IOutputHandler} to allow access to the output in form of
 * {@link String}.
 */
public class StringOutputHandler implements IOutputHandler {
	private StringBuilder str;

	public StringOutputHandler() {
		this.str = new StringBuilder();
	}

	@Override
	public void onProgramStart() {
	}

	@Override
	public void onProgramEnd() {
	}

	@Override
	public void handleLineSegment(List<TaggedWord> words,
								  boolean segOnly, char separator) {
		if (segOnly) {
			for (TaggedWord word : words) {
				this.str.append(word.word);
				this.str.append(' ');
			}
		} else {
			for (TaggedWord word : words) {
				this.str.append(word.word);
				this.str.append(separator);
				this.str.append(word.tag);
				this.str.append(' ');
			}
		}
	}

	@Override
	public void handleLineStart() throws IOException {
	}

	@Override
	public void handleLineEnd() throws IOException {
		this.str.append("\n");
	}

	public String getString() {
		return this.str.toString();
	}
}
