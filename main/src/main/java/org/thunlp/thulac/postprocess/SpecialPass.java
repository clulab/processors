package org.thunlp.thulac.postprocess;

import org.thunlp.thulac.data.TaggedWord;

import java.util.List;

/**
 * A postprocess path which deals with special cases.
 */
public class SpecialPass implements IPostprocessPass {
	@Override
	public void process(List<TaggedWord> sentence) {
		this.filterHTTPURLs(sentence);
	}

	/**
	 * Tag "x" for HTTP URLs.<br>
	 * HTTP URLs are identified as is, if the word is longer than 4 characters and
	 * starts with "http". (to conform with both {@code http} and {@code https} schemes)
	 *
	 * @param sentence
	 * 		The input sentence.
	 */
	private void filterHTTPURLs(List<TaggedWord> sentence) {
		for (TaggedWord tagged : sentence)
			if (tagged.word.length() >= 5 && tagged.word.startsWith("http"))
				tagged.tag = "x";
	}
}
