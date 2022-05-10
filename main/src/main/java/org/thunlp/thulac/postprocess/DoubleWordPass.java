package org.thunlp.thulac.postprocess;

import org.thunlp.thulac.data.TaggedWord;
import org.thunlp.thulac.util.StringUtils;

import java.util.List;

import static org.thunlp.thulac.util.CodePointUtils.SPECIAL_CHARS;

/**
 * A postprocess pass combining adjacent words which can form a double word together.
 *
 * @see #canFormDoubleWord(String, String)
 */
public class DoubleWordPass implements IPostprocessPass {
	@Override
	public void process(List<TaggedWord> sentence) {
		if (sentence.size() <= 1) return;

		TaggedWord tagged, last = sentence.get(sentence.size() - 1);
		for (int i = sentence.size() - 2; i >= 0; --i, last = tagged) {
			tagged = sentence.get(i);
			if (this.canFormDoubleWord(tagged.word, last.word)) {
				tagged.word += last.word;
				sentence.remove(i + 1);
			}
		}
	}

	/**
	 * Two words can form a double word if and only of:<br>
	 * <ul>
	 * <li>Both words contain only one code points and,</li>
	 * <li>The only code points in both words are identical and,</li>
	 * <li>This code point is not a {@linkplain org.thunlp.thulac.util.CodePointUtils#SPECIAL_CHARS
	 * special character}.</li>
	 * </ul>
	 *
	 * @param first
	 * 		The first word.
	 * @param second
	 * 		The second word.
	 *
	 * @return If the two words can form a double word.
	 */
	private boolean canFormDoubleWord(String first, String second) {
		if (StringUtils.codePointCount(first) != 1 ||
				StringUtils.codePointCount(second) != 1) return false;
		int firstCP = first.codePointAt(0);
		int secondCP = second.codePointAt(0);
		return firstCP == secondCP && SPECIAL_CHARS.indexOf(firstCP) == -1;
	}
}
