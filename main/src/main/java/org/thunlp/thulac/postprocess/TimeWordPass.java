package org.thunlp.thulac.postprocess;

import org.thunlp.thulac.data.TaggedWord;
import org.thunlp.thulac.util.StringUtils;

import java.util.List;

import static org.thunlp.thulac.util.CodePointUtils.DIGITS;
import static org.thunlp.thulac.util.CodePointUtils.generate;

/**
 * A postprocess pass which combine words which together represent a time period into
 * one word.<br>
 * For example, for input word list {@code "A", "B", "C1", "2", "34" "year"} ("year"
 * here can by any Chinese time unit in {@link #TIME_UNITS}), the output should be:
 * {@code "A", "B", "C1", "234year"}.<br>
 * It can be seen that {@code "C1"} is not concatenated to {@code "234year"}, since it
 * contains non-digit characters.<br>
 * Please notice that this class is able to deal with full-width numbers like U+FF10
 * (full-width digit 1) yet not Chinese digits like U+3007 (Chinese for "one").
 */
public class TimeWordPass implements IPostprocessPass {
	/**
	 * Chinese characters which represent time units: (description in English)<br>
	 * YEAR: U+5E74, MONTH: U+6708, DAY: U+65E5 & U+53F7, HOUR: U+65F6 & U+70B9,
	 * MINUTE: U+5206, SECOND: U+79D2.
	 */
	private static final String TIME_UNITS = generate('\u5E74', '\u6708', '\u65E5',
			'\u53F7', '\u65F6', '\u70B9', '\u5206', '\u79D2');

	/**
	 * {@code word} is a number if all the code points in {@code word} is a
	 * {@linkplain org.thunlp.thulac.util.CodePointUtils#DIGITS digit}.
	 *
	 * @param word
	 * 		The word to check.
	 *
	 * @return Whether this {@code word} is a number.
	 */
	private boolean isNumber(String word) {
		for (int codePoint : StringUtils.toCodePoints(word))
			if (DIGITS.indexOf(codePoint) == -1) return false;
		return true;
	}

	/**
	 * {@code word} is a time unit if and only if: {@code word} contains only ont code
	 * point and this code point is a {@linkplain #TIME_UNITS time unit}.
	 *
	 * @param word
	 * 		The word to check.
	 *
	 * @return Whether this {@code word} is a time unit.
	 */
	private boolean isTimeUnit(String word) {
		return StringUtils.codePointCount(word) == 1 &&
				TIME_UNITS.indexOf(word.codePointAt(0)) != -1;
	}

	@Override
	public void process(List<TaggedWord> sentence) {
		boolean isTimeWord = false;
		for (int i = sentence.size() - 1; i >= 0; i--) {
			TaggedWord tagged = sentence.get(i);
			if (this.isTimeUnit(tagged.word)) isTimeWord = true;
			else if (isTimeWord && this.isNumber(tagged.word)) {
				tagged.word += sentence.remove(i + 1).word;
				tagged.tag = "t";
			} else isTimeWord = false;
		}
	}
}
