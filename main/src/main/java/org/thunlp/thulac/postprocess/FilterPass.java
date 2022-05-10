package org.thunlp.thulac.postprocess;

import org.thunlp.thulac.data.Dat;
import org.thunlp.thulac.data.TaggedWord;
import org.thunlp.thulac.util.StringUtils;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.thunlp.thulac.util.CodePointUtils.CHINESE_DIGITS;
import static org.thunlp.thulac.util.CodePointUtils.DIGITS;

/**
 * A postprocess pass which filters forbidden tags from the the word list.
 */
public class FilterPass implements IPostprocessPass {
	/**
	 * Tags allowed to pass the filter. Words with tags out of this list will be
	 * discarded.
	 */
	private static final Set<String> ALLOWED_TAGS = new HashSet<>(Arrays.asList(
			"n", "np", "ns", "ni", "nz", "v", "a", "id", "t", "uw"));

	private Dat xuDat;
	private Dat timeDat;

	public FilterPass(String xuDatFile, String timeDatFile) throws IOException {
		this.xuDat = new Dat(xuDatFile);
		this.timeDat = new Dat(timeDatFile);
	}

	/**
	 * Returns {@code true} is one of the following is true:<br>
	 * <ul>
	 * <li>Word contains one or more normal digits.</li>
	 * <li>Word contains two or more Chinese digits.</li>
	 * <li>Word is in dictionary specified by {@link #timeDat}.</li>
	 * </ul>
	 *
	 * @param word
	 * 		The word to check.
	 *
	 * @return Whether the word contains number digits.
	 */
	private boolean hasNumber(String word) {
		int count = 0;
		for (int c : StringUtils.toCodePoints(word))
			if (DIGITS.indexOf(c) != -1) return true;
			else if (CHINESE_DIGITS.indexOf(c) != -1 && count++ != 0) return true;
		return this.timeDat.contains(word);
	}

	/**
	 * Remove words in segmented word list if one of the following is true:<br>
	 * <ul>
	 * <li>Tag of word not in {@link #ALLOWED_TAGS}.</li>
	 * <li>Word in dictionary specified by {@link #timeDat}.</li>
	 * <li>Word has tag "t" and {@linkplain #hasNumber(String) hasNumber(word)}
	 * returns {@code true}.</li>
	 * </ul>
	 *
	 * @param sentence
	 * 		The sentence to filter.
	 */
	@Override
	public void process(List<TaggedWord> sentence) {
		if (this.xuDat == null || this.timeDat == null || sentence.isEmpty()) return;

		for (int i = sentence.size() - 1; i >= 0; --i) {
			String word = sentence.get(i).word;
			String tag = sentence.get(i).tag;
			if (!ALLOWED_TAGS.contains(tag) || this.xuDat.contains(word) ||
					("t".equals(tag) && this.hasNumber(word))) sentence.remove(i);
		}
	}
}
