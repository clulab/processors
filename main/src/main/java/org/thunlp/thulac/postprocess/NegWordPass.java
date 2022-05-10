package org.thunlp.thulac.postprocess;

import org.thunlp.thulac.data.Dat;
import org.thunlp.thulac.data.TaggedWord;
import org.thunlp.thulac.util.StringUtils;

import java.io.IOException;
import java.util.List;

/**
 * A postprocess pass which recognises certain negative phrases (for example, "not good
 * enough" in English) and separate the negative word from the rest parts in the phrase
 * (in this example, "not good" is converted into "not" and "good enough") and give the
 * separated parts their respective tags. A {@link Dat} file stores the list of negative
 * phrases to be separated by {@link #process(List)}.
 */
public class NegWordPass implements IPostprocessPass {
	private Dat negPhrases;

	public NegWordPass(String negDatFile) throws IOException {
		this.negPhrases = new Dat(negDatFile);
	}

	@Override
	public void process(List<TaggedWord> sentence) {
		if (this.negPhrases == null || sentence.isEmpty()) return;

		for (int i = sentence.size() - 1; i >= 0; --i) {
			TaggedWord tagged = sentence.get(i);
			if (this.negPhrases.contains(tagged.word)) {
				int[] codePoints = StringUtils.toCodePoints(tagged.word);
				String word = StringUtils.toString(codePoints, 1, codePoints.length - 1);
				sentence.add(i + 1, new TaggedWord(word, "v"));
				tagged.word = StringUtils.toString(codePoints[0]);
				tagged.tag = "d";
			}
		}
	}
}
