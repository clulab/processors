package org.thunlp.thulac.postprocess;

import org.thunlp.thulac.data.Dat;
import org.thunlp.thulac.data.DatMaker;
import org.thunlp.thulac.data.TaggedWord;

import java.io.IOException;
import java.util.List;

/**
 * A postprocess pass which scans the word list, extract words that are found in the
 * dictionary and tag them.<br>
 * To show its behavior more clearly, we raise the following example:<br>
 * Assume that the input {@code sentence} is {@code "A", "B", "C", "DE"}, and the word
 * list specified by {@link #dictionary} is {@code "AB", "ABC", "ABCD"}.<br>
 * The {@link #process(List)} method tends to find the longest concatenation of words
 * in the word list which exists in the dictionary and combine these words into one
 * single {@link TaggedWord}.<br>
 * So, as for this example, all concatenations of words in the list beginning from
 * index 0 would be: {@code "A", "AB", "ABC", "ABCDE"}, in which only {@code "AB"} and
 * {@code "ABC"} is present in {@link #dictionary}.<br>
 * In this case, the longest concatenation would be {@code "ABC"} and therefore the
 * words {@code "A", "B", "C"} are removed and one single word {@code "ABC"} is added
 * to the word list, which makes the final output from {@link #process(List)} {@code
 * "ABC", "DE"}.<br>
 * Please notice that although {@code "ABCD"} exists in {@link #dictionary}, the
 * {@link #process(List)} method will not attempt to split whole words apart.
 */
public class DictionaryPass implements IPostprocessPass {
	private Dat dictionary;
	private String tag;

	public DictionaryPass(String dictFile, String tag, boolean isTxt)
			throws IOException {
		this.tag = tag;
		if (isTxt) this.dictionary = DatMaker.readFromTxtFile(dictFile);
		else this.dictionary = new Dat(dictFile);
	}

	@Override
	public void process(List<TaggedWord> sentence) {
		if (this.dictionary == null || sentence.isEmpty()) return;

		for (int i = 0, size = sentence.size(); i < size; i++) {
			// search for longest concatenation which exists in dict
			StringBuilder sb = new StringBuilder();
			String longest = null, current;
			int longestIndex = -1;
			for (int j = i; j < size; j++) {
				current = sb.append(sentence.get(j).word).toString();
				if (!this.dictionary.containsPrefix(current)) break;
				if (this.dictionary.contains(current)) {
					longest = current;
					longestIndex = j;
				}
			}

			// if found, combine the words and update the sentence
			if (longest == null) continue;
			sentence.set(i, new TaggedWord(longest, this.tag));
			for (int j = longestIndex; j > i; --j) sentence.remove(j);
			size = sentence.size();
		}
	}
}
