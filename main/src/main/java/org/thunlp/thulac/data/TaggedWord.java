package org.thunlp.thulac.data;

/**
 * A class which represent a tagged word, that is, a word with a tag.
 */
public class TaggedWord {
	public String word;
	public String tag;

	public TaggedWord() {
		this.word = "";
	}

	public TaggedWord(String word, String tag) {
		this.word = word;
		this.tag = tag;
	}
}
