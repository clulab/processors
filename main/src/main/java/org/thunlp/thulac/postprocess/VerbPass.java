package org.thunlp.thulac.postprocess;

import org.thunlp.thulac.data.Dat;
import org.thunlp.thulac.data.TaggedWord;

import java.io.IOException;
import java.util.List;

/**
 * A postprocess pass which identifies <i>Model Verbs</i> and <i>Directional Verbs</i>.
 *
 * @see <a href="https://en.wikipedia.org/wiki/Modal_verb">Model Verb</a>
 * @see <a href="https://zh.wikipedia.org/wiki/%E8%83%BD%E6%84%BF%E5%8A%A8%E8%AF%8D">
 * Model Verb in Chinese</a>
 * @see <a href="http://baike.baidu.com/item/%E8%B6%8B%E5%90%91%E5%8A%A8%E8%AF%8D">
 * Directional Verb</a>
 * @see <a href="https://zh.wikipedia.org/wiki/%E6%B1%89%E8%AF%AD%E8%AF%8D%E7%B1%BB#.E5.88.86.E7.B1.BB_2>
 * Chinese Word Categories</a>
 */
public class VerbPass implements IPostprocessPass {
	/**
	 * {@link Dat} file containing word list of Model Verbs.
	 */
	private Dat vM;
	/**
	 * {@link Dat} file containing word list of Directional Verbs.
	 */
	private Dat vD;
	/**
	 * The tag to represent ordinary verbs.
	 */
	private String tag;

	public VerbPass(String vMFile, String vDFile) throws IOException {
		this.vM = new Dat(vMFile);
		this.vD = new Dat(vDFile);
		this.tag = "v";
	}

	/**
	 * Within two adjacent verbs, only the first one might be a Model Verb and only the
	 * second one might be a Directional Verb.
	 *
	 * @param sentence
	 * 		The input sentence.
	 */
	@Override
	public void process(List<TaggedWord> sentence) {
		if (this.vM == null || this.vD == null || sentence.isEmpty()) return;

		TaggedWord last = sentence.get(0), tagged;
		for (int i = 1, size = sentence.size(); i < size; i++, last = tagged) {
			tagged = sentence.get(i + 1);
			if (this.tag.equals(last.tag) && this.tag.equals(tagged.tag))
				if (this.vM.contains(last.word)) tagged.tag = "vm";
				else if (this.vD.contains(tagged.word)) tagged.tag = "vd";
		}
	}
}
