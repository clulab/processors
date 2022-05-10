package org.thunlp.thulac.postprocess;

import org.thunlp.thulac.data.TaggedWord;

import java.util.List;

/**
 * An interface which process the list of {@link TaggedWord} after segmentation.
 */
public interface IPostprocessPass {
	/**
	 * Process the list of {@link TaggedWord}.
	 *
	 * @param sentence
	 * 		The list of {@link TaggedWord}.
	 */
	void process(List<TaggedWord> sentence);
}
