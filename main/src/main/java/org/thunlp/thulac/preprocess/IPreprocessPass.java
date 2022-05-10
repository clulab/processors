package org.thunlp.thulac.preprocess;

import org.thunlp.thulac.data.POCGraph;

/**
 * An interface which process the raw {@link String} before segmentation.
 */
public interface IPreprocessPass {
	/**
	 * Process the raw {@link String}.
	 *
	 * @param raw
	 * 		The raw {@link String} to process.
	 * @param graph
	 * 		The {@link POCGraph} to write to.
	 *
	 * @return The processed {@link String}.
	 */
	String process(String raw, POCGraph graph);
}
