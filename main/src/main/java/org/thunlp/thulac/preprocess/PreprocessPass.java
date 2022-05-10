package org.thunlp.thulac.preprocess;

import org.thunlp.thulac.data.POCGraph;
import org.thunlp.thulac.util.StringUtils;

import static org.thunlp.thulac.util.CodePointUtils.SPECIAL_CHARS;
import static org.thunlp.thulac.util.CodePointUtils.WHITESPACE_CHARS;

/**
 * A preprocess pass which cleans raw input up.
 */
public class PreprocessPass implements IPreprocessPass {
	// TODO: add more documentation

	private static final String SINGLE_PUNCTUATION_CODE_POINTS = StringUtils.toString(
			65292, 12290, 65311, 65281, 65306, 65307, 8216, 8217, 8220, 8221, 1230, 12304,
			12305, 12289, 12298, 12299, 64, 35, 65288, 65289, 34, 91, 93, 126, 47, 44, 58,
			63, 9700, 9734, 9733, 8230, 39, 33, 42, 43, 62, 40, 41, 59, 61);

	private boolean isSinglePunctuation(int c) {
		return SINGLE_PUNCTUATION_CODE_POINTS.indexOf(c) != -1;
	}

	private String cleanup(String sentence, POCGraph graph) {
		StringBuilder cleaned = new StringBuilder();
		graph.clear();
		boolean spaceFlag = false, otherFlag = false,
				singlePunctuationFlag = false, titleFlag = false;

		int titleStart = 0;
		int[] codePoints = StringUtils.toCodePoints(sentence);
		for (int c : codePoints) {
			if (WHITESPACE_CHARS.indexOf(c) != -1) {
				otherFlag = false;
				if (spaceFlag) continue;
				if (!graph.isEmpty())
					graph.setElementAt(graph.lastElement() & 12, graph.size() - 1);
				spaceFlag = true;
				continue;
			}

			cleaned.appendCodePoint(c);
			if (SPECIAL_CHARS.indexOf(c) != -1) {
				if (spaceFlag) {
					singlePunctuationFlag = this.isSinglePunctuation(c);
					graph.add(singlePunctuationFlag ? 8 : 9);
					spaceFlag = false;
				} else {
					if (otherFlag) {
						if (this.isSinglePunctuation(c)) {
							if (!graph.isEmpty()) graph.setElementAt(
									graph.lastElement() & 12, graph.size() - 1);
							graph.add(8);
						} else if (singlePunctuationFlag) graph.add(9);
						else {
							if (!graph.isEmpty() && graph.lastElement() == 0)
								graph.setElementAt(7, graph.size() - 1);
							graph.add(2);
						}
					} else graph.add(9);
					singlePunctuationFlag = this.isSinglePunctuation(c);
				}
				otherFlag = true;

				if (c == 12298) titleStart = graph.size();
				else if (c == 12299 && titleFlag) {
					int titleEnd = graph.size() - 2;
					if (titleEnd <= titleStart + 9)
						if (titleStart == titleEnd) graph.setElementAt(9, titleStart);
						else {
							graph.setElementAt(1, titleStart);
							for (int i = titleStart + 1; i < titleEnd; ++i)
								graph.setElementAt(2, i);
							graph.setElementAt(4, titleEnd);
						}
				}
				titleFlag = c == 12298;
			} else {
				if (spaceFlag) graph.add(9);
				else if (otherFlag) {
					graph.setElementAt(graph.lastElement() & 12, graph.size() - 1);
					graph.add(9);
					singlePunctuationFlag = false;
				} else graph.add(15);
				spaceFlag = false;
				otherFlag = false;
			}
		}

		// deal with first & last character
		if (!graph.isEmpty()) {
			int first = graph.firstElement() & 9, last = graph.lastElement() & 12;
			graph.setElementAt(first == 0 ? 9 : first, 0);
			graph.setElementAt(last == 0 ? 12 : last, graph.size() - 1);
		}

		return cleaned.toString();
	}

	@Override
	public String process(String raw, POCGraph graph) {
		return this.cleanup(raw, graph);
	}
}
