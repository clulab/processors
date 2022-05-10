package org.thunlp.thulac.util;

import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.assertNotEquals;

/**
 *
 */
public class CodePointUtilsTest {
	// the original one
	private static final String OTHER_CODE_POINTS =
			StringUtils.toString(65292, 12290, 65311, 65281, 65306, 65307, 8216, 8217,
					8220, 8221, 12304, 12305, 12289, 12298, 12299, 126, 183, 64, 124, 35,
					65509, 37, 8230, 38, 42, 65288, 65289, 8212, 45, 43, 61, 44, 46, 60,
					62, 63, 47, 33, 59, 58, 39, 34, 123, 125, 91, 93, 92, 124, 35, 36, 37,
					94, 38, 42, 40, 41, 95, 45, 43, 61, 9700, 9734, 9733, 65, 66, 67,
					68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84,
					85, 86, 87, 88, 89, 90, 97, 98, 99, 100, 101, 102, 103, 104, 105,
					106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
					120, 121, 122, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57);

	@Test
	public void test() {
		// equality test
		Arrays.stream(StringUtils.toCodePoints(OTHER_CODE_POINTS))
				.forEach(ch -> assertNotEquals(String.valueOf(ch),
						-1, CodePointUtils.SPECIAL_CHARS.indexOf(ch)));
		Arrays.stream(StringUtils.toCodePoints(CodePointUtils.SPECIAL_CHARS))
				.forEach(ch -> assertNotEquals(String.valueOf(ch),
						-1, OTHER_CODE_POINTS.indexOf(ch)));
	}
}
