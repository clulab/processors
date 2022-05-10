package org.thunlp.thulac.util;

import java.util.ArrayList;
import java.util.List;

/**
 * An utility class providing definitions for many sets of code points.
 */
public class CodePointUtils {
	/**
	 * ASCII and full-width digits.
	 */
	public static final String DIGITS =
			generate(range('0', '9'), range('\uFF10', '\uFF19'));

	/**
	 * Chinese digits.
	 */
	public static final String CHINESE_DIGITS = generate('\u3007', '\u4E00', '\u4E8C',
			'\u4E09', '\u56DB', '\u4E94', '\u516D', '\u4E03', '\u516B', '\u4E5D');

	/**
	 * Special characters, containing:<br>
	 * <ul>
	 * <li><b>Chinese full-width punctuations</b>:<br>
	 * U+FF0C: Comma, U+3002: Full Stop, U+FF1F: Question Mark, U+FF01: Exclamation
	 * Mark, U+FF1A: Colon, U+FF1B: Semicolon, U+3010 & U+3011: Brackets, U+3001:
	 * Ideographic Comma, U+300A & U+300B: Guillemets, U+FF08 & U+FF09: Parentheses.
	 * </li>
	 * <li><b>Standard punctuations</b>:<br>
	 * U+2018 & U+2019: Single Quotation Marks,U+201C & U+201D: Double Quotation
	 * Marks, U+00B7: Middle Point, U+2026: Horizontal Ellipsis, U+2014: Em Dash.
	 * </li>
	 * <li><b>Special characters</b>:
	 * U+FFE5: Full-width Yen Sign, U+25E4: Black Upper Left Triangle, U+2605: Black
	 * Star, U+2606: White Star.
	 * </li>
	 * <li><b>ASCII characters</b>: All printable ASCII characters (from U+0021 to
	 * U+007E) except for U+0060: Grave Accent.</li>
	 * </ul>
	 * (All of above character names are referred from the Unicode Consortium.)
	 */
	public static final String SPECIAL_CHARS = generate('\uFF0C', '\u3002', '\uFF1F',
			'\uFF01', '\uFF1A', '\uFF1B', '\u3010', '\u3011', '\u3001', '\u300A',
			'\u300B', '\uFF08', '\uFF09', '\u2018', '\u2019', '\u201C', '\u201D',
			'\u00B7', '\u2026', '\u2014', '\uFFE5', '\u25E4', '\u2605', '\u2606',
			range('\u0021', '\u005F'), range('\u0061', '\u007E'));

	/**
	 * Whitespaces: U+0020 & U+3000.
	 */
	public static final String WHITESPACE_CHARS = generate('\u0020', '\u3000');

	/**
	 * Generate a {@link String} containing a list of code points produced following
	 * these steps:<br>
	 * <ol>
	 * <li>Let {@code list} be the empty list of integers.</li>
	 * <li>For each {@link Object} {@code param} in {@code params}, sequentially from
	 * {@code params[0]} to {@code params[params.length - 1]}, switch on {@code
	 * param}'s class:<br>
	 * <ul>
	 * <li><b>{@link Integer}</b>: Append {@code param} to {@code list}.</li>
	 * <li><b>{@code int[]}</b>: Append every integer in {@code param} to {@code
	 * list}.</li>
	 * <li><b>{@link Character}</b>: Append {@code param}, converted to {@code char}
	 * and then to {@code int} and then to {@link Integer}, to {@code list}.</li>
	 * <li><b>{@link String}</b>: Append every code point in the content of {@code
	 * param} retrieved using {@link StringUtils#toCodePoints(String)} to {@code
	 * list}.</li>
	 * <li><b>Other</b>: Do nothing.</li>
	 * </ul>
	 * </li>
	 * <li>Convert {@code list} to {@link String} using {@link StringUtils#toString(int...)}</li>
	 * </ol>
	 *
	 * @param params
	 * 		The input parameters.
	 *
	 * @return The generated {@link String}.
	 */
	public static String generate(Object... params) {
		List<Integer> codePoints = new ArrayList<>();
		for (Object param : params)
			if (param instanceof Integer) codePoints.add((Integer) param);
			else if (param instanceof int[]) for (int codePoint : (int[]) param)
				codePoints.add(codePoint);
			else if (param instanceof String)
				for (int codePoint : StringUtils.toCodePoints((String) param))
					codePoints.add(codePoint);
			else if (param instanceof Character) codePoints.add((int) (Character) param);

		int[] cps = new int[codePoints.size()];
		for (int i = 0, size = codePoints.size(); i < size; ++i)
			cps[i] = codePoints.get(i);

		return StringUtils.toString(cps);
	}

	/**
	 * Return an {@code int[]} containing code points ranging from start to end
	 * (inclusive);
	 */
	public static int[] range(int start, int end) {
		if (end < start) return null;
		int[] range = new int[end - start + 1];
		for (int i = start; i <= end; ++i) range[i - start] = i;
		return range;
	}
}
