package org.thunlp.thulac.util;

/**
 * An utility class which deals with string, converting array of code points to and from
 * strings.
 */
public class StringUtils {
	/**
	 * Convert an array of code points to {@link String}.
	 *
	 * @param codePoints
	 * 		The code points to convert.
	 *
	 * @return The converted {@link String}.
	 */
	public static String toString(int... codePoints) {
		return toString(codePoints, 0, codePoints.length);
	}

	/**
	 * Convert an array of code points to {@link String}.
	 *
	 * @param codePoints
	 * 		The code points to convert.
	 * @param offset
	 * 		The starting offset of {@code codePoints}.
	 * @param len
	 * 		The number of code points to convert.
	 *
	 * @return The converted {@link String}, indices which exceeds {@code
	 * codePoints.length} are discarded.
	 */
	public static String toString(int[] codePoints, int offset, int len) {
		StringBuilder sb = new StringBuilder();
		for (int i = offset, max = Math.min(codePoints.length, offset + len);
			 i < max; ++i)
			sb.appendCodePoint(codePoints[i]);
		return sb.toString();
	}

	/**
	 * Convert a {@link String} to an array of code points.<br>
	 * Internally, data in {@link String} is stored in {@code char[]}, however for
	 * Unicode code points greater than U+FFFF, one {@code char} (that is, two bytes)
	 * is not enough. Therefore, Java uses <i>surrogates</i> to divide code points
	 * that cannot be represented by one {@code} into two. The problem is,
	 * {@link String#length()} return the length of its internal {@code char[]}, while
	 * the return value of {@link String#length()} is not necessarily (though in most
	 * cases) equal to the number of code points stored in the {@link String}.<br>
	 * To solve this problem, the {@link String} class provides a set of methods to
	 * retrieve the actual number of code points stored and to access a code points in
	 * the {@link String} using the index by code points, as implemented in this method.
	 * However, the iteration through a {@link String} by the actual code points is
	 * fairly complicated, and it is much easier for applications to achieve this if
	 * the string data is stored as {@code int[]}, each element representing a code point.
	 * And this is exactly What this method does: take a {@link String} as input,
	 * convert it into a {@code int[]} which contains exactly the same data as the
	 * {@link String}.<br>
	 * It is recommended that all applications which iterate through the characters
	 * stored in a {@link String} use<br>
	 * <pre><code>
	 * int[] codePoints = StringUtils.toCodePoints(str);
	 * for (int codePoint: codePoints) // do something ...
	 * </code></pre>
	 * instead of the traditional<br>
	 * <pre><code>
	 * for (int i = 0, length = str.length(); i < length; ++i) {
	 *     char c = str.charAt(i);
	 *     // do something ...
	 * }
	 * </code></pre>
	 *
	 * @param str
	 * 		The {@link String} to convert.
	 *
	 * @return The converted array of code points.
	 */
	public static int[] toCodePoints(String str) {
		if (str == null) return null;
		int codePointCount = str.codePointCount(0, str.length());
		int[] codePoints = new int[codePointCount];
		for (int i = 0; i < codePointCount; ++i)
			codePoints[i] = str.codePointAt(str.offsetByCodePoints(0, i));
		return codePoints;
	}

	/**
	 * Return the number of code points in the given {@link String}.
	 *
	 * @param str
	 * 		The given {@link String}.
	 *
	 * @return The number of code points in {@code str}.
	 */
	public static int codePointCount(String str) {
		return str.codePointCount(0, str.length());
	}

	/**
	 * Return code point {@code index}-ith code point in the given {@link String}.
	 *
	 * @param str
	 * 		The given {@link String}.
	 * @param index
	 * 		The index of the code point to return.
	 *
	 * @return The cde point at {@code index}.
	 *
	 * @throws IndexOutOfBoundsException
	 * 		If index is negative or greater than or equal to the number of code points
	 * 		of {@code str}.
	 */
	public static int codePointAt(String str, int index) {
		int codePointIndex = str.offsetByCodePoints(0, index);
		return str.codePointAt(codePointIndex);
	}
}
