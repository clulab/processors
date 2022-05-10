package org.thunlp.thulac.data;

import java.util.Vector;

/**
 * <i>POC</i> means <i>Position Of Character</i>, representing the possible positions
 * of a character in the segmented words.<br>
 * {@code POCGraph} is a list of integers, possesses a length of {@code l} when generated
 * by processing a string of length {@code l}, therefore we get:<br>
 * Let {@code graph} be an instance of {@code POCGraph}, and {@code l} be the length of
 * the graph. (retrieved by calling {@code graph.size()})<br>
 * {@code graph.get(i)} ({@code 0 <= i < length}) is an integer calculated by bitwise
 * or-ing zero or more of the following constants:<br>
 * <ul>
 * <li>POC_B = 0x01: included if the character can be the beginning of a word.</li>
 * <li>POC_M = 0x02: included if the character can be the middle of a word.</li>
 * <li>POC_E = 0x04: included if the character can be the end of a word.</li>
 * <li>POC_S = 0x08: included if the character can be exactly one single world.</li>
 * </ul>
 * As pseudo-code:<br>
 * <code><pre>
 * int i = &lt;index&gt;;
 * boolean canBeBeginning = input.canBeBeginning(i);
 * boolean canBeMiddle    = input.canBeMiddle(i);
 * boolean canBeEnd       = input.canBeEnd(i);
 * boolean canBeSingle    = input.canBeSingle(i);
 * int positions = (canBeBeginning ? POC_B : 0) |
 *                 (canBeMiddle    ? POC_M : 0) |
 *                 (canBeEnd       ? POC_E : 0) |
 *                 (canBeSingle    ? POC_S : 0);
 * graph[i] = positions;
 * </pre></code>
 * Note that the {@code POC_M} flag does not conflict with the other flags, e.g., a
 * {@code position} of {@code POC_M | POC_B} means that the character can either be the
 * middle or the beginning of a word. This applies also for {@code POC_S}, which
 * indicates that the character can form a single-character word.<br>
 * The generation of {@code POCGraph} is mainly based on punctuations and line breaks,
 * but in various implementations also on characters that would certainly not be a part
 * of a word, such as whitespaces or numbers.<br>
 * This class is merely a alias for {@linkplain Vector Vector&lt;Integer&gt;},
 * indicating that instances of this class are used as only as the list of {@code POCs},
 * no more behaviour is added.
 */
public class POCGraph extends Vector<Integer> {
}
