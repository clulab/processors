package org.thunlp.thulac.cb;

/**
 * A class which contains topological information of a node.
 */
public class Node {
	// TODO: add more documentation

	/**
	 * Value:<br>
	 * <ul>
	 * <li>1: If this {@link Node} is a starting node.</li>
	 * <li>2: If this {@link Node} is a ending node.</li>
	 * <li>0: otherwise</li>
	 * </ul>
	 */
	public int type;

	public int[] predecessors; // last element should be -1
	public int[] successors; // last element should be -1
}
