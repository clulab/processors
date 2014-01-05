package org.maltparser.ml.lib;

import java.util.ArrayList;


/**
 * The feature list is sorted according to the compareTo of the node.
 * 
 * @author Johan Hall
 *
 */
public class FeatureList  {
	private static final long serialVersionUID = 7526471155622776147L;
	private final ArrayList<MaltFeatureNode> list;
	
	/**
	 * Creates a feature list of MaltFeatureNode objects
	 */
	public FeatureList() {
		list = new ArrayList<MaltFeatureNode>();
	}
	
	/**
	 * Creates a feature list of MaltFeatureNode objects
	 */
	public FeatureList(int size) {
		list = new ArrayList<MaltFeatureNode>(size);
	}
	
	/**
	 * Adds a MaltFeatureNode object to the feature list. The object will be added in the sorted feature list based on 
	 * the compareTo() in MaltFeatureNode.
	 * 
	 * @param x a MaltFeatureNode object
	 */
	public void add(MaltFeatureNode x) {
		if (list.size() == 0) {
			list.add(x);
		} else {
	        int low = 0;
	        int high = list.size() - 1;
	        int mid;
	        MaltFeatureNode y;
	        while (low <= high ) {
	            mid = (low + high) / 2;
	            y = list.get(mid); 
	            if (y.compareTo(x) < 0 ) {
	                low = mid + 1;
	            } else if (y.compareTo(x) > 0 ) {
	                high = mid - 1;
	            } else {
	                break;
	            }
	        }
	        list.add(low,x);
		}
	}
	
	/**
	 * Adds an index/value pair to the feature list.
	 * 
	 * @param index a binarized feature index
	 * @param value a value 
	 */
	public void add(int index, double value) {
		add(new MaltFeatureNode(index,value));
	}
	
	/**
	 * @param i the position in the feature list
	 * @return a MaltFeatureNode object located on the position <i>i</i>
	 */
	public MaltFeatureNode get(int i) {
		if (i < 0 || i >= list.size()) {
			return null;
		}
		return list.get(i);
	}
	
	/**
	 * Clears the feature list
	 */
	public void clear() {
		list.clear();
	}
	
	/**
	 * @return the size of the feature list
	 */
	public int size() {
		return list.size();
	}
	
	public MaltFeatureNode[] toArray() {
		final MaltFeatureNode[] nodes = new MaltFeatureNode[list.size()];
		final int len = nodes.length;
		for (int i = 0; i < len; i++) {
			nodes[i] = list.get(i);
		}
		return nodes;
	}
}
