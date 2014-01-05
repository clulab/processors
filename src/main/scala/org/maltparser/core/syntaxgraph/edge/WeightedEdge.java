package org.maltparser.core.syntaxgraph.edge;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.syntaxgraph.Weightable;
import org.maltparser.core.syntaxgraph.node.Node;
/**
*
*
* @author Johan Hall
*/
public class WeightedEdge extends GraphEdge implements Weightable {
	private Double weight = Double.NaN;
	
	public WeightedEdge() { }
	
	public WeightedEdge(Node source, Node target, int type) throws MaltChainedException {
		super(source, target, type);
	}

	public WeightedEdge(Node source, Node target, int type, Double weight) throws MaltChainedException {
		super(source, target, type);
		setWeight(weight);
	}
	
	public void clear() throws MaltChainedException {
		super.clear();
		weight = Double.NaN;
	}
	
	public double getWeight() {
		return weight.doubleValue();
	}

	public void setWeight(double weight) {
		this.weight = weight;
	}
	
	public int compareTo(WeightedEdge that) {
	    if (this == that) return 0;
	    int comparison = this.weight.compareTo(that.getWeight());
	    if ( comparison != 0 ) return comparison;
	    
	    return super.compareTo(that);
	}
	
	public boolean equals(Object obj) {
		WeightedEdge e = (WeightedEdge)obj;
		return weight.equals(e.getWeight()) && super.equals(obj); 
	}
	
	public int hashCode() {
		int hash = 7;
		hash = 31 * hash + (null == weight ? 0 : weight.hashCode());
		return 31 * hash + super.hashCode();
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(getWeight());
		sb.append(' ');
		sb.append(getSource().getIndex());
		sb.append("->");
		sb.append(getTarget().getIndex());
		sb.append(' ');
		sb.append(super.toString());
		return sb.toString();
	}
}
