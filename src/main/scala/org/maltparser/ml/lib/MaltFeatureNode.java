package org.maltparser.ml.lib;


public class MaltFeatureNode implements Comparable<MaltFeatureNode> {
	int index;
	double value;
	
	public MaltFeatureNode() {
		index = -1;
		value = 0;
	}
	
	public MaltFeatureNode(int index, double value) {
		setIndex(index);
		setValue(value);
	}

	public int getIndex() {
		return index;
	}

	public void setIndex(int index) {
		this.index = index;
	}

	public double getValue() {
		return value;
	}

	public void setValue(double value) {
		this.value = value;
	}

	public void clear() {
		index = -1;
		value = 0;
	}
	
	public int hashCode() {
		final int prime = 31;
		final long temp = Double.doubleToLongBits(value);
		return prime * (prime  + index) + (int) (temp ^ (temp >>> 32));
	}

	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		MaltFeatureNode other = (MaltFeatureNode) obj;
		if (index != other.index)
			return false;
		if (Double.doubleToLongBits(value) != Double.doubleToLongBits(other.value))
			return false;
		return true;
	}
	
	public int compareTo(MaltFeatureNode aThat) {
		final int BEFORE = -1;
		final int EQUAL = 0;
		final int AFTER = 1;

		if (this == aThat)
			return EQUAL;

		if (this.index < aThat.index)
			return BEFORE;
		if (this.index > aThat.index)
			return AFTER;

		if (this.value < aThat.value)
			return BEFORE;
		if (this.value > aThat.value)
			return AFTER;

		return EQUAL;
	}

	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append("MaltFeatureNode [index=");
		sb.append(index);
		sb.append(", value=");
		sb.append(value);
		sb.append("]");
		return sb.toString();
	}
}
