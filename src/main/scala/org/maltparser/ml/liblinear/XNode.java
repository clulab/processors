package org.maltparser.ml.liblinear;

public class XNode implements Comparable<XNode> {
	private int index;
	private double value;
	
	public XNode(int index, double value) {
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

	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + index;
		long temp;
		temp = Double.doubleToLongBits(value);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		return result;
	}

	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		XNode other = (XNode) obj;
		if (index != other.index)
			return false;
		if (Double.doubleToLongBits(value) != Double
				.doubleToLongBits(other.value))
			return false;
		return true;
	}
	
	public int compareTo(XNode aThat) {
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
		return "XNode [index=" + index + ", value=" + value + "]";
	}
}
