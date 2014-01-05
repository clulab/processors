package org.maltparser.core.feature.value;

import org.maltparser.core.feature.function.Function;

/**
 *  
 *
 * @author Johan Hall
 * @since 1.0
**/
public abstract class FeatureValue extends FunctionValue {
	protected boolean nullValue;
	
	public FeatureValue(Function function) {
		super(function);
		setNullValue(true);
	}
	
	public void reset() {
		setNullValue(true);
	}

	public boolean isNullValue() {
		return nullValue;
	}

	public void setNullValue(boolean nullValue) {
		this.nullValue = nullValue;
	}
	
	public abstract boolean isMultiple();
	
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		return super.equals(obj);
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(super.toString());
		sb.append("[null=");
		sb.append(nullValue);
		sb.append("]");
		return sb.toString();
	}
}