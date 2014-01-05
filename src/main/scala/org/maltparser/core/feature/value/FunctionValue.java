package org.maltparser.core.feature.value;

import org.maltparser.core.feature.function.Function;
/**
 *  
 *
 * @author Johan Hall
 * @since 1.0
**/
public abstract class FunctionValue {
	protected Function function;
	
	public FunctionValue(Function function) {
		setFunction(function);
	}

	public Function getFunction() {
		return function;
	}

	public void setFunction(Function function) {
		this.function = function;
	}
	
	public abstract void reset();
	
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		return function.equals(((FunctionValue)obj).function);
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(function.toString());
		sb.append(':');
		return sb.toString();
	}
}
