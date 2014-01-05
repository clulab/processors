package org.maltparser.parser.algorithm.planar;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.function.AddressFunction;
import org.maltparser.core.feature.value.AddressValue;
import org.maltparser.parser.Algorithm;
import org.maltparser.parser.ParsingException;

/**
*
* @author Carlos Gomez Rodriguez
**/
public class PlanarAddressFunction extends AddressFunction {
	public enum PlanarSubFunction {
		STACK, INPUT
	};
	private String subFunctionName;
	private PlanarSubFunction subFunction;
	private Algorithm parsingAlgorithm;
	private int index;
	
	public PlanarAddressFunction(String subFunctionName, Algorithm parsingAlgorithm) {
		super();
		setSubFunctionName(subFunctionName);
		setAlgorithm(parsingAlgorithm);
	}
	
	public void initialize(Object[] arguments) throws MaltChainedException {
		if (arguments.length != 1) {
			throw new ParsingException("Could not initialize "+this.getClass().getName()+": number of arguments are not correct. ");
		}
		if (!(arguments[0] instanceof Integer)) {
			throw new ParsingException("Could not initialize "+this.getClass().getName()+": the first argument is not an integer. ");
		}
		
		setIndex(((Integer)arguments[0]).intValue());
	}
	
	public Class<?>[] getParameterTypes() {
		Class<?>[] paramTypes = { java.lang.Integer.class };
		return paramTypes; 
	}
	
	public void update() throws MaltChainedException {
		update((PlanarConfig)parsingAlgorithm.getCurrentParserConfiguration());
	}
	
	public void update(Object[] arguments) throws MaltChainedException {
		if (arguments.length != 1 || !(arguments[0] instanceof PlanarConfig)) {
			throw new ParsingException("Arguments to the planar address function is not correct. ");
		}
		update((PlanarConfig)arguments[0]);
	}
	
	private void update(PlanarConfig config) throws MaltChainedException {
		if (subFunction == PlanarSubFunction.STACK) {
			address.setAddress(config.getStackNode(index));
		} else if (subFunction == PlanarSubFunction.INPUT) {
			address.setAddress(config.getInputNode(index));
		} else {
			address.setAddress(null);
		}
	}
	
	public String getSubFunctionName() {
		return subFunctionName;
	}

	public void setSubFunctionName(String subFunctionName) {
		this.subFunctionName = subFunctionName;
		subFunction = PlanarSubFunction.valueOf(subFunctionName.toUpperCase());
	}
	
	public PlanarSubFunction getSubFunction() {
		return subFunction;
	}
	
	public AddressValue getAddressValue() {
		return address;
	}
	
	public Algorithm getParsingAlgorithm() {
		return parsingAlgorithm;
	}

	public void setAlgorithm(Algorithm parsingAlgorithm) {
		this.parsingAlgorithm = parsingAlgorithm;
	}

	public int getIndex() {
		return index;
	}

	public void setIndex(int index) {
		this.index = index;
	}
	
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		
		PlanarAddressFunction other = (PlanarAddressFunction) obj;
		if (index != other.index)
			return false;
		if (parsingAlgorithm == null) {
			if (other.parsingAlgorithm != null)
				return false;
		} else if (!parsingAlgorithm.equals(other.parsingAlgorithm))
			return false;
		if (subFunction == null) {
			if (other.subFunction != null)
				return false;
		} else if (!subFunction.equals(other.subFunction))
			return false;
		return true;
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(subFunctionName);
		sb.append('[');
		sb.append(index);
		sb.append(']');
		return sb.toString();
	}
}
