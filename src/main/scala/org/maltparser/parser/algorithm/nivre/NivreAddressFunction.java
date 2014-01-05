package org.maltparser.parser.algorithm.nivre;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.function.AddressFunction;
import org.maltparser.core.feature.value.AddressValue;
import org.maltparser.parser.Algorithm;
import org.maltparser.parser.ParsingException;

/**
*
* @author Johan Hall
**/
public class NivreAddressFunction extends AddressFunction {
	public enum NivreSubFunction {
		STACK, INPUT
	};
	private String subFunctionName;
	private NivreSubFunction subFunction;
	private Algorithm parsingAlgorithm;
	private int index;
	
	public NivreAddressFunction(String subFunctionName, Algorithm parsingAlgorithm) {
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
		update((NivreConfig)parsingAlgorithm.getCurrentParserConfiguration());
	}
	
	public void update(Object[] arguments) throws MaltChainedException {
//		if (arguments.length != 1 || !(arguments[0] instanceof NivreConfig)) {
//			throw new ParsingException("Arguments to the Nivre address function is not correct. ");
//		}
//		update((NivreConfig)arguments[0]);
		if (subFunction == NivreSubFunction.STACK) {
			address.setAddress(((NivreConfig)arguments[0]).getStackNode(index));
		} else if (subFunction == NivreSubFunction.INPUT) {
			address.setAddress(((NivreConfig)arguments[0]).getInputNode(index));
		} else {
			address.setAddress(null);
		}
	}
	
	private void update(NivreConfig config) throws MaltChainedException {
		if (subFunction == NivreSubFunction.STACK) {
			address.setAddress(config.getStackNode(index));
		} else if (subFunction == NivreSubFunction.INPUT) {
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
		subFunction = NivreSubFunction.valueOf(subFunctionName.toUpperCase());
	}
	
	public NivreSubFunction getSubFunction() {
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
		
		NivreAddressFunction other = (NivreAddressFunction) obj;
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
		final StringBuilder sb = new StringBuilder();
		sb.append(subFunctionName);
		sb.append('[');
		sb.append(index);
		sb.append(']');
		return sb.toString();
	}
}
