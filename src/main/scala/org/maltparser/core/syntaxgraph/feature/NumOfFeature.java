package org.maltparser.core.syntaxgraph.feature;

import java.util.LinkedHashMap;
import java.util.Map;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.function.AddressFunction;
import org.maltparser.core.feature.function.FeatureFunction;
import org.maltparser.core.feature.value.AddressValue;
import org.maltparser.core.feature.value.FeatureValue;
import org.maltparser.core.feature.value.SingleFeatureValue;
import org.maltparser.core.io.dataformat.ColumnDescription;
import org.maltparser.core.symbol.SymbolTable;
import org.maltparser.core.symbol.SymbolTableHandler;
import org.maltparser.core.symbol.nullvalue.NullValues.NullValueId;
import org.maltparser.core.syntaxgraph.SyntaxGraphException;
import org.maltparser.core.syntaxgraph.node.DependencyNode;

public class NumOfFeature implements FeatureFunction {
	public enum NumOfRelation {
		LDEPS, RDEPS, DEPS
	};
	protected AddressFunction addressFunction;
	protected SymbolTableHandler tableHandler;
	protected SymbolTable table;
	protected SingleFeatureValue featureValue;
	protected NumOfRelation numOfRelation;
	protected String numOfRelationName;
	protected String normalizationString;
	protected Map<Integer,String> normalization;
	
	public NumOfFeature(SymbolTableHandler tableHandler) throws MaltChainedException {
		super();
		featureValue = new SingleFeatureValue(this);
		setTableHandler(tableHandler);
		normalization = new LinkedHashMap<Integer,String>();
	}
	
	/**
	 * Initialize the distance feature function
	 * 
	 * @param arguments an array of arguments with the type returned by getParameterTypes()
	 * @throws MaltChainedException
	 */
	public void initialize(Object[] arguments) throws MaltChainedException {
		if (arguments.length != 3) {
			throw new SyntaxGraphException("Could not initialize NumOfFeature: number of arguments are not correct. ");
		}
		// Checks that the two arguments are address functions
		if (!(arguments[0] instanceof AddressFunction)) {
			throw new SyntaxGraphException("Could not initialize NumOfFeature: the first argument is not an address function. ");
		}
		if (!(arguments[1] instanceof java.lang.String)) {
			throw new SyntaxGraphException("Could not initialize NumOfFeature: the second argument (relation) is not a string. ");
		}
		if (!(arguments[2] instanceof java.lang.String)) {
			throw new SyntaxGraphException("Could not initialize NumOfFeature: the third argument (normalization) is not a string. ");
		}
		setAddressFunction((AddressFunction)arguments[0]);
		setNumOfRelation((String)arguments[1]);
		normalizationString = (String)arguments[2];
		// Creates a symbol table called "NUMOF" using one null value
		setSymbolTable(tableHandler.addSymbolTable("NUMOF"+normalizationString, ColumnDescription.INPUT, "one"));
		
		String[] items  = normalizationString.split("\\|");
		
		if (items.length <= 0 || !items[0].equals("0")) {
			throw new SyntaxGraphException("Could not initialize NumOfFeature ("+this+"): the third argument (normalization) must contain a list of integer values separated with | and the first element must be 0.");
		}
		int tmp = -1;
		for (int i = 0; i < items.length; i++) {
			int v;
			try {
				v = Integer.parseInt(items[i]);
			} catch (NumberFormatException e) {
				throw new SyntaxGraphException("Could not initialize NumOfFeature ("+this+"): the third argument (normalization) must contain a sorted list of integer values separated with |", e);
			}
			normalization.put(v, ">="+v);
			table.addSymbol(">="+v);
			if (tmp != -1 && tmp >= v) {
				throw new SyntaxGraphException("Could not initialize NumOfFeature ("+this+"): the third argument (normalization) must contain a sorted list of integer values separated with |");
			}
			tmp = v;
		}
	}
	
	/**
	 * Returns an array of class types used by the feature extraction system to invoke initialize with
	 * correct arguments.
	 * 
	 * @return an array of class types
	 */
	public Class<?>[] getParameterTypes() {
		Class<?>[] paramTypes = { org.maltparser.core.feature.function.AddressFunction.class,
								  java.lang.String.class,
								  java.lang.String.class};
		return paramTypes; 
	}
	
	/**
	 * Returns the string representation of the integer <code>code</code> according to the numof feature function. 
	 * 
	 * @param code the integer representation of the symbol
	 * @return the string representation of the integer <code>code</code> according to the numof feature function.
	 * @throws MaltChainedException
	 */
	public String getSymbol(int code) throws MaltChainedException {
		return table.getSymbolCodeToString(code);
	}
	
	/**
	 * Returns the integer representation of the string <code>symbol</code> according to the numof feature function.
	 * 
	 * @param symbol the string representation of the symbol
	 * @return the integer representation of the string <code>symbol</code> according to the numof feature function.
	 * @throws MaltChainedException
	 */
	public int getCode(String symbol) throws MaltChainedException {
		return table.getSymbolStringToCode(symbol);
	}
	
	/**
	 * Cause the numof feature function to update the cardinality of the feature value.
	 * 
	 * @throws MaltChainedException
	 */
	public void updateCardinality() {
//		featureValue.setCardinality(table.getValueCounter()); 
	}
	
	/**
	 * Cause the feature function to update the feature value.
	 * 
	 * @throws MaltChainedException
	 */
	public void update() throws MaltChainedException {
		// Retrieve the address value 
		final AddressValue arg1 = addressFunction.getAddressValue();
		// if arg1 or arg2 is null, then set a NO_NODE null value as feature value
		if (arg1.getAddress() == null ) { 
			featureValue.setIndexCode(table.getNullValueCode(NullValueId.NO_NODE));
			featureValue.setSymbol(table.getNullValueSymbol(NullValueId.NO_NODE));
			featureValue.setNullValue(true);			
		} else {
			// Unfortunately this method takes a lot of time  arg1.getAddressClass().asSubclass(org.maltparser.core.syntaxgraph.node.DependencyNode.class);
			// Cast the address arguments to dependency nodes
			final DependencyNode node = (DependencyNode)arg1.getAddress();
			int numof = 0;
			if (numOfRelation == NumOfRelation.DEPS) {
				numof = node.getLeftDependentCount() +  node.getRightDependentCount();
			} else if (numOfRelation == NumOfRelation.LDEPS) {
				numof = node.getLeftDependentCount();
			} else if (numOfRelation == NumOfRelation.RDEPS) {
				numof = node.getRightDependentCount();
			} 
			int lower = -1;
			boolean f = false;
			for (Integer upper : normalization.keySet()) {
				if (numof >= lower && numof < upper) {
					featureValue.setIndexCode(table.getSymbolStringToCode(normalization.get(lower)));
					featureValue.setSymbol(normalization.get(lower));
					f = true;
					break;
				}
				lower = upper;
			}
			if (f == false) {
				featureValue.setIndexCode(table.getSymbolStringToCode(normalization.get(lower)));
				featureValue.setSymbol(normalization.get(lower));
			}
			// Tells the feature value that the feature is known and is not a null value
			featureValue.setNullValue(false);
		}
		featureValue.setValue(1);
//		featureValue.setKnown(true);
	}
	
	public void setNumOfRelation(String numOfRelationName) {
		this.numOfRelationName = numOfRelationName;
		numOfRelation = NumOfRelation.valueOf(numOfRelationName.toUpperCase());
	}
	
	public NumOfRelation getNumOfRelation() {
		return numOfRelation;
	}
	
	/**
	 * Returns the feature value
	 * 
	 * @return the feature value
	 */
	public FeatureValue getFeatureValue() {
		return featureValue;
	}
	
	/**
	 * Returns the symbol table used by the numof feature function
	 * 
	 * @return the symbol table used by the numof feature function
	 */
	public SymbolTable getSymbolTable() {
		return table;
	}
	
	/**
	 * Returns the address function 
	 * 
	 * @return the address function 
	 */
	public AddressFunction getAddressFunction() {
		return addressFunction;
	}


	/**
	 * Sets the address function 
	 * 
	 * @param addressFunction a address function 
	 */
	public void setAddressFunction(AddressFunction addressFunction) {
		this.addressFunction = addressFunction;
	}
	
	/**
	 * Sets the symbol table handler
	 * 
	 * @param tableHandler a symbol table handler
	 */
	public void setTableHandler(SymbolTableHandler tableHandler) {
		this.tableHandler = tableHandler;
	}

	/**
	 * Sets the symbol table used by the numof feature function
	 * 
	 * @param table
	 */
	public void setSymbolTable(SymbolTable table) {
		this.table = table;
	}
	
	public  int getType() {
		return ColumnDescription.STRING;
	}
	
	public String getMapIdentifier() {
		return getSymbolTable().getName();
	}
	
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		return obj.toString().equals(this.toString());
	}
	
	public int hashCode() {
		return 217 + (null == toString() ? 0 : toString().hashCode());
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append("NumOf(");
		sb.append(addressFunction.toString());
		sb.append(", ");
		sb.append(numOfRelationName);
		sb.append(", ");
		sb.append(normalizationString);
		sb.append(')');
		return sb.toString();
	}
}
