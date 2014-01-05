package org.maltparser.core.syntaxgraph.feature;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.function.AddressFunction;
import org.maltparser.core.feature.function.FeatureFunction;
import org.maltparser.core.feature.value.FeatureValue;
import org.maltparser.core.feature.value.SingleFeatureValue;
import org.maltparser.core.io.dataformat.ColumnDescription;
import org.maltparser.core.symbol.SymbolTable;
import org.maltparser.core.symbol.SymbolTableHandler;
import org.maltparser.core.syntaxgraph.SyntaxGraphException;

public class ExistsFeature implements FeatureFunction {
	protected AddressFunction addressFunction;
	protected SymbolTableHandler tableHandler;
	protected SymbolTable table;
	protected SingleFeatureValue featureValue;
	
	public ExistsFeature(SymbolTableHandler tableHandler) throws MaltChainedException {
		super();
		featureValue = new SingleFeatureValue(this);
		setTableHandler(tableHandler);
	}
	
	/**
	 * Initialize the exists feature function
	 * 
	 * @param arguments an array of arguments with the type returned by getParameterTypes()
	 * @throws MaltChainedException
	 */
	public void initialize(Object[] arguments) throws MaltChainedException {
		if (arguments.length != 1) {
			throw new SyntaxGraphException("Could not initialize ExistsFeature: number of arguments are not correct. ");
		}
		// Checks that the two arguments are address functions
		if (!(arguments[0] instanceof AddressFunction)) {
			throw new SyntaxGraphException("Could not initialize ExistsFeature: the first argument is not an address function. ");
		}

		setAddressFunction((AddressFunction)arguments[0]);
		
		// Creates a symbol table called "EXISTS" using one null value
//		setSymbolTable(tableHandler.addSymbolTable("EXISTS", ColumnDescription.INPUT, "one"));
//		
//		table.addSymbol("TRUE"); // The address exists
//		table.addSymbol("FALSE"); // The address don't exists
	}
	
	/**
	 * Returns an array of class types used by the feature extraction system to invoke initialize with
	 * correct arguments.
	 * 
	 * @return an array of class types
	 */
	public Class<?>[] getParameterTypes() {
		Class<?>[] paramTypes = { org.maltparser.core.feature.function.AddressFunction.class };
		return paramTypes; 
	}
	/**
	 * Returns the string representation of the integer <code>code</code> according to the exists feature function. 
	 * 
	 * @param code the integer representation of the symbol
	 * @return the string representation of the integer <code>code</code> according to the exists feature function.
	 * @throws MaltChainedException
	 */
	public String getSymbol(int code) throws MaltChainedException {
		return (code == 1)?"true":"false"; 
	}
	
	/**
	 * Returns the integer representation of the string <code>symbol</code> according to the exists feature function.
	 * 
	 * @param symbol the string representation of the symbol
	 * @return the integer representation of the string <code>symbol</code> according to the exists feature function.
	 * @throws MaltChainedException
	 */
	public int getCode(String symbol) throws MaltChainedException {
		return (symbol.equals("true"))?1:0;
	}
	
	/**
	 * Cause the exists feature function to update the cardinality of the feature value.
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
		featureValue.setIndexCode(1);
		featureValue.setNullValue(false);
		if (addressFunction.getAddressValue().getAddress() != null) {
			featureValue.setSymbol("true");
			featureValue.setValue(1);
		} else {
			featureValue.setSymbol("false");
			featureValue.setValue(0);
		}
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
	 * Returns the symbol table used by the exists feature function
	 * 
	 * @return the symbol table used by the exists feature function
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
	 * Returns symbol table handler
	 * 
	 * @return a symbol table handler
	 */
	public SymbolTableHandler getTableHandler() {
		return tableHandler;
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
	 * Sets the symbol table used by the exists feature function
	 * 
	 * @param table
	 */
	public void setSymbolTable(SymbolTable table) {
		this.table = table;
	}
	
	public  int getType() {
		return ColumnDescription.BOOLEAN;
	}
	
	public String getMapIdentifier() {
		return "EXISTS";
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
		sb.append("Exists(");
		sb.append(addressFunction.toString());
		sb.append(')');
		return sb.toString();
	}
}
