package org.maltparserx.core.syntaxgraph.feature;

import java.util.LinkedHashMap;
import java.util.Map;
import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.feature.function.AddressFunction;
import org.maltparserx.core.feature.function.FeatureFunction;
import org.maltparserx.core.feature.value.AddressValue;
import org.maltparserx.core.feature.value.FeatureValue;
import org.maltparserx.core.feature.value.SingleFeatureValue;
import org.maltparserx.core.io.dataformat.ColumnDescription;
import org.maltparserx.core.symbol.SymbolTable;
import org.maltparserx.core.symbol.SymbolTableHandler;
import org.maltparserx.core.symbol.nullvalue.NullValues.NullValueId;
import org.maltparserx.core.syntaxgraph.SyntaxGraphException;
import org.maltparserx.core.syntaxgraph.node.DependencyNode;

public class DistanceFeature implements FeatureFunction {
	protected AddressFunction addressFunction1;
	protected AddressFunction addressFunction2;
	protected SymbolTableHandler tableHandler;
	protected SymbolTable table;
	protected SingleFeatureValue featureValue;
	protected String normalizationString;
	protected Map<Integer,String> normalization;
	
	
	public DistanceFeature(SymbolTableHandler tableHandler) throws MaltChainedException {
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
			throw new SyntaxGraphException("Could not initialize DistanceFeature: number of arguments is not correct. ");
		}
		// Checks that the two arguments are address functions
		if (!(arguments[0] instanceof AddressFunction)) {
			throw new SyntaxGraphException("Could not initialize DistanceFeature: the first argument is not an address function. ");
		}
		if (!(arguments[1] instanceof AddressFunction)) {
			throw new SyntaxGraphException("Could not initialize DistanceFeature: the second argument is not an address function. ");
		}
		if (!(arguments[2] instanceof java.lang.String)) {
			throw new SyntaxGraphException("Could not initialize DistanceFeature: the third argument is not a string. ");
		}
		setAddressFunction1((AddressFunction)arguments[0]);
		setAddressFunction2((AddressFunction)arguments[1]);
		
		normalizationString = (String)arguments[2];
		// Creates a symbol table called "DISTANCE" using one null value
		setSymbolTable(tableHandler.addSymbolTable("DISTANCE_"+normalizationString, ColumnDescription.INPUT, "one"));
		
		String[] items  = normalizationString.split("\\|");
		
		if (items.length <= 0 || !items[0].equals("0")) {
			throw new SyntaxGraphException("Could not initialize DistanceFeature ("+this+"): the third argument (normalization) must contain a list of integer values separated with | and the first element must be 0.");
		}
		int tmp = -1;
		for (int i = 0; i < items.length; i++) {
			int v;
			try {
				v = Integer.parseInt(items[i]);
			} catch (NumberFormatException e) {
				throw new SyntaxGraphException("Could not initialize DistanceFeature ("+this+"): the third argument (normalization) must contain a sorted list of integer values separated with |", e);
			}
			normalization.put(v, ">="+v);
			table.addSymbol(">="+v);
			if (tmp != -1 && tmp >= v) {
				throw new SyntaxGraphException("Could not initialize DistanceFeature ("+this+"): the third argument (normalization) must contain a sorted list of integer values separated with |");
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
		Class<?>[] paramTypes = { org.maltparserx.core.feature.function.AddressFunction.class,
								  org.maltparserx.core.feature.function.AddressFunction.class,
								  java.lang.String.class};
		return paramTypes; 
	}
	
	/**
	 * Returns the string representation of the integer <code>code</code> according to the distance feature function. 
	 * 
	 * @param code the integer representation of the symbol
	 * @return the string representation of the integer <code>code</code> according to the distance feature function.
	 * @throws MaltChainedException
	 */
	public String getSymbol(int code) throws MaltChainedException {
		return table.getSymbolCodeToString(code);
	}
	
	/**
	 * Returns the integer representation of the string <code>symbol</code> according to the distance feature function.
	 * 
	 * @param symbol the string representation of the symbol
	 * @return the integer representation of the string <code>symbol</code> according to the distance feature function.
	 * @throws MaltChainedException
	 */
	public int getCode(String symbol) throws MaltChainedException {
		return table.getSymbolStringToCode(symbol);
	}
	
	/**
	 * Cause the distance feature function to update the cardinality of the feature value.
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
		final AddressValue arg1 = addressFunction1.getAddressValue();
		final AddressValue arg2 = addressFunction2.getAddressValue();
//		featureValue.setKnown(true);
		// if arg1 or arg2 is null, then set a NO_NODE null value as feature value
		if (arg1.getAddress() == null || arg2.getAddress() == null) { 
			featureValue.setIndexCode(table.getNullValueCode(NullValueId.NO_NODE));
			featureValue.setSymbol(table.getNullValueSymbol(NullValueId.NO_NODE));
			featureValue.setValue(1);

			featureValue.setNullValue(true);			
		} else {
			// Unfortunately this method takes a lot of time  arg1.getAddressClass().asSubclass(org.maltparser.core.syntaxgraph.node.DependencyNode.class);
			// Cast the address arguments to dependency nodes
			final DependencyNode node1 = (DependencyNode)arg1.getAddress();
			final DependencyNode node2 = (DependencyNode)arg2.getAddress();
			
			if (!node1.isRoot() && !node2.isRoot()) { 
				// Calculates the distance
				final int index1 = node1.getIndex();
				final int index2 = node2.getIndex();
				final int distance = Math.abs(index1-index2);
				
				
				int lower = -1;
				boolean f = false;
				for (Integer upper : normalization.keySet()) {
					if (distance >= lower && distance < upper) {
						featureValue.setIndexCode(table.getSymbolStringToCode(normalization.get(lower)));
						featureValue.setSymbol(normalization.get(lower));
						featureValue.setValue(1);
						f = true;
						break;
					}
					lower = upper;
				}
				if (f == false) {
					featureValue.setIndexCode(table.getSymbolStringToCode(normalization.get(lower)));
					featureValue.setSymbol(normalization.get(lower));
					featureValue.setValue(1);
				}
				
				// Tells the feature value that the feature is known and is not a null value
				
				featureValue.setNullValue(false);

			} else { 
				// if node1 or node2 is a root node, set a ROOT_NODE null value as feature value
				featureValue.setIndexCode(table.getNullValueCode(NullValueId.ROOT_NODE));
				featureValue.setSymbol(table.getNullValueSymbol(NullValueId.ROOT_NODE));
				featureValue.setValue(1);
				featureValue.setNullValue(true);
			}
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
	 * Returns the symbol table used by the distance feature function
	 * 
	 * @return the symbol table used by the distance feature function
	 */
	public SymbolTable getSymbolTable() {
		return table;
	}
	
	/**
	 * Returns the address function 1 (argument 1) 
	 * 
	 * @return the address function 1 (argument 1) 
	 */
	public AddressFunction getAddressFunction1() {
		return addressFunction1;
	}


	/**
	 * Sets the address function 1 (argument 1) 
	 * 
	 * @param addressFunction1 a address function 1 (argument 1) 
	 */
	public void setAddressFunction1(AddressFunction addressFunction1) {
		this.addressFunction1 = addressFunction1;
	}
	
	/**
	 * Returns the address function 2 (argument 2) 
	 * 
	 * @return the address function 1 (argument 2) 
	 */
	public AddressFunction getAddressFunction2() {
		return addressFunction2;
	}

	/**
	 * Sets the address function 2 (argument 2) 
	 * 
	 * @param addressFunction2 a address function 2 (argument 2) 
	 */
	public void setAddressFunction2(AddressFunction addressFunction2) {
		this.addressFunction2 = addressFunction2;
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
	 * Sets the symbol table used by the distance feature function
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
		sb.append("Distance(");
		sb.append(addressFunction1.toString());
		sb.append(", ");
		sb.append(addressFunction2.toString());
		sb.append(", ");
		sb.append(normalizationString);
		sb.append(')');
		return sb.toString();
	}
}

