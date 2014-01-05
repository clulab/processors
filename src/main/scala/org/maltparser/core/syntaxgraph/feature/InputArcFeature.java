package org.maltparser.core.syntaxgraph.feature;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.FeatureException;
import org.maltparser.core.feature.function.AddressFunction;
import org.maltparser.core.feature.function.FeatureFunction;
import org.maltparser.core.feature.value.AddressValue;
import org.maltparser.core.feature.value.FeatureValue;
import org.maltparser.core.feature.value.SingleFeatureValue;
import org.maltparser.core.io.dataformat.ColumnDescription;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.symbol.SymbolTable;
import org.maltparser.core.symbol.SymbolTableHandler;
import org.maltparser.core.symbol.nullvalue.NullValues.NullValueId;
import org.maltparser.core.syntaxgraph.SyntaxGraphException;
import org.maltparser.core.syntaxgraph.node.DependencyNode;
/**
*
* @author Johan Hall
* @since 1.1
**/
public class InputArcFeature implements FeatureFunction {
	protected AddressFunction addressFunction1;
	protected AddressFunction addressFunction2;
	protected ColumnDescription column;
	protected DataFormatInstance dataFormatInstance;
	protected SymbolTableHandler tableHandler;
	protected SymbolTable table;
	protected SingleFeatureValue featureValue;

	
	public InputArcFeature(DataFormatInstance dataFormatInstance, SymbolTableHandler tableHandler) throws MaltChainedException {
		super();
		setDataFormatInstance(dataFormatInstance);
		setTableHandler(tableHandler);
		setFeatureValue(new SingleFeatureValue(this));
	}
	
	public void initialize(Object[] arguments) throws MaltChainedException {
		if (arguments.length != 3) {
			throw new FeatureException("Could not initialize InputArcFeature: number of arguments are not correct. ");
		}
		// Checks that the two arguments are address functions

		if (!(arguments[0] instanceof String)) {
			throw new FeatureException("Could not initialize InputArcFeature: the first argument is not a string. ");
		}
		if (!(arguments[1] instanceof AddressFunction)) {
			throw new SyntaxGraphException("Could not initialize InputArcFeature: the second argument is not an address function. ");
		}
		if (!(arguments[2] instanceof AddressFunction)) {
			throw new SyntaxGraphException("Could not initialize InputArcFeature: the third argument is not an address function. ");
		}
		setAddressFunction1((AddressFunction)arguments[1]);
		setAddressFunction2((AddressFunction)arguments[2]);
		
		setColumn(dataFormatInstance.getColumnDescriptionByName((String)arguments[0]));
		setSymbolTable(tableHandler.addSymbolTable("ARC_"+column.getName(),ColumnDescription.INPUT, "one"));
		table.addSymbol("LEFT");
		table.addSymbol("RIGHT");
	}
	
	public Class<?>[] getParameterTypes() {
	    Class<?>[] paramTypes = { java.lang.String.class, org.maltparser.core.feature.function.AddressFunction.class, org.maltparser.core.feature.function.AddressFunction.class };
		return paramTypes;
	}
	
	public int getCode(String symbol) throws MaltChainedException {
		return table.getSymbolStringToCode(symbol);
	}


	public FeatureValue getFeatureValue() {
		return featureValue;
	}


	public String getSymbol(int code) throws MaltChainedException {
		return table.getSymbolCodeToString(code);
	}


	public void updateCardinality() throws MaltChainedException {
//		featureValue.setCardinality(table.getValueCounter());
	}

	public void update() throws MaltChainedException {
		// Retrieve the address value 
		final AddressValue arg1 = addressFunction1.getAddressValue();
		final AddressValue arg2 = addressFunction2.getAddressValue();
		if (arg1.getAddress() != null && arg1.getAddressClass() == org.maltparser.core.syntaxgraph.node.DependencyNode.class &&
		    arg2.getAddress() != null && arg2.getAddressClass() == org.maltparser.core.syntaxgraph.node.DependencyNode.class) {
		    DependencyNode node1 = (DependencyNode)arg1.getAddress();
		    DependencyNode node2 = (DependencyNode)arg2.getAddress();
		    try {
			int head1 = Integer.parseInt(node1.getLabelSymbol(column.getSymbolTable()));
			int head2 = Integer.parseInt(node2.getLabelSymbol(column.getSymbolTable()));
			if (!node1.isRoot() && head1 == node2.getIndex()) {
			    featureValue.setIndexCode(table.getSymbolStringToCode("LEFT"));
			    featureValue.setSymbol("LEFT");
			    featureValue.setNullValue(false);
			} else if (!node2.isRoot() && head2 == node1.getIndex()) {
			    featureValue.setIndexCode(table.getSymbolStringToCode("RIGHT"));
			    featureValue.setSymbol("RIGHT");
			    featureValue.setNullValue(false);			
			} else {
			    featureValue.setIndexCode(table.getNullValueCode(NullValueId.NO_NODE));
			    featureValue.setSymbol(table.getNullValueSymbol(NullValueId.NO_NODE));
			    featureValue.setNullValue(true);
			}
		    } catch (NumberFormatException e) {
		    	throw new FeatureException("The index of the feature must be an integer value. ", e);
		    }
		} else {
		    featureValue.setIndexCode(table.getNullValueCode(NullValueId.NO_NODE));
		    featureValue.setSymbol(table.getNullValueSymbol(NullValueId.NO_NODE));
		    featureValue.setNullValue(true);
		}
//		featureValue.setKnown(true);
		featureValue.setValue(1);
	}

	public ColumnDescription getColumn() {
		return column;
	}

	public void setColumn(ColumnDescription column) throws MaltChainedException {
		if (column.getType() != ColumnDescription.INTEGER) {
			throw new FeatureException("InputArc feature column must be of type integer. ");
		}
		this.column = column;
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
	
	public DataFormatInstance getDataFormatInstance() {
		return dataFormatInstance;
	}

	public void setDataFormatInstance(DataFormatInstance dataFormatInstance) {
		this.dataFormatInstance = dataFormatInstance;
	}

	public void setFeatureValue(SingleFeatureValue featureValue) {
		this.featureValue = featureValue;
	}
	
	public SymbolTable getSymbolTable() {
		return table;
	}

	public void setSymbolTable(SymbolTable table) {
		this.table = table;
	}
	
	public SymbolTableHandler getTableHandler() {
		return tableHandler;
	}

	public void setTableHandler(SymbolTableHandler tableHandler) {
		this.tableHandler = tableHandler;
	}
	
	public  int getType() {
		return ColumnDescription.STRING;
	}
	
	public String getMapIdentifier() {
		return getSymbolTable().getName();
	}
	
	public boolean equals(Object obj) {
		if (!(obj instanceof InputArcFeature)) {
			return false;
		}
		if (!obj.toString().equals(this.toString())) {
			return false;
		}
		return true;
	}
	
	public String toString() {
		return "InputArc(" + column.getName() + ")";
	}
}
