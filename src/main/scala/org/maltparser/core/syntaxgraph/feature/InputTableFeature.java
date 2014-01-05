package org.maltparser.core.syntaxgraph.feature;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.function.AddressFunction;
import org.maltparser.core.feature.value.AddressValue;
import org.maltparser.core.io.dataformat.ColumnDescription;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.symbol.TableFeature;
import org.maltparser.core.symbol.nullvalue.NullValues.NullValueId;
import org.maltparser.core.syntaxgraph.SyntaxGraphException;
import org.maltparser.core.syntaxgraph.node.DependencyNode;

public class InputTableFeature extends TableFeature {
	protected AddressFunction addressFunction;

	public InputTableFeature(DataFormatInstance dataFormatInstance) throws MaltChainedException {
		super();
		setTableHandler(dataFormatInstance.getSymbolTables());
	}
	
	public void initialize(Object[] arguments) throws MaltChainedException {
		if (arguments.length != 2) {
			throw new SyntaxGraphException("Could not initialize InputTableFeature: number of arguments are not correct. ");
		}
		if (!(arguments[0] instanceof String)) {
			throw new SyntaxGraphException("Could not initialize InputTableFeature: the first argument is not a string. ");
		}
		if (!(arguments[1] instanceof AddressFunction)) {
			throw new SyntaxGraphException("Could not initialize InputTableFeature: the second argument is not an address function. ");
		}
		setTableName((String)arguments[0]);
		setSymbolTable(tableHandler.getSymbolTable(getTableName()));
		setAddressFunction((AddressFunction)arguments[1]);
		setType(ColumnDescription.STRING); // TODO Probably it could possible to vary the type
	}
	
	public Class<?>[] getParameterTypes() {
		Class<?>[] paramTypes = { java.lang.String.class, org.maltparser.core.feature.function.AddressFunction.class };
		return paramTypes; 
	}

	public void update()  throws MaltChainedException {
		final AddressValue a = addressFunction.getAddressValue();
		
		if (a.getAddress() == null) {
			if (getSymbolTable() != null) {
				featureValue.setIndexCode(getSymbolTable().getNullValueCode(NullValueId.NO_NODE));
				featureValue.setSymbol(getSymbolTable().getNullValueSymbol(NullValueId.NO_NODE));
			} else {
				featureValue.setIndexCode(0);
				featureValue.setSymbol("#null");
			}
//			featureValue.setKnown(true);
			featureValue.setNullValue(true);			
		} else {
			final DependencyNode node = (DependencyNode)a.getAddress();
			if (!node.isRoot()) {
				if (getSymbolTable() != null && node.hasLabel(getSymbolTable())) {
					featureValue.setIndexCode(node.getLabelCode(getSymbolTable()));
					featureValue.setSymbol(getSymbolTable().getSymbolCodeToString(node.getLabelCode(getSymbolTable())));
//					featureValue.setKnown(getSymbolTable().getKnown(node.getLabelCode(getSymbolTable())));
					featureValue.setNullValue(false);
				} else {
//						featureValue.setCode(0);
//						featureValue.setSymbol("#null");
					if (getSymbolTable() != null) {
						featureValue.setIndexCode(getSymbolTable().getNullValueCode(NullValueId.NO_VALUE));
						featureValue.setSymbol(getSymbolTable().getNullValueSymbol(NullValueId.NO_VALUE));
					} 
//						else {
//							featureValue.setCode(0);
//							featureValue.setSymbol("#null");
//						}
//					featureValue.setKnown(true);
					featureValue.setNullValue(true);
				}	
			} else {
				if (getSymbolTable() != null) {
					featureValue.setIndexCode(getSymbolTable().getNullValueCode(NullValueId.ROOT_NODE));
					featureValue.setSymbol(getSymbolTable().getNullValueSymbol(NullValueId.ROOT_NODE));
				} 
//					else {
//						featureValue.setCode(0);
//						featureValue.setSymbol("#null");
//					}
//					featureValue.setCode(0);
//					featureValue.setSymbol("#null");
//				featureValue.setKnown(true);
				featureValue.setNullValue(true);
			}
		}
		featureValue.setValue(1);
	}
	
	public AddressFunction getAddressFunction() {
		return addressFunction;
	}

	public void setAddressFunction(AddressFunction addressFunction) {
		this.addressFunction = addressFunction;
	}
	
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		return obj.toString().equals(toString());
	}
	
	public int hashCode() {
		return 217 + (null == toString() ? 0 : toString().hashCode());
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append("InputTable(");
		sb.append(super.toString());
		sb.append(", ");
		sb.append(addressFunction.toString());
		sb.append(")");
		return sb.toString();
	}

}
