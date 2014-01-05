package org.maltparser.core.syntaxgraph.feature;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.function.AddressFunction;
import org.maltparser.core.feature.value.AddressValue;
import org.maltparser.core.io.dataformat.ColumnDescription;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.symbol.SymbolTableHandler;
import org.maltparser.core.symbol.TableFeature;
import org.maltparser.core.symbol.nullvalue.NullValues.NullValueId;
import org.maltparser.core.syntaxgraph.SyntaxGraphException;
import org.maltparser.core.syntaxgraph.node.DependencyNode;
/**
*
*
* @author Johan Hall
*/
public class OutputTableFeature extends TableFeature {
	protected AddressFunction addressFunction;
	protected SymbolTableHandler tableHandler;
	
	public OutputTableFeature(DataFormatInstance dataFormatInstance) throws MaltChainedException {
		super();
		setTableHandler(dataFormatInstance.getSymbolTables());
	}
	
	public void initialize(Object[] arguments) throws MaltChainedException {
		if (arguments.length != 2) {
			throw new SyntaxGraphException("Could not initialize OutputTableFeature: number of arguments are not correct. ");
		}
		if (!(arguments[0] instanceof String)) {
			throw new SyntaxGraphException("Could not initialize OutputTableFeature: the first argument is not a string. ");
		}
		if (!(arguments[1] instanceof AddressFunction)) {
			throw new SyntaxGraphException("Could not initialize OutputTableFeature: the second argument is not an address function. ");
		}
		setSymbolTable(tableHandler.getSymbolTable((String)arguments[0]));
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
			featureValue.setIndexCode(getSymbolTable().getNullValueCode(NullValueId.NO_NODE));
			featureValue.setSymbol(getSymbolTable().getNullValueSymbol(NullValueId.NO_NODE));
//			featureValue.setKnown(true);
			featureValue.setNullValue(true);			
		} else {
//			try { 
//				a.getAddressClass().asSubclass(org.maltparser.core.syntaxgraph.node.DependencyNode.class);

				final DependencyNode node = (DependencyNode)a.getAddress();
				if (!node.isRoot()) {
					if (node.hasHead()) {
						featureValue.setIndexCode(node.getHeadEdge().getLabelCode(getSymbolTable()));
						featureValue.setSymbol(getSymbolTable().getSymbolCodeToString(node.getHeadEdge().getLabelCode(getSymbolTable())));
//						featureValue.setKnown(getSymbolTable().getKnown(node.getHeadEdge().getLabelCode(getSymbolTable())));
						featureValue.setNullValue(false);
					} else {
						featureValue.setIndexCode(getSymbolTable().getNullValueCode(NullValueId.NO_VALUE));
						featureValue.setSymbol(getSymbolTable().getNullValueSymbol(NullValueId.NO_VALUE));
//						featureValue.setKnown(true);
						featureValue.setNullValue(true);
					}	
				} else {
					featureValue.setIndexCode(getSymbolTable().getNullValueCode(NullValueId.ROOT_NODE));
					featureValue.setSymbol(getSymbolTable().getNullValueSymbol(NullValueId.ROOT_NODE));
//					featureValue.setKnown(true);
					featureValue.setNullValue(true);
				}
//			} catch (ClassCastException e) {
//				featureValue.setCode(getSymbolTable().getNullValueCode(NullValueId.NO_NODE));
//				featureValue.setSymbol(getSymbolTable().getNullValueSymbol(NullValueId.NO_NODE));
//				featureValue.setKnown(true);
//				featureValue.setNullValue(true);
//			}
		}
		featureValue.setValue(1);
	}
	
	public AddressFunction getAddressFunction() {
		return addressFunction;
	}

	public void setAddressFunction(AddressFunction addressFunction) {
		this.addressFunction = addressFunction;
	}
	
	public SymbolTableHandler getTableHandler() {
		return tableHandler;
	}

	public void setTableHandler(SymbolTableHandler tableHandler) {
		this.tableHandler = tableHandler;
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
		sb.append("OutputTable(");
		sb.append(super.toString());
		sb.append(", ");
		sb.append(addressFunction.toString());
		sb.append(")");
		return sb.toString();
	}
}
