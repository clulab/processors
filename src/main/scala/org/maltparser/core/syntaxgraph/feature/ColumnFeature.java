package org.maltparser.core.syntaxgraph.feature;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.FeatureException;
import org.maltparser.core.feature.function.FeatureFunction;
import org.maltparser.core.feature.function.Modifiable;
import org.maltparser.core.feature.value.FeatureValue;
import org.maltparser.core.feature.value.SingleFeatureValue;
import org.maltparser.core.io.dataformat.ColumnDescription;
import org.maltparser.core.symbol.SymbolTable;
import org.maltparser.core.symbol.nullvalue.NullValues.NullValueId;

/**
*
*
* @author Johan Hall
*/
public abstract class ColumnFeature implements FeatureFunction, Modifiable {
	protected ColumnDescription column;
	protected final SingleFeatureValue featureValue;
	
	public ColumnFeature() throws MaltChainedException {
		featureValue = new SingleFeatureValue(this);
	}
	
	public abstract void update() throws MaltChainedException;
	public abstract void initialize(Object[] arguments) throws MaltChainedException;
	public abstract Class<?>[] getParameterTypes();
	
	public String getSymbol(int value) throws MaltChainedException {
		return column.getSymbolTable().getSymbolCodeToString(value);
	}
	
	public int getCode(String value) throws MaltChainedException {
		return column.getSymbolTable().getSymbolStringToCode(value);
	}
	
	public ColumnDescription getColumn() {
		return column;
	}
	
	protected void setColumn(ColumnDescription column) {
		this.column = column;
	}
	
	public void setFeatureValue(int indexCode) throws MaltChainedException {
		final String symbol = column.getSymbolTable().getSymbolCodeToString(indexCode);
		
		if (symbol == null) {
			featureValue.update(indexCode, column.getSymbolTable().getNullValueSymbol(NullValueId.NO_NODE), true, 1);
		} else {
			boolean nullValue = column.getSymbolTable().isNullValue(indexCode);
			if (column.getType() == ColumnDescription.STRING || nullValue) {
				featureValue.update(indexCode, symbol, nullValue, 1);
			} else {
				castFeatureValue(symbol);
			}
		}
	}
	
	public void setFeatureValue(String symbol) throws MaltChainedException {
		final int indexCode = column.getSymbolTable().getSymbolStringToCode(symbol);
		if (indexCode < 0) {
			featureValue.update(column.getSymbolTable().getNullValueCode(NullValueId.NO_NODE), symbol, true, 1);
		} else {
			boolean nullValue = column.getSymbolTable().isNullValue(symbol);
			if (column.getType() == ColumnDescription.STRING || nullValue) {
				featureValue.update(indexCode, symbol, nullValue, 1);
			} else {
				castFeatureValue(symbol);
			}
		}
	}
	
	protected void castFeatureValue(String symbol) throws MaltChainedException {
		if (column.getType() == ColumnDescription.INTEGER) {
			try {
				final int dotIndex = symbol.indexOf('.');
				if (dotIndex == -1) {
					featureValue.setValue(Integer.parseInt(symbol));
					featureValue.setSymbol(symbol);
				} else {
					featureValue.setValue(Integer.parseInt(symbol.substring(0,dotIndex)));
					featureValue.setSymbol(symbol.substring(0,dotIndex));
				}
			} catch (NumberFormatException e) {
				throw new FeatureException("Could not cast the feature value '"+symbol+"' to integer value.", e);
			}
		} else if (column.getType() == ColumnDescription.BOOLEAN) {
			final int dotIndex = symbol.indexOf('.');
			if (symbol.equals("1") || symbol.equals("true") ||  symbol.equals("#true#") || (dotIndex != -1 && symbol.substring(0,dotIndex).equals("1"))) {
				featureValue.setValue(1);
				featureValue.setSymbol("true");
			} else if (symbol.equals("false") || symbol.equals("0") || (dotIndex != -1 && symbol.substring(0,dotIndex).equals("0"))) {
				featureValue.setValue(0);
				featureValue.setSymbol("false");
			} else {
				throw new FeatureException("Could not cast the feature value '"+symbol+"' to boolean value.");
			}
		} else if (column.getType() == ColumnDescription.REAL) {
			try {
				featureValue.setValue(Double.parseDouble(symbol));
				featureValue.setSymbol(symbol);
			} catch (NumberFormatException e) {
				throw new FeatureException("Could not cast the feature value '"+symbol+"' to real value.", e);
			}
		}
		if (column.getType() == ColumnDescription.INTEGER || column.getType() == ColumnDescription.BOOLEAN || column.getType() == ColumnDescription.REAL) {
			featureValue.setNullValue(false);
			featureValue.setIndexCode(1);
		}
	}
	
	public FeatureValue getFeatureValue() {
		return featureValue;
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

	public String getColumnName() {
		return column.getName();
	}
	
	public SymbolTable getSymbolTable() {
		return column.getSymbolTable();
	}
	
	public  int getType() {
		return column.getType();
	}
	
	public String getMapIdentifier() {
		return getSymbolTable().getName();
	}
	
	public String toString() {
		return column.getName();
	}
}
