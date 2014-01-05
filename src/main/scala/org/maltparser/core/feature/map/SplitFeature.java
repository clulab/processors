package org.maltparser.core.feature.map;

import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.FeatureException;
import org.maltparser.core.feature.function.FeatureFunction;
import org.maltparser.core.feature.function.FeatureMapFunction;
import org.maltparser.core.feature.value.FeatureValue;
import org.maltparser.core.feature.value.FunctionValue;
import org.maltparser.core.feature.value.MultipleFeatureValue;
import org.maltparser.core.feature.value.SingleFeatureValue;
import org.maltparser.core.io.dataformat.ColumnDescription;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.symbol.SymbolTable;
import org.maltparser.core.symbol.SymbolTableHandler;

/**
*
*
* @author Johan Hall
*/
public class SplitFeature implements FeatureMapFunction {
	protected FeatureFunction parentFeature;
	protected MultipleFeatureValue multipleFeatureValue;
	protected DataFormatInstance dataFormatInstance;
	protected ColumnDescription column;
	protected SymbolTable table;
	protected String separators;
	protected Pattern separatorsPattern;
	
	public SplitFeature(DataFormatInstance dataFormatInstance) throws MaltChainedException {
		super();
		setDataFormatInstance(dataFormatInstance);
		multipleFeatureValue = new MultipleFeatureValue(this);
	}
	
	public void initialize(Object[] arguments) throws MaltChainedException {
		if (arguments.length != 2) {
			throw new FeatureException("Could not initialize SplitFeature: number of arguments are not correct. ");
		}
		if (!(arguments[0] instanceof FeatureFunction)) {
			throw new FeatureException("Could not initialize SplitFeature: the first argument is not a feature. ");
		}
		if (!(arguments[1] instanceof String)) {
			throw new FeatureException("Could not initialize SplitFeature: the second argument is not a string. ");
		}
		setParentFeature((FeatureFunction)arguments[0]);
		setSeparators((String)arguments[1]);
		ColumnDescription parentColumn = dataFormatInstance.getColumnDescriptionByName(parentFeature.getSymbolTable().getName());
		if (parentColumn.getType() != ColumnDescription.STRING) {
			throw new FeatureException("Could not initialize SplitFeature: the first argument must be a string. ");
		}
		setColumn(dataFormatInstance.addInternalColumnDescription("SPLIT_"+parentFeature.getSymbolTable().getName(), parentColumn));
		setSymbolTable(column.getSymbolTable());
//		setSymbolTable(tableHandler.addSymbolTable("SPLIT_"+parentFeature.getSymbolTable().getName(), parentFeature.getSymbolTable()));
	}
	
	public Class<?>[] getParameterTypes() {
		Class<?>[] paramTypes = { org.maltparser.core.feature.function.FeatureFunction.class, java.lang.String.class };
		return paramTypes; 
	}

	public FeatureValue getFeatureValue() {
		return multipleFeatureValue;
	}

	public String getSymbol(int code) throws MaltChainedException {
		return table.getSymbolCodeToString(code);
	}
	
	public int getCode(String symbol) throws MaltChainedException {
		return table.getSymbolStringToCode(symbol);
	}

	public void update() throws MaltChainedException {
		multipleFeatureValue.reset();
		parentFeature.update();
		FunctionValue value = parentFeature.getFeatureValue();
		if (value instanceof SingleFeatureValue) {
			String symbol = ((SingleFeatureValue)value).getSymbol();
			if (((FeatureValue)value).isNullValue()) {
				multipleFeatureValue.addFeatureValue(parentFeature.getSymbolTable().getSymbolStringToCode(symbol), symbol);
				multipleFeatureValue.setNullValue(true);
			} else {
				String items[];
				try {
					items = separatorsPattern.split(symbol);
				} catch (PatternSyntaxException e) {
					throw new FeatureException("The split feature '"+this.toString()+"' could not split the value using the following separators '"+separators+"'",e);
				}
				for (int i = 0; i < items.length; i++) {
					if (items[i].length() > 0) {
						multipleFeatureValue.addFeatureValue(table.addSymbol(items[i]), items[i]);
					}
				}
				multipleFeatureValue.setNullValue(false);
			}
		} else if (value instanceof MultipleFeatureValue) {
			if (((MultipleFeatureValue)value).isNullValue()) {
				multipleFeatureValue.addFeatureValue(parentFeature.getSymbolTable().getSymbolStringToCode(((MultipleFeatureValue)value).getFirstSymbol()), ((MultipleFeatureValue)value).getFirstSymbol());
				multipleFeatureValue.setNullValue(true);
			} else {
				for (String symbol : ((MultipleFeatureValue)value).getSymbols()) {
					String items[];
					try {
						items = separatorsPattern.split(symbol);
					} catch (PatternSyntaxException e) {
						throw new FeatureException("The split feature '"+this.toString()+"' could not split the value using the following separators '"+separators+"'", e);
					}
					for (int i = 0; i < items.length; i++) {
						multipleFeatureValue.addFeatureValue(table.addSymbol(items[i]), items[i]);
					}
					multipleFeatureValue.setNullValue(false);
				}
			}
		}
	}

	public void updateCardinality() throws MaltChainedException {
//		parentFeature.updateCardinality();
//		multipleFeatureValue.setCardinality(table.getValueCounter()); 
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
	
	public FeatureFunction getParentFeature() {
		return parentFeature;
	}
	
	public void setParentFeature(FeatureFunction parentFeature) {
		this.parentFeature = parentFeature;
	}

	public String getSeparators() {
		return separators;
	}
	
	public void setSeparators(String separators) {
		this.separators = separators;
		separatorsPattern = Pattern.compile(separators);
	}

	public SymbolTable getSymbolTable() {
		return table;
	}

	public void setSymbolTable(SymbolTable table) {
		this.table = table;
	}

	public SymbolTableHandler getTableHandler() {
		return dataFormatInstance.getSymbolTables();
	}

	public DataFormatInstance getDataFormatInstance() {
		return dataFormatInstance;
	}

	public void setDataFormatInstance(DataFormatInstance dataFormatInstance) {
		this.dataFormatInstance = dataFormatInstance;
	}
	
	public ColumnDescription getColumn() {
		return column;
	}
	
	protected void setColumn(ColumnDescription column) {
		this.column = column;
	}

	public  int getType() {
		return column.getType();
	}
	
	public String getMapIdentifier() {
		return getSymbolTable().getName();
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append("Split(");
		sb.append(parentFeature.toString());
		sb.append(", ");
		sb.append(separators);
		sb.append(')');
		return sb.toString();
	}	
}

