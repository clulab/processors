package org.maltparser.core.feature.function;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.value.FeatureValue;
import org.maltparser.core.symbol.SymbolTable;
/**
*
*
* @author Johan Hall
*/
public interface FeatureFunction extends Function {
	/**
	 * Cause the feature function to update the cardinality of the feature value.
	 * 
	 * @throws MaltChainedException
	 */
//	public abstract void updateCardinality() throws MaltChainedException;
	/**
	 * Returns the string representation of the integer <code>code</code> according to the feature function. 
	 * 
	 * @param code the integer representation of the symbol
	 * @return the string representation of the integer <code>code</code> according to the feature function.
	 * @throws MaltChainedException
	 */
	public abstract String getSymbol(int code) throws MaltChainedException;
	/**
	 * Returns the integer representation of the string <code>symbol</code> according to the feature function.
	 * 
	 * @param symbol the string representation of the symbol
	 * @return the integer representation of the string <code>symbol</code> according to the feature function.
	 * @throws MaltChainedException
	 */
	public abstract int getCode(String symbol) throws MaltChainedException;	
	/**
	 * Returns the symbol table used by the feature function.
	 * 
	 * @return the symbol table used by the feature function.
	 */
	public abstract SymbolTable getSymbolTable();
	/**
	 * Returns the feature value
	 * 
	 * @return the feature value
	 */
	public abstract FeatureValue getFeatureValue(); 
	
	public abstract int getType();
	public abstract String getMapIdentifier();
}
