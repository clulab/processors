package org.maltparser.core.feature.value;

import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import org.maltparser.core.feature.function.Function;
import org.maltparser.core.helper.HashSet;
/**
 *  
 *
 * @author Johan Hall
 * @since 1.0
**/
public class MultipleFeatureValue extends FeatureValue {
	protected SortedMap<Integer, String> featureValues;
	
	public MultipleFeatureValue(Function function) {
		super(function);
		setFeatureValues(new TreeMap<Integer, String>()); 
	}
	
	public void reset() {
		super.reset();
		featureValues.clear();
	}
	
	public void addFeatureValue(int code, String Symbol) { 
		featureValues.put(code, Symbol);
	}
	
	protected void setFeatureValues(SortedMap<Integer, String> featureValues) { 
		this.featureValues = featureValues;
	}
	
	public Set<Integer> getCodes() {
		return (Set<Integer>)featureValues.keySet();
	}
	
	public int getFirstCode() {
		return featureValues.firstKey();
	}
	
	public Set<String> getSymbols() {
		return new HashSet<String>(featureValues.values());
	}
	
	public String getFirstSymbol() {
		return featureValues.get(featureValues.firstKey());
	}	
	
	public boolean isMultiple() {
		return true;
	}
	
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		MultipleFeatureValue v = ((MultipleFeatureValue)obj);
		if (!featureValues.equals(v.featureValues))
			return false;
		return super.equals(obj);
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(super.toString());
		sb.append('{');
		for (Integer code : featureValues.keySet()) {
			sb.append('{');
			sb.append(featureValues.get(code));
			sb.append("->");
			sb.append(code);
			sb.append('}');
		}
		sb.append('}');
		return sb.toString();
	}
}
