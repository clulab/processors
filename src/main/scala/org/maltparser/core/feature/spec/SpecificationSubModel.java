package org.maltparser.core.feature.spec;

import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;

/**
*
*
* @author Johan Hall
*/
public class SpecificationSubModel  implements Iterable<String> {
	private Set<String> featureSpecSet;
	private String name;
	private final Pattern blanks = Pattern.compile("\\s+");
	
	public SpecificationSubModel() {
		this("MAIN");
	}
	
	public SpecificationSubModel(String name) {
		setSubModelName(name);
		featureSpecSet = new TreeSet<String>();
	}
	
	public void add(String featureSpec) {
		if (featureSpec != null && featureSpec.trim().length() > 0) {
			String strippedFeatureSpec = blanks.matcher(featureSpec).replaceAll("");
			featureSpecSet.add(strippedFeatureSpec);
		}
	}

	public String getSubModelName() {
		return name;
	}

	public void setSubModelName(String name) {
		this.name = name;
	}

	public int size() {
		return featureSpecSet.size();
	}
	
	public Iterator<String> iterator() {
		return featureSpecSet.iterator();
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		for (String str : featureSpecSet) {
			sb.append(str);
			sb.append('\n');
		}
		return sb.toString();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((featureSpecSet == null) ? 0 : featureSpecSet.hashCode());
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		SpecificationSubModel other = (SpecificationSubModel) obj;
		if (featureSpecSet == null) {
			if (other.featureSpecSet != null)
				return false;
		} else if (!featureSpecSet.equals(other.featureSpecSet))
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return true;
	}
}
