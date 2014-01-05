package org.maltparser.core.feature.spec;
import java.util.Iterator;
import java.util.LinkedHashMap;

import org.maltparser.core.exception.MaltChainedException;

/**
*
*
* @author Johan Hall
*/
public class SpecificationModel implements Iterable<SpecificationSubModel> {
	private String specModelName;
	private LinkedHashMap<String, SpecificationSubModel> subModelMap;
	
	public SpecificationModel() throws MaltChainedException {
		this(null);
	}
	
	public SpecificationModel(String specModelName) throws MaltChainedException {
		setSpecModelName(specModelName);
		subModelMap = new LinkedHashMap<String, SpecificationSubModel>();
	}
	
	public void add(String featureSpec) throws MaltChainedException {
		this.add("MAIN", featureSpec);
	}
	
	public void add(String subModelName, String featureSpec) throws MaltChainedException {
		if (subModelName == null || subModelName.length() < 1  || subModelName.toUpperCase().equals("MAIN")) {
			if (!subModelMap.containsKey("MAIN")) {
				subModelMap.put("MAIN", new SpecificationSubModel("MAIN"));
			}
			subModelMap.get("MAIN").add(featureSpec);
		} else {
			if (!subModelMap.containsKey(subModelName.toUpperCase())) {
				subModelMap.put(subModelName.toUpperCase(), new SpecificationSubModel(subModelName.toUpperCase()));
			}
			subModelMap.get(subModelName.toUpperCase()).add(featureSpec);
		}
	}

	public String getSpecModelName() {
		return specModelName;
	}

	public void setSpecModelName(String specModelName) {
		this.specModelName = specModelName;
	}
	
	public Iterator<SpecificationSubModel> iterator() {
		return subModelMap.values().iterator();
	}
	
	public int size() {
		return subModelMap.size();
	}
	
	public SpecificationSubModel getSpecSubModel(String subModelName) {
		return subModelMap.get(subModelName);
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder();

		for (SpecificationSubModel subModel : this) {
			if (subModel.size() > 0) { 
				if (subModelMap.size() != 1 || subModel.getSubModelName().equalsIgnoreCase("MAIN")) {
					sb.append(subModel.getSubModelName());
					sb.append('\n');
				}
				sb.append(subModel.toString());
			}
		}
		return sb.toString();
	}
}
