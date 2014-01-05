package org.maltparser.core.options;

import java.util.Collections;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import org.maltparser.core.options.option.Option;

/**
 * OptionValues contain a number of option containers, which contains the option values (the instance of 
 * options).
 *
 * @author Johan Hall
 * @since 1.0
**/
public class OptionValues {
	private SortedMap<Integer, OptionContainer> optionContainers;
	
	/**
	 * Creates OptionValues.
	 */
	public OptionValues() {
		super();
		optionContainers = Collections.synchronizedSortedMap(new TreeMap<Integer, OptionContainer>());
//		optionContainers = new TreeMap<Integer, OptionContainer>();
	}

	/**
	 * Returns the option value for an option that is in a specific option container.
	 * 
	 * @param containerIndex	the index of the option container.
	 * @param option	the option object
	 * @return	an object that contains the value of the option, <i>null</i> if the option value could not be found.
	 * @throws OptionException
	 */
	public Object getOptionValue(int containerIndex, Option option) throws OptionException {
		OptionContainer oc = optionContainers.get(containerIndex);
		if (oc == null) {
			throw new OptionException("The option container '"+containerIndex+"' cannot be found. ");
		}
		return oc.getOptionValue(option);
	}
	
	/**
	 * Returns a string representation of the option value for an option that is in a specific option container.
	 * 
	 * @param containerIndex	the index of the option container.
	 * @param option	an option object
	 * @return a string representation of the option value for an option that is in a specific option container.
	 * @throws OptionException
	 */
	public String getOptionValueString(int containerIndex, Option option) throws OptionException {
		OptionContainer oc = optionContainers.get(containerIndex);
		if (oc == null) {
			throw new OptionException("The option container '"+containerIndex+"' cannot be found. ");
		}
		return oc.getOptionValueString(option);
	}
	
	/**
	 * Returns the option value for an option.
	 * 
	 * @param option	an option object
	 * @return	 the option value for an option, <i>null</i> if the option value could not be found.
	 * @throws OptionException
	 */
	public Object getOptionValue(Option option) throws OptionException {
		if (optionContainers.size() == 0) {
			return null;
		}
		OptionContainer oc = optionContainers.get(optionContainers.firstKey());
		return oc.getOptionValue(option);
	}
	
	/**
	 * Returns the number of option values for a particular option container.
	 * 
	 * @param containerIndex	The index of the option container.
	 * @return	 the number of option values for a particular option container.
	 */
	public int getNumberOfOptionValues(int containerIndex) {
		if (!optionContainers.containsKey(containerIndex)) {
			return 0;
		}
		return optionContainers.get(containerIndex).getNumberOfOptionValues();
	}
	
	/**
	 * Returns a sorted set of container names.
	 * 
	 * @return	a sorted set of container names.
	 */
	public Set<Integer> getOptionContainerIndices() {
		return optionContainers.keySet();
	}
	
	
	/**
	 * Adds an option value to an option to one of the internal option container specified by the type.
	 * 
	 * @param containerType		the type of the option container.
	 * @param containerIndex	the index of the option container.
	 * @param option	an option to add
	 * @param value	an option value to add
	 * @return	true if the value is added, false if the value already is in use.
	 * @throws OptionException
	 */
	public boolean addOptionValue(int containerType, int containerIndex, Option option, Object value) throws OptionException {
		if (option == null) {
			throw new OptionException("The option cannot be found. ");
		}
		if (value == null) {
			throw new OptionException("The option value cannot be found. ");
		}
		
		if (!optionContainers.containsKey(containerIndex)) {
			optionContainers.put(containerIndex, new OptionContainer(containerIndex));
		} 
		OptionContainer oc = optionContainers.get(containerIndex);
		if (oc == null) {
			throw new OptionException("The option container index "+containerIndex+" is unknown");
		}
		if (!oc.contains(containerType, option)) {
			oc.addOptionValue(containerType, option, value);
			return true;
		}
		return false;
	}
	
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		if (optionContainers.size() == 0) {
			sb.append("No option values.");
		} else if (optionContainers.size() == 1) {
			sb.append(optionContainers.get(optionContainers.firstKey()));
		} else {
			for (Integer index : optionContainers.keySet()) {
				sb.append("Option container : "+index+"\n");
				sb.append(optionContainers.get(index)+"\n");
			}
		}
		return sb.toString();
	}
}
