package org.maltparser.core.options;

import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

import org.maltparser.core.options.option.Option;

/**
 * An option container stores the option values for one instance usage. For example, a
 * single malt configuration there will only be one option container, but for an ensemble parser there
 * could be several option containers. 
 * 
 * There are four types internal option container:
 * <ul>
 * <li>SAVEDOPTION, contains option values load from the saved option file.
 * <li>DEPENDENCIES_RESOLVED, contains option values that overload option values in COMMANDLINE and OPTIONFILE
 * due to dependencies with other options.
 * <li>COMMANDLINE, contains option values that are read from the command-line prompt.
 * <li>OPTIONFILE, contains option values that are read from the option file.
 * </ul>
 * <p>These internal option containers have following priority: SAVEDOPTION, DEPENDENCIES_RESOLVED, COMMANDLINE,
 * OPTIONFILE. If an option cannot be found in the SAVEDOPTION internal option container it will continue to
 * look in the DEPENDENCIES_RESOLVED internal option container and and so fourth. If the option value cannot be
 * found in none of the internal option container, the option manager uses the default option value provided by
 * the option description.</p>
 *
 * @author Johan Hall
 * @since 1.0
**/
public class OptionContainer implements Comparable<OptionContainer>{
	/* Types of internal option container */
	public static final int SAVEDOPTION = 0;
	public static final int DEPENDENCIES_RESOLVED = 1; 
	public static final int COMMANDLINE = 2; 
	public static final int OPTIONFILE = 3;
	
	private int index;
	private SortedMap<Option,Object> savedOptionMap;
	private SortedMap<Option,Object> dependenciesResolvedOptionMap;
	private SortedMap<Option,Object> commandLineOptionMap;
	private SortedMap<Option,Object> optionFileOptionMap;

	/**
	 * Creates an option container
	 * 
	 * @param index The index of the option container (0..n).
	 */
	public OptionContainer(int index) throws OptionException {
		setIndex(index);
		savedOptionMap = new TreeMap<Option,Object>();
		dependenciesResolvedOptionMap = new TreeMap<Option,Object>();
		commandLineOptionMap = new TreeMap<Option,Object>();
		optionFileOptionMap = new TreeMap<Option,Object>();
	}
	
	/**
	 * Adds an option value to an option to one of the internal option container specified by the type.
	 * 
	 * @param type	the internal option container
	 * @param option	the option object
	 * @param value		the option value object
	 * @throws OptionException
	 */
	public void addOptionValue(int type, Option option, Object value) throws OptionException {
		if (type == OptionContainer.SAVEDOPTION) {
			savedOptionMap.put(option, value);
		} else if (type == OptionContainer.DEPENDENCIES_RESOLVED) {
			dependenciesResolvedOptionMap.put(option, value);
		} else if (type == OptionContainer.COMMANDLINE) {
			commandLineOptionMap.put(option, value);
		} else if (type == OptionContainer.OPTIONFILE) {
			optionFileOptionMap.put(option, value);
		} else {
			throw new OptionException("Unknown option container type");
		}
	}
	
	/**
	 * Returns the option value object for the option. It uses the priority amongst the internal 
	 * option containers.
	 * 
	 * @param option the option object
	 * @return the option value object
	 */
	public Object getOptionValue(Option option) {
		Object value = null;
		for (int i = OptionContainer.SAVEDOPTION; i <= OptionContainer.OPTIONFILE; i++) {
			if (i == OptionContainer.SAVEDOPTION) {
				value = savedOptionMap.get(option);
			} else if (i == OptionContainer.DEPENDENCIES_RESOLVED) {
				value = dependenciesResolvedOptionMap.get(option);
			} else if (i == OptionContainer.COMMANDLINE) {
				value = commandLineOptionMap.get(option);
			} else if (i == OptionContainer.OPTIONFILE) {
				value = optionFileOptionMap.get(option);
			}
			if (value != null) {
				return value;
			}
		}
		return null;
	}
	
	/**
	 * Returns a string representation of the option value for the specified option. It uses the priority 
	 * amongst the internal option containers.
	 * 
	 * @param option the option object
	 * @return a string representation of the option value
	 */
	public String getOptionValueString(Option option) {
		String value = null;
		for (int i = OptionContainer.SAVEDOPTION; i <= OptionContainer.OPTIONFILE; i++) {
			if (i == OptionContainer.SAVEDOPTION) {
				value = option.getStringRepresentation(savedOptionMap.get(option));
			} else if (i == OptionContainer.DEPENDENCIES_RESOLVED) {
				value = option.getStringRepresentation(dependenciesResolvedOptionMap.get(option));
			} else if (i == OptionContainer.COMMANDLINE) {
				value = option.getStringRepresentation(commandLineOptionMap.get(option));
			} else if (i == OptionContainer.OPTIONFILE) {
				value = option.getStringRepresentation(optionFileOptionMap.get(option));
			}
			if (value != null) {
				return value;
			}
		}
		return null;
	}
	
	
	/**
	 * Returns true if the option is present in the specified internal option container, otherwise false.
	 * 
	 * @param type	the internal option container
	 * @param option	the option object
	 * @return true if the option is present in the specified internal option container, otherwise false
	 * @throws OptionException
	 */
	public boolean contains(int type, Option option) throws OptionException {
		if (type == OptionContainer.SAVEDOPTION) {
			return savedOptionMap.containsValue(option);
		} else if (type == OptionContainer.DEPENDENCIES_RESOLVED) {
			return dependenciesResolvedOptionMap.containsValue(option);
		} else if (type == OptionContainer.COMMANDLINE) {
			return commandLineOptionMap.containsValue(option);
		} else if (type == OptionContainer.OPTIONFILE) {
			return optionFileOptionMap.containsValue(option);
		} else {
			throw new OptionException("Unknown option container type");
		}	
	}
	
	/**
	 * Returns the number of option values amongst all internal option containers.
	 * 
	 * @return the number of option values amongst all internal option containers
	 */
	public int getNumberOfOptionValues() {
		SortedSet<Option> union = new TreeSet<Option>(savedOptionMap.keySet());
		union.addAll(dependenciesResolvedOptionMap.keySet());
		union.addAll(commandLineOptionMap.keySet());
		union.addAll(optionFileOptionMap.keySet());
		return union.size();
	}
	
	/**
	 * Returns the option container index.
	 * 
	 * @return the option container index
	 */
	public int getIndex() {
		return index;
	}
	
	/**
	 * Sets the option container index, if the index is great than or equal 0.
	 * @param index the option container index
	 * @throws OptionException
	 */
	private void setIndex(int index) throws OptionException {
		if (index < 0) {
			throw new OptionException("The option container index must be an integer value great than or equal 0. ");
		}
		this.index = index;
	}

	public int compareTo(OptionContainer that) {
		final int BEFORE = -1;
	    final int EQUAL = 0;
	    final int AFTER = 1;
	    if (this == that) return EQUAL;
	    if (this.index < that.index) return BEFORE;
	    if (this.index > that.index) return AFTER;
	    return EQUAL;
	}
	
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		SortedSet<Option> union = new TreeSet<Option>(savedOptionMap.keySet());
		union.addAll(dependenciesResolvedOptionMap.keySet());
		union.addAll(commandLineOptionMap.keySet());
		union.addAll(optionFileOptionMap.keySet());
		for (Option option : union) {
			Object value = null;
			for (int i = OptionContainer.SAVEDOPTION; i <= OptionContainer.OPTIONFILE; i++) {
				if (i == OptionContainer.SAVEDOPTION) {
					value = savedOptionMap.get(option);
				} else if (i == OptionContainer.DEPENDENCIES_RESOLVED) {
					value = dependenciesResolvedOptionMap.get(option);
				} else if (i == OptionContainer.COMMANDLINE) {
					value = commandLineOptionMap.get(option);
				} else if (i == OptionContainer.OPTIONFILE) {
					value = optionFileOptionMap.get(option);
				}
				if (value != null) {
					break;
				}
			}
			sb.append(option.getGroup().getName()+"\t"+option.getName()+"\t"+value+"\n");
		}
		return sb.toString();
	}

}
