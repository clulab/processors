package org.maltparser.core.options;

import java.util.Collection;
import java.util.HashMap;
import java.util.TreeSet;

import org.maltparser.core.options.option.Option;


/**
 * An option group categories a group of options.
 *
 * @author Johan Hall
 * @since 1.0
**/
public class OptionGroup {
	private String name;
	private HashMap<String, Option> options;
	
	public static int toStringSetting = 0;
	public final static int WITHGROUPNAME = 0; 
	public final static int NOGROUPNAME = 1; 
	
	/**
	 * Creates an option group with an option group name.
	 * 
	 * @param name	The name of the option group
	 */
	public OptionGroup(String name) {
		setName(name);
		options = new HashMap<String, Option>();
	}

	/**
	 * Returns the name of the option group
	 * 
	 * @return	the name of the option group
	 */
	public String getName() {
		return name;
	}

	/**
	 * Sets the name of the option group
	 * 
	 * @param name	the name of the option group
	 */
	public void setName(String name) {
		this.name = name.toLowerCase();
	}
	
	/**
	 * Adds an option to the option group.
	 * 
	 * @param option	an option
	 * @throws OptionException
	 */
	public void addOption(Option option) throws OptionException {
		if (option.getName() == null || option.getName().equals("")) {
			throw new OptionException("The option name is null or contains the empty string. ");
		} else if (options.containsKey(option.getName().toLowerCase())) {
			throw new OptionException("The option name already exists for that option group. ");
		} else {
			options.put(option.getName().toLowerCase(), option);
		}
	}
	
	/**
	 * Returns the option according to the option name.
	 * 
	 * @param optionname	an option name
	 * @return	an option, <i>null</i> if the option name can't be found
	 */
	public Option getOption(String optionname) {
		return options.get(optionname);
	}
	
	/**
	 * Returns all options for this option group.
	 * 
	 * @return	a list of options
	 */
	public Collection<Option> getOptionList() {
		return options.values();
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		final StringBuilder sb = new StringBuilder();
//		for (String value : new TreeSet<String>(options.keySet())) {
//			sb.append("super.put(\""); 
//			sb.append("--");
//			sb.append(name);
//			sb.append("-");
//			sb.append(options.get(value).getName());
//			sb.append("\", \"");
//			sb.append(options.get(value).getDefaultValueString());
//			sb.append("\");\n");
//		}

		if (OptionGroup.toStringSetting == OptionGroup.WITHGROUPNAME) {
			sb.append("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
			sb.append("+ " + name+"\n");
			sb.append("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
		}
		
		for (String value : new TreeSet<String>(options.keySet())) {
			sb.append(options.get(value).toString());
		}
		return sb.toString();
	}
}
