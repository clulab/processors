package org.maltparser.core.options.option;

import java.util.Formatter;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.options.OptionException;
import org.maltparser.core.options.OptionGroup;

/**
 * Abstract class that contains description of an option that are the same over all option types.
 *
 * @author Johan Hall
 * @since 1.0
**/
public abstract class Option implements Comparable<Option>{
	public static final int NONE = 0; 
	/**
	 * The option is only relevant during learning 
	 */
	public static final int TRAIN = 1; 
	/**
	 * The option is only relevant during processing (parsing)
	 */
	public static final int PROCESS = 2; 
	/**
	 * The option is relevant both during learning and processing (parsing)
	 */
	public static final int BOTH = 3; 
	/**
	 * The option is saved during learning and cannot be overloaded during processing (parsing)
	 */
	public static final int SAVE = 4; 
	
	private OptionGroup group;
	private String name;
	private String shortDescription;
	private String flag;
	private int usage;
	private boolean ambiguous;
	
	/**
	 * Creates an option description
	 * 
	 * @param group	a reference to the option group.
	 * @param name	the name of the option.
	 * @param shortDescription	a short description of the option.
	 * @param flag	a flag that can be used in the command line.
	 * @param usage	a string that explains the usage of the option.
	 * @throws OptionException
	 */
	public Option(OptionGroup group, String name, String shortDescription, String flag, String usage) throws MaltChainedException {
		setGroup(group);
		setName(name);
		setShortDescription(shortDescription);
		setFlag(flag);
		setUsage(usage);
		setAmbiguous(false);
	} 
	
	/**
	 * Returns the corresponding object for the option value (specified as a string value).
	 * 
	 * @param value	the string option value
	 * @return the corresponding object for the option value (specified as a string value).
	 * @throws OptionException
	 */
	public abstract Object getValueObject(String value) throws MaltChainedException;
	
	/**
	 * Returns the object for the default value for option.
	 * 
	 * @return	the object for the default value for option.
	 * @throws OptionException
	 */
	public abstract Object getDefaultValueObject() throws MaltChainedException;
	
	/**
	 * Returns a string representation of the default value.
	 * 
	 * @return a string representation of the default value
	 */
	public abstract String getDefaultValueString();
	
	/**
	 * Sets the default value for the option.
	 * 
	 * @param defaultValue	the string default value
	 * @throws OptionException
	 */
	public abstract void setDefaultValue(String defaultValue) throws MaltChainedException;
	
	
	/**
	 * Returns a string representation of the option value.
	 * 
	 * @param value	an option value object
	 * @return a string representation of the option value, if the option value could not be found null is returned.
	 */
	public abstract String getStringRepresentation(Object value);
	
	/**
	 * Returns a reference to a option group.
	 * 
	 * @return	a reference to a option group.
	 */
	public OptionGroup getGroup() {
		return group;
	}
	/**
	 * Sets the reference to the option group
	 * @param group	a reference to a option group
	 */
	public void setGroup(OptionGroup group) {
		this.group = group;
	}	
	/**
	 * Returns the name of the option.
	 * 
	 * @return	the name of the option.
	 */
	public String getName() {
		return name;
	}
	/**
	 * Sets the name of the option.
	 * 
	 * @param name the name of the option.
	 * @throws OptionException
	 */
	public void setName(String name) throws MaltChainedException {
		if (name == null || name.length() == 0) {
			throw new OptionException("The option name has no value. ");	
		}
		this.name = name.toLowerCase();
	}
	/**
	 * Returns a short description of the option
	 * 
	 * @return	a short description of the option
	 */
	public String getShortDescription() {
		return shortDescription;
	}
	/**
	 * Sets a short description of the option
	 * 
	 * @param shortDescription	a short description of the option
	 */
	public void setShortDescription(String shortDescription) {
		this.shortDescription = shortDescription;
	}
	/**
	 * Returns a character that is used as a flag for the command line input
	 * 
	 * @return	a character that is used as a flag for the command line input
	 */
	public String getFlag() {
		return flag;
	}
	/**
	 * Sets a character that is used as a flag for the command line input
	 * 
	 * @param flag	a character that is used as a flag for the command line input
	 * @throws OptionException
	 */
	public void setFlag(String flag) throws MaltChainedException {
		if (flag == null) {
			this.flag = null;
		} else {
			this.flag = flag;
		} 
	}
	/**
	 * Returns the usage of the option.
	 * 
	 * @return	the usage of the option.
	 */
	public int getUsage() {
		return usage;
	}
	/**
	 * Sets the usage of the option.
	 * 
	 * @param usage	the usage of the option.
	 * @throws OptionException
	 */
	public void setUsage(String usage) throws MaltChainedException {
		if (usage == null || usage.equals("") || usage.toLowerCase().equals("none")) {
			this.usage = Option.NONE;
		} else if (usage.toLowerCase().equals("train")) {
			this.usage = Option.TRAIN;
		} else if (usage.toLowerCase().equals("process")) {
			this.usage = Option.PROCESS;
		} else if (usage.toLowerCase().equals("both")) {
			this.usage = Option.BOTH;
		} else if (usage.toLowerCase().equals("save")) {
			this.usage = Option.SAVE;
		} else {
			throw new OptionException("Illegal use of the usage attibute value: "+usage+" for the '"+getName()+"' option. ");
		}
	}
	/**
	 * Sets the usage of the option.
	 * 
	 * @param usage	the usage of the option.
	 * @throws OptionException
	 */
	public void setUsage(int usage) throws MaltChainedException {
		if (usage >= 0 && usage <= 4) {
			this.usage = usage;
		} else {
			throw new OptionException("Illegal use of the usage attibute value: "+usage+" for the '"+getName()+"' option. ");
		}
	}
	
	/**
	 * Returns true if the option name is ambiguous over all option groups, otherwise false.
	 * 
	 * @return	true if the option name is ambiguous over all option groups, otherwise false.
	 */
	public boolean isAmbiguous() {
		return ambiguous;
	}

	/**
	 * Sets true if the option name is ambiguous over all option groups, otherwise false.
	 * 
	 * @param ambiguous	true if the option name is ambiguous over all option groups, otherwise false.
	 */
	public void setAmbiguous(boolean ambiguous) {
		this.ambiguous = ambiguous;
	}

	public int compareTo(Option o) {
		if (group.getName().equals(o.group.getName())) {
			return name.compareTo(o.getName());
		} 
		return group.getName().compareTo(o.group.getName());
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		int splitsize = 45;
		final StringBuilder sb = new StringBuilder();
		Formatter formatter = new Formatter(sb);
		formatter.format("%-20s ", getName());
		if (isAmbiguous()) {
			formatter.format("*");
		} else {
			sb.append(" ");
		}
		if (getFlag() != null) {
			formatter.format("(%4s) : ", "-"+getFlag());
		} else {
			sb.append("       : ");
		}
		int r = shortDescription.length() / splitsize;
		for (int i = 0; i <= r; i++) {
			if (shortDescription.substring(splitsize*i).length() <= splitsize) {
				formatter.format(((i==0)?"%s":"%28s")+"%-45s\n", "", shortDescription.substring(splitsize*i));
			} else {
				formatter.format(((i==0)?"%s":"%28s")+"%-45s\n", "", shortDescription.substring(splitsize*i, splitsize*i+splitsize));				
			}
		}
		return sb.toString();
	}
}
