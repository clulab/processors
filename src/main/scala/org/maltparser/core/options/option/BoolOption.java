package org.maltparser.core.options.option;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.options.OptionException;
import org.maltparser.core.options.OptionGroup;

/**
 * A boolean option is an option that can only contain a boolean value (true or false). 
 *
 * @author Johan Hall
 * @since 1.0
**/
public class BoolOption extends Option{
	private Boolean defaultValue;
	
	/**
	 * Creates a boolean option description
	 * 
	 * @param group	a reference to the option group.
	 * @param name	the name of the option.
	 * @param shortDescription	a short description of the option.
	 * @param flag	a short string that can be used in the command line.
	 * @param usage	a string that explains the usage of the option.
	 * @param defaultValue	a default value string (true or false).
	 * @throws OptionException
	 */
	public BoolOption(OptionGroup group, 
			String name, 
			String shortDescription, 
			String flag, 
			String usage, 
			String defaultValue) throws MaltChainedException {
		super(group, name, shortDescription, flag, usage);
		setDefaultValue(defaultValue);
	}

	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getValueObject(java.lang.String)
	 */
	public Object getValueObject(String value) throws MaltChainedException {
		if (value.equalsIgnoreCase("true")) {
			return new Boolean(true);
		} else if (value.equalsIgnoreCase("false")) {
			return new Boolean(false);
		} else {
			throw new OptionException("Illegal boolean value '"+value+"' for the '"+getName()+"' option. ");
		}
	}
	
	public Object getDefaultValueObject() throws MaltChainedException {
		return new Boolean(defaultValue);
	}
	
	public void setDefaultValue(String defaultValue) throws MaltChainedException {
		if (defaultValue.equalsIgnoreCase("true")) {
			this.defaultValue = true; 
		} else if (defaultValue.equalsIgnoreCase("false")) {
			this.defaultValue = false; 
		} else {
			throw new OptionException("Illegal boolean default value '"+defaultValue+"' for the '"+getName()+"' option. ");
		}
	}
	
	public String getDefaultValueString() {
		return defaultValue.toString();
	}
	
	public String getStringRepresentation(Object value) {
		if (value instanceof Boolean) {
			return value.toString();
		}
		return null;
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(super.toString());
		sb.append("-----------------------------------------------------------------------------\n");
		return sb.toString();
	}
}

