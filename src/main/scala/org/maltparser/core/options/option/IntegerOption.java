package org.maltparser.core.options.option;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.options.OptionException;
import org.maltparser.core.options.OptionGroup;

/**
 * An integer option is an option that can only contain an integer value.  
 *
 * @author Johan Hall
 * @since 1.0
**/
public class IntegerOption extends Option{
	private int defaultValue = 0;
	
	/**
	 * Creates an integer option description
	 * 
	 * @param group	a reference to the option group.
	 * @param name	the name of the option.
	 * @param shortDescription	a short description of the option.
	 * @param flag	a short string that can be used in the command line.
	 * @param usage	a string that explains the usage of the option.
	 * @param defaultValue	a default value string (must be an integer value).
	 * @throws OptionException
	 */
	public IntegerOption(OptionGroup group, 
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
		try {
			return new Integer(Integer.parseInt(value));
		} catch (NumberFormatException e) {
			throw new OptionException("Illegal integer value '"+value+"' for the '"+getName()+"' option. ", e);
		}	
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getDefaultValueObject()
	 */
	public Object getDefaultValueObject() throws MaltChainedException {
		return new Integer(defaultValue);
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#setDefaultValue(java.lang.String)
	 */
	public void setDefaultValue(String defaultValue) throws MaltChainedException {
		try {
			this.defaultValue = Integer.parseInt(defaultValue);
		} catch (NumberFormatException e) {
			throw new OptionException("Illegal integer default value '"+defaultValue+"' for the '"+getName()+"' option. ", e);
		}
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getDefaultValueString()
	 */
	public String getDefaultValueString() {
		return Integer.toString(defaultValue);
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getStringRepresentation(java.lang.Object)
	 */
	public String getStringRepresentation(Object value) {
		if (value instanceof Integer) {
			return value.toString();
		}
		return null;
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#toString()
	 */
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(super.toString());
		sb.append("-----------------------------------------------------------------------------\n");
		return sb.toString();
	}
}
