package org.maltparser.core.options.option;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.options.OptionException;
import org.maltparser.core.options.OptionGroup;

/**
 * A string option is an option that contains a string value. 
 *
 * @author Johan Hall
 * @since 1.0
**/
public class StringOption extends Option {
	private String defaultValue;
	
	/**
	 * Creates a string option description
	 * 
	 * @param group	a reference to the option group.
	 * @param name	the name of the option.
	 * @param shortDescription	a short description of the option.
	 * @param flag	a short string that can be used in the command line.
	 * @param usage	a string that explains the usage of the option.
	 * @param defaultValue	a default value string.
	 * @throws OptionException
	 */
	public StringOption(OptionGroup group, 
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
		return new String(value);
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getDefaultValueObject()
	 */
	public Object getDefaultValueObject() throws MaltChainedException {
		return new String(defaultValue);
	}

	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#setDefaultValue(java.lang.String)
	 */
	public void setDefaultValue(String defaultValue) {
		this.defaultValue = defaultValue;
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getDefaultValueString()
	 */
	public String getDefaultValueString() {
		return defaultValue;
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getStringRepresentation(java.lang.Object)
	 */
	public String getStringRepresentation(Object value) {
		if (value instanceof String) {
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
