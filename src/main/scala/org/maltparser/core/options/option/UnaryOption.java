package org.maltparser.core.options.option;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.options.OptionException;
import org.maltparser.core.options.OptionGroup;

/**
 * An unary option is an option that doesn't need a value to the 
 * specified. For example the --system-help option (-h or --help). 
 *
 * @author Johan Hall
 * @since 1.0
**/
public class UnaryOption extends Option {

	/**
	 * Creates an unary option description
	 * 
	 * @param group	a reference to the option group.
	 * @param name	the name of the option.
	 * @param shortDescription	a short description of the option.
	 * @param flag	a short string that can be used in the command line.
	 * @param usage	a string that explains the usage of the option.
	 * @throws OptionException
	 */
	public UnaryOption(OptionGroup group, 
						String name, 
						String shortDescription, 
						String flag, 
						String usage) throws MaltChainedException {
		super(group, name, shortDescription, flag, usage);
	}

	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getValueObject(java.lang.String)
	 */
	public Object getValueObject(String value) throws MaltChainedException {
		if (value.equalsIgnoreCase("used")) {
			return new Boolean(true);
		}
		return null;
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getDefaultValueObject()
	 */
	public Object getDefaultValueObject() throws MaltChainedException {
		return null;
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getDefaultValueString()
	 */
	public String getDefaultValueString() {
		return null;
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getStringRepresentation(java.lang.Object)
	 */
	public String getStringRepresentation(Object value)  {
		return null;
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#setDefaultValue(java.lang.String)
	 */
	public void setDefaultValue(String defaultValue) throws MaltChainedException { }
	
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
