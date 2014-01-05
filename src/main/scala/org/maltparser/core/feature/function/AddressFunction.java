package org.maltparser.core.feature.function;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.value.AddressValue;

/**
*
*
* @author Johan Hall
*/
public abstract class AddressFunction implements Function {
	protected AddressValue address;
	public AddressFunction() {
		address = new AddressValue(this);
	}
	
	public abstract void update(Object[] arguments) throws MaltChainedException;
	
	/**
	 * Returns the address value of address function
	 * 
	 * @return the address value of address function
	 */
	public AddressValue getAddressValue() {
		return address;
	}
	
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;

		return address.equals(((AddressFunction)obj).getAddressValue());
	}

	public String toString() {
		return address.toString();
	}
}
