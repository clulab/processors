package org.maltparser.core.feature.value;

import org.maltparser.core.feature.function.Function;
/**
 *  
 *
 * @author Johan Hall
 * @since 1.0
**/
public class AddressValue extends FunctionValue {
	private Object address;
	
	public AddressValue(Function function) {
		super(function);
		setAddress(null);
	}
	
	public void reset() {
		setAddress(null);
	}
	
	public Class<?> getAddressClass() {
		if (address != null) {
			return address.getClass();
		}
		return null;
	}
	
	public Object getAddress() {
		return address;
	}

	public void setAddress(Object address) {
		this.address = address;
	}
	
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		AddressValue other = (AddressValue) obj;
		if (address == null) {
			if (other.address != null)
				return false;
		} else if (!address.equals(other.address))
			return false;
		return super.equals(obj);
	}
	
	public int hashCode() {
		return 31 + ((address == null) ? 0 : address.hashCode());
	}

	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(super.toString());
		sb.append(address.toString());
		return sb.toString();
	}
}
