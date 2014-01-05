package org.maltparser.core.syntaxgraph.feature;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.function.AddressFunction;
import org.maltparser.core.feature.value.AddressValue;
import org.maltparser.core.syntaxgraph.SyntaxGraphException;
import org.maltparser.core.syntaxgraph.node.DependencyNode;
import org.maltparser.core.syntaxgraph.node.TokenNode;
/**
*
*
* @author Johan Hall
*/
public class DGraphAddressFunction extends AddressFunction {
	public enum DGraphSubFunction {
		HEAD, LDEP, RDEP, RDEP2, LSIB, RSIB, PRED, SUCC, ANC, PANC, LDESC, PLDESC, RDESC, PRDESC
	};
	private AddressFunction addressFunction;
	private String subFunctionName;
	private DGraphSubFunction subFunction;
	
	public DGraphAddressFunction(String subFunctionName) {
		super();
		setSubFunctionName(subFunctionName);
	}
	
	public void initialize(Object[] arguments) throws MaltChainedException {
		if (arguments.length != 1) {
			throw new SyntaxGraphException("Could not initialize NodeAddressFunction: number of arguments are not correct. ");
		}
		if (!(arguments[0] instanceof AddressFunction)) {
			throw new SyntaxGraphException("Could not initialize NodeAddressFunction: the second argument is not an addres function. ");
		}
		setAddressFunction((AddressFunction)arguments[0]);
	}
	
	public Class<?>[] getParameterTypes() {
		Class<?>[] paramTypes = { org.maltparser.core.feature.function.AddressFunction.class };
		return paramTypes; 
	}
	
	public void update() throws MaltChainedException {
		final AddressValue a = addressFunction.getAddressValue();
		if (a.getAddress() == null) {
			address.setAddress(null);
		} else {
//			try { 
//				a.getAddressClass().asSubclass(org.maltparser.core.syntaxgraph.node.DependencyNode.class);
		
				final DependencyNode node = (DependencyNode)a.getAddress();
				if (subFunction == DGraphSubFunction.HEAD && !node.isRoot()) {
					address.setAddress(node.getHead());
				} else if (subFunction == DGraphSubFunction.LDEP) {
					address.setAddress(node.getLeftmostDependent());
				} else if (subFunction == DGraphSubFunction.RDEP) {
					address.setAddress(node.getRightmostDependent());
				} else if (subFunction == DGraphSubFunction.RDEP2) {
					// To emulate the behavior of MaltParser 0.4 (bug)
					if (!node.isRoot()) {
						address.setAddress(node.getRightmostDependent());
					} else {
						address.setAddress(null);
					}
				} else if (subFunction == DGraphSubFunction.LSIB) {
					address.setAddress(node.getSameSideLeftSibling());
				} else if (subFunction == DGraphSubFunction.RSIB) {
					address.setAddress(node.getSameSideRightSibling());
				} else if ((subFunction == DGraphSubFunction.PRED || subFunction == DGraphSubFunction.SUCC) && node instanceof TokenNode) {
					final TokenNode tokenNode = (TokenNode)node;
					if (subFunction == DGraphSubFunction.PRED) {
						address.setAddress(tokenNode.getPredecessor());
					} else if (subFunction == DGraphSubFunction.SUCC) {
						address.setAddress(tokenNode.getSuccessor());
					}
				} else if (subFunction == DGraphSubFunction.ANC) {
					address.setAddress(node.getAncestor());
				} else if (subFunction == DGraphSubFunction.PANC) {
					address.setAddress(node.getProperAncestor());
				} else if (subFunction == DGraphSubFunction.LDESC) {
					address.setAddress(node.getLeftmostDescendant());
				} else if (subFunction == DGraphSubFunction.PLDESC) {
					address.setAddress(node.getLeftmostProperDescendant());
				} else if (subFunction == DGraphSubFunction.RDESC) {
					address.setAddress(node.getRightmostDescendant());
				} else if (subFunction == DGraphSubFunction.PRDESC) {
					address.setAddress(node.getRightmostProperDescendant());
				} else {
					address.setAddress(null);
				}
//			} catch (ClassCastException e) {
//				address.setAddress(null);
//			}
		}
	}
	
	public void update(Object[] arguments) throws MaltChainedException {
		update();
	}
	
	public AddressFunction getAddressFunction() {
		return addressFunction;
	}

	public void setAddressFunction(AddressFunction addressFunction) {
		this.addressFunction = addressFunction;
	}

	public String getSubFunctionName() {
		return subFunctionName;
	}

	public void setSubFunctionName(String subFunctionName) {
		this.subFunctionName = subFunctionName;
		subFunction = DGraphSubFunction.valueOf(subFunctionName.toUpperCase());
	}
	
	public DGraphSubFunction getSubFunction() {
		return subFunction;
	}

	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		if (!addressFunction.equals(((DGraphAddressFunction)obj).getAddressFunction())) {
			return false;
		} else if (!subFunction.equals(((DGraphAddressFunction)obj).getSubFunction())) {
			return false;
		} 
		return true;
	}

	public String toString() {
		return subFunctionName + "(" + addressFunction.toString() + ")";
	}
}
