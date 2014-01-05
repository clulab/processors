package org.maltparser.core.propagation.spec;

import java.util.ArrayList;


/**
 * @author Johan Hall
 *
 */
public class PropagationSpecs extends ArrayList<PropagationSpec> {
	public static final long serialVersionUID = 1L;
	
	public PropagationSpecs() {
		super();
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		for (PropagationSpec spec: this) {
			sb.append(spec.toString() + "\n");
		}
		
		return sb.toString();
	}
}
