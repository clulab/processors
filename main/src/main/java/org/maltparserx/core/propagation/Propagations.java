package org.maltparserx.core.propagation;

import java.util.ArrayList;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.io.dataformat.DataFormatInstance;
import org.maltparserx.core.propagation.spec.PropagationSpec;
import org.maltparserx.core.propagation.spec.PropagationSpecs;
import org.maltparserx.core.syntaxgraph.edge.Edge;

public class Propagations {
	private ArrayList<Propagation> propagations;

	
	public Propagations(PropagationSpecs specs,DataFormatInstance dataFormatInstance) throws MaltChainedException {
		propagations = new ArrayList<Propagation>(specs.size());
		for (PropagationSpec spec : specs) {
			propagations.add(new Propagation(spec, dataFormatInstance));
		}
	}

	public void propagate(Edge e) throws MaltChainedException {
		for (Propagation propagation : propagations) {
			propagation.propagate(e);
		}
	}
	


	public ArrayList<Propagation> getPropagations() {
		return propagations;
	}

	@Override
	public String toString() {
		return "Propagations [propagations=" + propagations + "]";
	}
	
	
}
