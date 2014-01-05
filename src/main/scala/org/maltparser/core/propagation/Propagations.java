package org.maltparser.core.propagation;

import java.util.ArrayList;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.propagation.spec.PropagationSpec;
import org.maltparser.core.propagation.spec.PropagationSpecs;
import org.maltparser.core.syntaxgraph.edge.Edge;

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
