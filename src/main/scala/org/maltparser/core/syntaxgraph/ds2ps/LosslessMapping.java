package org.maltparser.core.syntaxgraph.ds2ps;


import java.util.SortedMap;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.helper.SystemLogger;
import org.maltparser.core.io.dataformat.ColumnDescription;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.symbol.SymbolTable;
import org.maltparser.core.symbol.SymbolTableHandler;
import org.maltparser.core.syntaxgraph.MappablePhraseStructureGraph;
import org.maltparser.core.syntaxgraph.edge.Edge;
import org.maltparser.core.syntaxgraph.headrules.HeadRules;
import org.maltparser.core.syntaxgraph.node.DependencyNode;
import org.maltparser.core.syntaxgraph.node.NonTerminalNode;
import org.maltparser.core.syntaxgraph.node.PhraseStructureNode;
/**
*
*
* @author Johan Hall
*/
public class LosslessMapping implements Dependency2PhraseStructure {
	private String DEPREL = "DEPREL";
	private String PHRASE = "PHRASE";
	private String HEADREL = "HEADREL";
	private String ATTACH = "ATTACH";
	private String CAT = "CAT";
	private String EDGELABEL;
	private final char EMPTY_SPINE = '*';
	private final String EMPTY_LABEL = "??";
	private final char SPINE_ELEMENT_SEPARATOR = '|';
	private final char LABEL_ELEMENT_SEPARATOR = '~';
	private final char QUESTIONMARK = '?';
	private String optionString;
	private HeadRules headRules;
	private DataFormatInstance dependencyDataFormatInstance;
	private DataFormatInstance phraseStructuretDataFormatInstance;
	private boolean lockUpdate = false;
	private int nonTerminalCounter;
	private StringBuilder deprel;
	private StringBuilder headrel;
	private StringBuilder phrase;
	
	public LosslessMapping(DataFormatInstance dependencyDataFormatInstance, DataFormatInstance phraseStructuretDataFormatInstance) {
		setDependencyDataFormatInstance(dependencyDataFormatInstance);
		setPhraseStructuretDataFormatInstance(phraseStructuretDataFormatInstance);
		deprel = new StringBuilder();
		headrel = new StringBuilder();
		phrase = new StringBuilder();
		
		if (phraseStructuretDataFormatInstance.getPhraseStructureEdgeLabelColumnDescriptionSet().size() == 1) {
			for (ColumnDescription column : phraseStructuretDataFormatInstance.getPhraseStructureEdgeLabelColumnDescriptionSet()) {
				EDGELABEL = column.getName();
			}
		}

		clear();
	}

	public void clear() {
		nonTerminalCounter = 0;
	}
	
	public String getOptionString() {
		return optionString;
	}

	public void setOptionString(String optionString) {
		this.optionString = optionString;
	}

	public DataFormatInstance getDependencyDataFormatInstance() {
		return dependencyDataFormatInstance;
	}

	public void setDependencyDataFormatInstance(
			DataFormatInstance dependencyDataFormatInstance) {
		this.dependencyDataFormatInstance = dependencyDataFormatInstance;
	}

	public DataFormatInstance getPhraseStructuretDataFormatInstance() {
		return phraseStructuretDataFormatInstance;
	}

	public void setPhraseStructuretDataFormatInstance(
			DataFormatInstance phraseStructuretDataFormatInstance) {
		this.phraseStructuretDataFormatInstance = phraseStructuretDataFormatInstance;
	}
	
	public void update(MappablePhraseStructureGraph graph, Edge e, Object arg) throws MaltChainedException {
		if (lockUpdate == false) {
//			if (e.getType() == Edge.PHRASE_STRUCTURE_EDGE && e.getSource() instanceof NonTerminalNode && lockUpdate == false) { 
//				if(e.getTarget() instanceof TerminalNode) {
//					PhraseStructureNode top = (PhraseStructureNode)e.getTarget(); 
//					while (top.getParent() != null && ((NonTerminalNode)top.getParent()).getLexicalHead() == (PhraseStructureNode)e.getTarget()) {
//						top = top.getParent();
//					}
//					updateDependenyGraph(graph, top);
//				}
//				else if (e.getSource().isRoot()) {
//					updateDependenyGraph(graph, graph.getPhraseStructureRoot());
//				}
//			}
			if (e.getType() == Edge.DEPENDENCY_EDGE && e.getSource() instanceof DependencyNode && e.getTarget() instanceof DependencyNode) {
				if (e.isLabeled() && e.getLabelSet().size() == 4) {
					updatePhraseStructureGraph(graph, (Edge)e, false);
				}
			}
		}
	}
	
	public void updateDependenyGraph(MappablePhraseStructureGraph graph, PhraseStructureNode top) throws MaltChainedException {
		if (graph.nTokenNode() == 1 && graph.nNonTerminals() == 0) {
			// Special case when the root dominates direct a single terminal node
			Edge e = graph.addDependencyEdge(graph.getDependencyRoot(), graph.getDependencyNode(1));
			e.addLabel(graph.getSymbolTables().getSymbolTable(DEPREL), graph.getDefaultRootEdgeLabelSymbol(graph.getSymbolTables().getSymbolTable(DEPREL)));
			e.addLabel(graph.getSymbolTables().getSymbolTable(HEADREL), graph.getDefaultRootEdgeLabelSymbol(graph.getSymbolTables().getSymbolTable(HEADREL)));
			e.addLabel(graph.getSymbolTables().getSymbolTable(PHRASE), "*");
//			e.addLabel(graph.getSymbolTables().getSymbolTable(PHRASE), graph.getDefaultRootEdgeLabelSymbol(graph.getSymbolTables().getSymbolTable(PHRASE)));
			e.addLabel(graph.getSymbolTables().getSymbolTable(ATTACH), graph.getDefaultRootEdgeLabelSymbol(graph.getSymbolTables().getSymbolTable(ATTACH)));
		} else {
			updateDependencyEdges(graph, top);
			updateDependenyLabels(graph);
		}
	}

	
	
	private void updateDependencyEdges(MappablePhraseStructureGraph graph, PhraseStructureNode top) throws MaltChainedException {
		if (top == null) {
			return;
		}
		DependencyNode head = null;
		DependencyNode dependent = null;
		if (top instanceof NonTerminalNode) {
			for (PhraseStructureNode node : ((NonTerminalNode)top).getChildren()) {
				if (node instanceof NonTerminalNode) {
					updateDependencyEdges(graph,node);
				} else {
					head = ((NonTerminalNode)top).getLexicalHead(headRules);
					dependent = (DependencyNode)node;
					if (head != null && dependent != null && head != dependent) {
						lockUpdate = true;
						if (!dependent.hasHead()) {
							graph.addDependencyEdge(head, dependent);
						} 
						else if (head != dependent.getHead()) {
							graph.moveDependencyEdge(head, dependent);
						}
						lockUpdate = false;
					}
				}
			}
		}
		
		head = null;
		if (top.getParent() != null) {
			head = ((NonTerminalNode)top.getParent()).getLexicalHead(headRules);
		} else if (top.isRoot()) {
			head = (DependencyNode)top;	
		}
		
		if (top instanceof NonTerminalNode) {
			dependent = ((NonTerminalNode)top).getLexicalHead(headRules);
		} else if (!top.isRoot()) {
			dependent = (DependencyNode)top;
		}
		if (head != null && dependent != null && head != dependent) {
			lockUpdate = true;
			if (!dependent.hasHead()) {
				graph.addDependencyEdge(head, dependent);
			} 
			else if (head != dependent.getHead()) {
				graph.moveDependencyEdge(head, dependent);
			}
			lockUpdate = false;
		}
	}
	
	private void updateDependenyLabels(MappablePhraseStructureGraph graph) throws MaltChainedException {
		for (int index :graph.getTokenIndices()) {
			PhraseStructureNode top = (PhraseStructureNode)graph.getTokenNode(index);
			
			while (top != null && top.getParent() != null &&graph.getTokenNode(index) == ((NonTerminalNode)top.getParent()).getLexicalHead(headRules)) {
				top = top.getParent();
			}
			lockUpdate = true;
			labelDependencyEdge(graph, graph.getTokenNode(index).getHeadEdge(), top);
			lockUpdate = false;
		}	
	}
	
	
//	private void updateDependenyLabels(MappablePhraseStructureGraph graph, PhraseStructureNode top) throws MaltChainedException {
//		if (top == null) {
//			return;
//		}
//		DependencyNode head = null;
//		DependencyNode dependent = null;
//		if (top instanceof NonTerminalNode) {
//			for (PhraseStructureNode node : ((NonTerminalNode)top).getChildren()) {
//				if (node instanceof NonTerminalNode) {
//					updateDependenyLabels(graph, node);
//				} else {
//					head = ((NonTerminalNode)top).getLexicalHead(headRules);
//					dependent = (DependencyNode)node;
//					if (head != null && dependent != null && head != dependent) {
//						lockUpdate = true;
//						if (dependent.hasHead()) {
//							Edge e = dependent.getHeadEdge();
//							labelDependencyEdge(graph, e, node);
//						}
//						lockUpdate = false;
//					}
//				}
//			}
//		}
//		
//		dependent = null;
//		if (top instanceof NonTerminalNode) {
//			dependent = ((NonTerminalNode)top).getLexicalHead(headRules);
//		}
//
//		if (dependent != null) {
//			lockUpdate = true;
//			if (dependent.hasHead()) {
//				Edge e = dependent.getHeadEdge();
//				labelDependencyEdge(graph, e, top);
//			}
//			lockUpdate = false;
//		}
//	}
	
	private void labelDependencyEdge(MappablePhraseStructureGraph graph, Edge e, PhraseStructureNode top) throws MaltChainedException {
		if  (e == null) {
			return;
		}
		SymbolTableHandler symbolTables = graph.getSymbolTables();
		deprel.setLength(0);
		phrase.setLength(0);
		headrel.setLength(0);

		e.removeLabel(symbolTables.getSymbolTable(DEPREL));
		e.removeLabel(symbolTables.getSymbolTable(HEADREL));
		e.removeLabel(symbolTables.getSymbolTable(PHRASE));
		e.removeLabel(symbolTables.getSymbolTable(ATTACH));
		
		int i = 0;
		SortedMap<String, SymbolTable> edgeLabelSymbolTables = phraseStructuretDataFormatInstance.getPhraseStructureEdgeLabelSymbolTables();
		SortedMap<String, SymbolTable> nodeLabelSymbolTables = phraseStructuretDataFormatInstance.getPhraseStructureNodeLabelSymbolTables();
		if (!top.isRoot()) {
			for (String name : edgeLabelSymbolTables.keySet()) {
				if (top.hasParentEdgeLabel(symbolTables.getSymbolTable(name))) {
					deprel.append(top.getParentEdgeLabelSymbol(symbolTables.getSymbolTable(name)));
				} else {
					deprel.append(EMPTY_LABEL);
				}
				i++;
				if (i < edgeLabelSymbolTables.size()) {
					deprel.append(LABEL_ELEMENT_SEPARATOR);
				}
			}
			if (deprel.length() != 0) {
				e.addLabel(symbolTables.getSymbolTable(DEPREL), deprel.toString());
			}
		} else {
			String deprelDefaultRootLabel = graph.getDefaultRootEdgeLabelSymbol(symbolTables.getSymbolTable(DEPREL));
			if (deprelDefaultRootLabel != null) {
				e.addLabel(symbolTables.getSymbolTable(DEPREL), deprelDefaultRootLabel);
			} else {
				e.addLabel(symbolTables.getSymbolTable(DEPREL), EMPTY_LABEL);
			}
		}
		PhraseStructureNode tmp = (PhraseStructureNode)e.getTarget();
		while (tmp != top && tmp.getParent() != null) { // && !tmp.getParent().isRoot()) {
			i=0;
			for (String name : edgeLabelSymbolTables.keySet()) {
				if (tmp.hasParentEdgeLabel(symbolTables.getSymbolTable(name))) {
					headrel.append(tmp.getParentEdgeLabelSymbol(symbolTables.getSymbolTable(name)));
				} else {
					headrel.append(EMPTY_LABEL);
				}
				i++;
				if (i < edgeLabelSymbolTables.size()) {
					headrel.append(LABEL_ELEMENT_SEPARATOR);
				}	
			}
			i=0;
			headrel.append(SPINE_ELEMENT_SEPARATOR);
			for (String name : nodeLabelSymbolTables.keySet()) {
				if (tmp.getParent().hasLabel(symbolTables.getSymbolTable(name))) {
					phrase.append(tmp.getParent().getLabelSymbol(symbolTables.getSymbolTable(name)));
				} else {
					if (tmp.getParent().isRoot()) {
						String deprelDefaultRootLabel = graph.getDefaultRootEdgeLabelSymbol(symbolTables.getSymbolTable(PHRASE));
						if (deprelDefaultRootLabel != null) {
							phrase.append(deprelDefaultRootLabel);
						} else {
							phrase.append(EMPTY_LABEL);
						}
					} else {
						phrase.append(EMPTY_LABEL);
					}
				}
				i++;
				if (i < nodeLabelSymbolTables.size()) {
					phrase.append(LABEL_ELEMENT_SEPARATOR);
				}
			}
			phrase.append(SPINE_ELEMENT_SEPARATOR);
			tmp = tmp.getParent();
		}
		if (phrase.length() == 0) {
			headrel.append(EMPTY_SPINE);
			phrase.append(EMPTY_SPINE);
		} else {
			headrel.setLength(headrel.length()-1);
			phrase.setLength(phrase.length()-1);
		}
		e.addLabel(symbolTables.getSymbolTable(HEADREL), headrel.toString());
		e.addLabel(symbolTables.getSymbolTable(PHRASE), phrase.toString());
		int a = 0;
		tmp = (PhraseStructureNode)e.getSource();
		while (top.getParent() != null && tmp.getParent() != null && tmp.getParent() != top.getParent()) {
			a++;
			tmp = tmp.getParent();
		}
		e.addLabel(symbolTables.getSymbolTable(ATTACH), Integer.toString(a));
	}
	
	public void connectUnattachedSpines(MappablePhraseStructureGraph graph) throws MaltChainedException {
		connectUnattachedSpines(graph, graph.getDependencyRoot());
		
		if (!graph.getPhraseStructureRoot().isLabeled()) {
			graph.getPhraseStructureRoot().addLabel(graph.getSymbolTables().addSymbolTable(CAT), graph.getDefaultRootEdgeLabelSymbol(graph.getSymbolTables().getSymbolTable(PHRASE)));

		}
	}
	
	private void connectUnattachedSpines(MappablePhraseStructureGraph graph, DependencyNode depNode) throws MaltChainedException {
		if (!depNode.isRoot()) {
			PhraseStructureNode dependentSpine = (PhraseStructureNode)depNode;
			while (dependentSpine.getParent() != null) {
				dependentSpine = dependentSpine.getParent();
			}
			if (!dependentSpine.isRoot()) {
				updatePhraseStructureGraph(graph,depNode.getHeadEdge(),true);
			}
		}
		for (int i = 0; i < depNode.getLeftDependentCount(); i++) {
			connectUnattachedSpines(graph, depNode.getLeftDependent(i));
		}
		for (int i = depNode.getRightDependentCount()-1; i >= 0 ; i--) {
			connectUnattachedSpines(graph, depNode.getRightDependent(i));
		}
	}
	
	public void updatePhraseStructureGraph(MappablePhraseStructureGraph graph, Edge depEdge, boolean attachHeadSpineToRoot) throws MaltChainedException {
		PhraseStructureNode dependentSpine = (PhraseStructureNode)depEdge.getTarget();
		
		if (((PhraseStructureNode)depEdge.getTarget()).getParent() == null) {
			// Restore dependent spine
			String phraseSpineLabel = null;
			String edgeSpineLabel = null;
			int empty_label = 0;
			
			if (depEdge.hasLabel(graph.getSymbolTables().getSymbolTable(PHRASE))) {
				phraseSpineLabel = depEdge.getLabelSymbol(graph.getSymbolTables().getSymbolTable(PHRASE));
			}
			if (depEdge.hasLabel(graph.getSymbolTables().getSymbolTable(HEADREL))) {
				edgeSpineLabel = depEdge.getLabelSymbol(graph.getSymbolTables().getSymbolTable(HEADREL));
			}
			if (phraseSpineLabel != null && phraseSpineLabel.length() > 0 && phraseSpineLabel.charAt(0) != EMPTY_SPINE) {
				int ps = 0, es = 0, i = 0, j = 0, n = phraseSpineLabel.length()-1, m = edgeSpineLabel.length()-1;
				PhraseStructureNode child = (PhraseStructureNode)depEdge.getTarget();
				while (true) {
					while (i <= n && phraseSpineLabel.charAt(i) != SPINE_ELEMENT_SEPARATOR) {
						if (phraseSpineLabel.charAt(i) == QUESTIONMARK) {
							empty_label++;
						} else {
							empty_label = 0;
						}
						i++;
					}
					if (depEdge.getSource().isRoot() && i >= n) {
						dependentSpine = graph.getPhraseStructureRoot();
					} else {
						dependentSpine = graph.addNonTerminalNode(++nonTerminalCounter);
					}

					if (empty_label != 2 && ps != i) {
						dependentSpine.addLabel(graph.getSymbolTables().addSymbolTable(CAT), phraseSpineLabel.substring(ps,i));
					} 

					empty_label = 0;
					if (edgeSpineLabel != null) {
						while (j <= m && edgeSpineLabel.charAt(j) != SPINE_ELEMENT_SEPARATOR) {
							if (edgeSpineLabel.charAt(j) == QUESTIONMARK) {
								empty_label++;
							} else {
								empty_label = 0;
							}
							j++;
						}
					}
					lockUpdate = true;
					Edge e = graph.addPhraseStructureEdge(dependentSpine, child);
					if (empty_label != 2 && es != j && edgeSpineLabel != null  && e != null) {
						e.addLabel(graph.getSymbolTables().addSymbolTable(EDGELABEL), edgeSpineLabel.substring(es,j));
					} else if (es == j) {
						e.addLabel(graph.getSymbolTables().addSymbolTable(EDGELABEL), EMPTY_LABEL);
					}

					lockUpdate = false;
					child = dependentSpine;
					if (i >= n) { break; }
					empty_label = 0;
					ps = i = i + 1;
					es = j = j + 1;
				}
			}

			// Recursively attach the dependent spines to target node. 
			DependencyNode target = (DependencyNode)depEdge.getTarget();
			for (int i = 0; i < target.getLeftDependentCount(); i++) {
				updatePhraseStructureGraph(graph, target.getLeftDependent(i).getHeadEdge(), attachHeadSpineToRoot);
			}
			for (int i = target.getRightDependentCount()-1; i >= 0 ; i--) {
				updatePhraseStructureGraph(graph, target.getRightDependent(i).getHeadEdge(), attachHeadSpineToRoot);
			}
		} else {
			// If dependent spine already exist, then set dependentSpine to the highest nonterminal
			// of the dependent spine.
			while (dependentSpine.getParent() != null && !dependentSpine.getParent().isRoot()) {
				dependentSpine = dependentSpine.getParent();
			}
		}

		
		PhraseStructureNode headSpine = null; 
		if (((PhraseStructureNode)depEdge.getSource()).getParent() != null) {
			// If head spine exist, then attach dependent spine to the head spine at the attachment level a.
			int a = 0;
			headSpine = ((PhraseStructureNode)depEdge.getSource()).getParent();
			if (depEdge.hasLabel(graph.getSymbolTables().getSymbolTable(ATTACH))) {
				try {
				a = Integer.parseInt((depEdge.getLabelSymbol(graph.getSymbolTables().getSymbolTable(ATTACH))));
				} catch (NumberFormatException e) {
					throw new MaltChainedException(e.getMessage());
				}
			}
			for (int i = 0; i < a && headSpine != null; i++) {
				headSpine = headSpine.getParent();
			}
			
			if ((headSpine == null || headSpine == dependentSpine) && attachHeadSpineToRoot) {
				headSpine = graph.getPhraseStructureRoot();
			}
			if (headSpine != null) {
				lockUpdate = true;
				Edge e = graph.addPhraseStructureEdge(headSpine, dependentSpine);
				if (depEdge.hasLabel(graph.getSymbolTables().getSymbolTable(DEPREL)) && !depEdge.getLabelSymbol(graph.getSymbolTables().getSymbolTable(DEPREL)).equals(EMPTY_LABEL) & e != null) {
					e.addLabel(graph.getSymbolTables().addSymbolTable(EDGELABEL), depEdge.getLabelSymbol(graph.getSymbolTables().getSymbolTable(DEPREL)));
				}
				lockUpdate = false;
			}
		}
		else if (depEdge.getSource().isRoot() && !depEdge.isLabeled()) {
				headSpine = graph.getPhraseStructureRoot();
				lockUpdate = true;
				Edge e = graph.addPhraseStructureEdge(headSpine, dependentSpine);
				if (depEdge.hasLabel(graph.getSymbolTables().getSymbolTable(DEPREL)) && !depEdge.getLabelSymbol(graph.getSymbolTables().getSymbolTable(DEPREL)).equals(EMPTY_LABEL) & e != null) {
					e.addLabel(graph.getSymbolTables().addSymbolTable(EDGELABEL), depEdge.getLabelSymbol(graph.getSymbolTables().getSymbolTable(DEPREL)));
				} else {
					e.addLabel(graph.getSymbolTables().addSymbolTable(EDGELABEL), graph.getDefaultRootEdgeLabelSymbol(graph.getSymbolTables().getSymbolTable(DEPREL)));
				}
				lockUpdate = false;
				// Recursively attach the dependent spines to target node. 
				DependencyNode target = (DependencyNode)depEdge.getTarget();
				for (int i = 0; i < target.getLeftDependentCount(); i++) {
					updatePhraseStructureGraph(graph, target.getLeftDependent(i).getHeadEdge(), attachHeadSpineToRoot);
				}
				for (int i = target.getRightDependentCount()-1; i >= 0 ; i--) {
					updatePhraseStructureGraph(graph, target.getRightDependent(i).getHeadEdge(), attachHeadSpineToRoot);
				}
		}
	}

	public HeadRules getHeadRules() {
		return headRules;
	}

	public void setHeadRules(HeadRules headRules) {
		this.headRules = headRules;
	}
	
	public void setHeadRules(String headRulesURL) throws MaltChainedException {
		if (headRulesURL != null && headRulesURL.length() > 0 && !headRulesURL.equals("*")) {
			headRules = new HeadRules(SystemLogger.logger(), phraseStructuretDataFormatInstance);
			headRules.parseHeadRules(headRulesURL);
		}
	}
}
