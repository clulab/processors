package org.maltparser.transform.pseudo;

import java.util.SortedSet;
import java.util.TreeSet;
import java.util.Vector;

import org.apache.log4j.Logger;
import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.io.dataformat.ColumnDescription;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.symbol.SymbolTable;
import org.maltparser.core.syntaxgraph.DependencyStructure;
import org.maltparser.core.syntaxgraph.node.DependencyNode;

/**
 * This class contains methods for projectivizing and deprojectivizing
 * 
 * @author Jens Nilsson
 */
public class PseudoProjectivity {
	static int id = 0;

	private enum PseudoProjectiveEncoding {
		NONE, BASELINE, HEAD, PATH, HEADPATH, TRACE
	};

	private enum CoveredRootAttachment {
		NONE, IGNORE, LEFT, RIGHT, HEAD
	};

	private enum LiftingOrder {
		SHORTEST, DEEPEST
	};

	private PseudoProjectiveEncoding markingStrategy;
	private CoveredRootAttachment rootAttachment;
	private LiftingOrder liftingOrder;
	private Logger configLogger;

	private SymbolTable deprelSymbolTable;
	private SymbolTable pppathSymbolTable;
	private SymbolTable ppliftedSymbolTable;
	private SymbolTable ppcoveredRootSymbolTable;
	
	private ColumnDescription deprelColumn;
	private ColumnDescription pppathColumn;
	private ColumnDescription ppliftedColumn;
	private ColumnDescription ppcoveredRootColumn;
	
	private Vector<Boolean> nodeLifted;
	private Vector<Vector<DependencyNode>> nodeTrace;
	private Vector<DependencyNode> headDeprel;
	private Vector<Boolean> nodePath;
	private Vector<Boolean> isCoveredRoot;
	private Vector<Integer> nodeRelationLength;
	private Vector<String> synacticHeadDeprel;


	public PseudoProjectivity() { }

	public void initialize(String markingStrategyString, String coveredRoot, String liftingOrder, Logger configLogger,
			DataFormatInstance dataFormatInstance) throws MaltChainedException {
		nodeLifted = new Vector<Boolean>();
		nodeTrace = new Vector<Vector<DependencyNode>>();
		headDeprel = new Vector<DependencyNode>();
		nodePath = new Vector<Boolean>();
		isCoveredRoot = new Vector<Boolean>();
		nodeRelationLength = new Vector<Integer>();
		synacticHeadDeprel = new Vector<String>();

		this.configLogger = configLogger;
		if (markingStrategyString.equalsIgnoreCase("none")) {
			markingStrategy = PseudoProjectiveEncoding.NONE;
		} else if (markingStrategyString.equalsIgnoreCase("baseline")) {
			markingStrategy = PseudoProjectiveEncoding.BASELINE;
		} else if (markingStrategyString.equalsIgnoreCase("head")) {
			markingStrategy = PseudoProjectiveEncoding.HEAD;
		} else if (markingStrategyString.equalsIgnoreCase("path")) {
			markingStrategy = PseudoProjectiveEncoding.PATH;
		} else if (markingStrategyString.equalsIgnoreCase("head+path")) {
			markingStrategy = PseudoProjectiveEncoding.HEADPATH;
		} else if (markingStrategyString.equalsIgnoreCase("trace")) {
			markingStrategy = PseudoProjectiveEncoding.TRACE;
		}
		this.deprelColumn = dataFormatInstance.getColumnDescriptionByName("DEPREL");
		this.deprelSymbolTable = deprelColumn.getSymbolTable();
//		this.deprelSymbolTable = dataFormatInstance.getSymbolTables().getSymbolTable("DEPREL");
		if (markingStrategy == PseudoProjectiveEncoding.HEAD || markingStrategy == PseudoProjectiveEncoding.PATH
				|| markingStrategy == PseudoProjectiveEncoding.HEADPATH) {
			this.ppliftedColumn = dataFormatInstance.addInternalColumnDescription("PPLIFTED", "DEPENDENCY_EDGE_LABEL", "BOOLEAN", "", deprelColumn.getNullValueStrategy());
			this.ppliftedSymbolTable = ppliftedColumn.getSymbolTable();
//			this.ppliftedSymbolTable = dataFormatInstance.getSymbolTables().addSymbolTable("PPLIFTED", deprelSymbolTable);
			if (this.markingStrategy == PseudoProjectiveEncoding.PATH) {
				ppliftedSymbolTable.addSymbol("#true#");
				ppliftedSymbolTable.addSymbol("#false#");
			} else {
				ppliftedSymbolTable.addSymbol("#false#");
			}
		}

		if (markingStrategy == PseudoProjectiveEncoding.PATH || markingStrategy == PseudoProjectiveEncoding.HEADPATH) {
			this.pppathColumn = dataFormatInstance.addInternalColumnDescription("PPPATH", "DEPENDENCY_EDGE_LABEL", "BOOLEAN", "", deprelColumn.getNullValueStrategy());
			this.pppathSymbolTable = pppathColumn.getSymbolTable();
			pppathSymbolTable.addSymbol("#true#");
			pppathSymbolTable.addSymbol("#false#");
		}

		if (coveredRoot.equalsIgnoreCase("none")) {
			this.rootAttachment = CoveredRootAttachment.NONE;
		} else if (coveredRoot.equalsIgnoreCase("ignore")) {
			this.rootAttachment = CoveredRootAttachment.IGNORE;
		} else if (coveredRoot.equalsIgnoreCase("left")) {
			this.rootAttachment = CoveredRootAttachment.LEFT;
		} else if (coveredRoot.equalsIgnoreCase("right")) {
			this.rootAttachment = CoveredRootAttachment.RIGHT;
		} else if (coveredRoot.equalsIgnoreCase("head")) {
			this.rootAttachment = CoveredRootAttachment.HEAD;
		}

		if (this.rootAttachment != CoveredRootAttachment.NONE) {
			this.ppcoveredRootColumn = dataFormatInstance.addInternalColumnDescription("PPCOVERED", "DEPENDENCY_EDGE_LABEL", "BOOLEAN", "", deprelColumn.getNullValueStrategy());
			this.ppcoveredRootSymbolTable = ppcoveredRootColumn.getSymbolTable();
			ppcoveredRootSymbolTable.addSymbol("#true#");
			ppcoveredRootSymbolTable.addSymbol("#false#");
		}
		if (liftingOrder.equalsIgnoreCase("shortest")) {
			this.liftingOrder = LiftingOrder.SHORTEST;
		} else if (liftingOrder.equalsIgnoreCase("deepest")) {
			this.liftingOrder = LiftingOrder.DEEPEST;
		}
	}
	
	private void initProjectivization(DependencyStructure pdg) throws MaltChainedException {
		nodeLifted.clear();
		nodeTrace.clear();
		headDeprel.clear();
		nodePath.clear();
		isCoveredRoot.clear();
		nodeRelationLength.clear();

		for (int index : pdg.getDependencyIndices()) {
			nodeLifted.add(false);
			nodeTrace.add(new Vector<DependencyNode>());
			headDeprel.add(null);
			nodePath.add(false);
			isCoveredRoot.add(false);
			if (ppliftedSymbolTable != null && index != 0) {
				pdg.getDependencyNode(index).getHeadEdge().getLabelSet().put(ppliftedSymbolTable, ppliftedSymbolTable.getSymbolStringToCode("#false#"));
			}
			if (pppathSymbolTable != null && index != 0) {
				pdg.getDependencyNode(index).getHeadEdge().getLabelSet().put(pppathSymbolTable, pppathSymbolTable.getSymbolStringToCode("#false#"));
			}
			if (ppcoveredRootSymbolTable != null && index != 0) {
				pdg.getDependencyNode(index).getHeadEdge().getLabelSet().put(ppcoveredRootSymbolTable, ppcoveredRootSymbolTable.getSymbolStringToCode("#false#"));
			}
		}
		computeRelationLength(pdg);
	}
	
    public void projectivize(DependencyStructure pdg) throws MaltChainedException {
        id++;
        if (!pdg.isTree()) {
            configLogger.info("\n[Warning: Sentence '" + id + "' cannot projectivize, because the dependency graph is not a tree]\n");
            return;
        }
        DependencyNode deepestNonProjectiveNode;
        initProjectivization(pdg);
        if (rootAttachment == CoveredRootAttachment.IGNORE) {
    		if (markingStrategy != PseudoProjectiveEncoding.NONE) {
    			while (!pdg.isProjective()) {
    				if (liftingOrder == LiftingOrder.DEEPEST) {
    					deepestNonProjectiveNode = getDeepestNonProjectiveNode(pdg);
    				} else {
    					deepestNonProjectiveNode = getShortestNonProjectiveNode(pdg);
    				}
    				if (!attachCoveredRoots(pdg, deepestNonProjectiveNode)) {
    					nodeLifted.set(deepestNonProjectiveNode.getIndex(), true);
    					setHeadDeprel(deepestNonProjectiveNode, deepestNonProjectiveNode.getHead());
    					setPath(deepestNonProjectiveNode.getHead());
    					pdg.moveDependencyEdge(pdg.getDependencyNode(deepestNonProjectiveNode.getHead().getHead().getIndex()).getIndex(), deepestNonProjectiveNode.getIndex());
    				}
    			}
    			deattachCoveredRootsForProjectivization(pdg);
    		}
        } else {
	        if (rootAttachment != CoveredRootAttachment.NONE) {
	            for (int index : pdg.getTokenIndices()) {
	                attachCoveredRoots(pdg, pdg.getTokenNode(index));
	            }
	        }
	        if (markingStrategy != PseudoProjectiveEncoding.NONE) {
	            while (!pdg.isProjective()) {
	                if (liftingOrder == LiftingOrder.DEEPEST) {
	                    deepestNonProjectiveNode = getDeepestNonProjectiveNode(pdg);
	                } else {
	                    deepestNonProjectiveNode = getShortestNonProjectiveNode(pdg);
	                }
	                nodeLifted.set(deepestNonProjectiveNode.getIndex(), true);
	                setHeadDeprel(deepestNonProjectiveNode, deepestNonProjectiveNode.getHead());
	                setPath(deepestNonProjectiveNode.getHead());
	                pdg.moveDependencyEdge(pdg.getDependencyNode(deepestNonProjectiveNode.getHead().getHead().getIndex()).getIndex(), deepestNonProjectiveNode.getIndex());
	            }
	        }
        }
        // collectTraceStatistics(pdg);
        assignPseudoProjectiveDeprels(pdg);
    }

	public void mergeArclabels(DependencyStructure pdg) throws MaltChainedException {
		assignPseudoProjectiveDeprelsForMerge(pdg);
	}

	public void splitArclabels(DependencyStructure pdg) throws MaltChainedException {
		int pathLabelIndex = -1, movedLabelIndex = -1, coveredArcLabelIndex;
		String label;
		initDeprojeciviztion(pdg);
		for (int index : pdg.getTokenIndices()) {
			if (pdg.getTokenNode(index).getHeadEdge().hasLabel(deprelSymbolTable)) {
				label = deprelSymbolTable.getSymbolCodeToString(pdg.getTokenNode(index).getHeadEdge().getLabelCode(deprelSymbolTable));
				if (label != null && (pathLabelIndex = label.indexOf("%")) != -1) {
					label = label.substring(0, pathLabelIndex);
					setLabel(pdg.getTokenNode(index), label);
					pdg.getTokenNode(index).getHeadEdge().addLabel(pppathSymbolTable, pppathSymbolTable.getSymbolStringToCode("#true#"));
				}
				if (label != null && (movedLabelIndex = label.indexOf("|")) != -1 && label.indexOf("|null") == -1) {
					if (movedLabelIndex + 1 < label.length()) {
						pdg.getTokenNode(index).getHeadEdge().addLabel(ppliftedSymbolTable, ppliftedSymbolTable.getSymbolStringToCode(label.substring(movedLabelIndex + 1)));
					} else {
						pdg.getTokenNode(index).getHeadEdge().addLabel(ppliftedSymbolTable, ppliftedSymbolTable.getSymbolStringToCode("#true#"));
					}
					label = label.substring(0, movedLabelIndex);
					setLabel(pdg.getTokenNode(index), label);
				}
			}
		}
		for (int index : pdg.getTokenIndices()) {
			if (pdg.getTokenNode(index).getHeadEdge().hasLabel(deprelSymbolTable)) {
				label = deprelSymbolTable.getSymbolCodeToString(pdg.getTokenNode(index).getHeadEdge().getLabelCode(deprelSymbolTable));
				if ((coveredArcLabelIndex = label.indexOf("|null")) != -1) {
					label = label.substring(0, coveredArcLabelIndex);
					setLabel(pdg.getTokenNode(index), label);
					pdg.getTokenNode(index).getHeadEdge().addLabel(ppcoveredRootSymbolTable, ppcoveredRootSymbolTable.getSymbolStringToCode("#true#"));
				}
			}
		}
	}

	private void setHeadDeprel(DependencyNode node, DependencyNode parent) {
		if (headDeprel.get(node.getIndex()) == null) {
			headDeprel.set(node.getIndex(), parent);
		}
		nodeTrace.set(node.getIndex(), headDeprel);
	}

	private void setPath(DependencyNode node) {
		nodePath.set(node.getIndex(), true);
	}

	private boolean isCoveredRoot(DependencyNode node) {
		return isCoveredRoot.get(node.getIndex());
	}

	private void deattachCoveredRootsForProjectivization(DependencyStructure pdg) throws MaltChainedException {
		for (int index : pdg.getTokenIndices()) {
			if (isCoveredRoot(pdg.getTokenNode(index))) {
				pdg.moveDependencyEdge(pdg.getDependencyRoot().getIndex(), pdg.getTokenNode(index).getIndex());
			}
		}
	}

	private boolean attachCoveredRoots(DependencyStructure pdg, DependencyNode deepest) throws MaltChainedException {
		int i;
		boolean foundCoveredRoot = false;
		DependencyNode coveredRootHead;
		for (i = Math.min(deepest.getIndex(), deepest.getHead().getIndex()) + 1; i < Math.max(deepest.getIndex(), deepest.getHead()
				.getIndex()); i++) {
			int leftMostIndex = pdg.getDependencyNode(i).getLeftmostProperDescendantIndex();
			if (leftMostIndex == -1) {
				leftMostIndex = i;
			}
			int rightMostIndex = pdg.getDependencyNode(i).getRightmostProperDescendantIndex();
			if (rightMostIndex == -1) {
				rightMostIndex = i;
			}
			if (!nodeLifted.get(i) && pdg.getDependencyNode(i).getHead().isRoot() && !deepest.getHead().isRoot()
					&& Math.min(deepest.getIndex(), deepest.getHead().getIndex()) < leftMostIndex
					&& rightMostIndex < Math.max(deepest.getIndex(), deepest.getHead().getIndex())) {
				if (rootAttachment == CoveredRootAttachment.LEFT) {
					if (deepest.getHead().getIndex() < deepest.getIndex()) {
						coveredRootHead = deepest.getHead();
					} else {
						coveredRootHead = deepest;
					}
				} else if (rootAttachment == CoveredRootAttachment.RIGHT) {
					if (deepest.getIndex() < deepest.getHead().getIndex()) {
						coveredRootHead = deepest.getHead();
					} else {
						coveredRootHead = deepest;
					}
				} else {
					coveredRootHead = deepest.getHead();
				}
				pdg.moveDependencyEdge(coveredRootHead.getIndex(), pdg.getDependencyNode(i).getIndex());
				setCoveredRoot(pdg.getDependencyNode(i));
				foundCoveredRoot = true;
			}
		}
		return foundCoveredRoot;
	}

	private void setCoveredRoot(DependencyNode node) {
		isCoveredRoot.set(node.getIndex(), true);
	}

	private DependencyNode getDeepestNonProjectiveNode(DependencyStructure pdg) throws MaltChainedException {
		DependencyNode deepestNonProjectiveNode = null;
		for (int index : pdg.getDependencyIndices()) {
			if (!pdg.getDependencyNode(index).isProjective()
					&& (deepestNonProjectiveNode == null 
					|| pdg.getDependencyNode(index).getDependencyNodeDepth() > pdg.getDependencyNode(deepestNonProjectiveNode.getIndex()).getDependencyNodeDepth())) {
				deepestNonProjectiveNode = pdg.getDependencyNode(index);
			}
		}
		
		return deepestNonProjectiveNode;
	}

	private DependencyNode getShortestNonProjectiveNode(DependencyStructure pdg) throws MaltChainedException {
		DependencyNode shortestNonProjectiveNode = null;
		for (int index : pdg.getDependencyIndices()) {
			if (!pdg.getDependencyNode(index).isProjective()
					&& (shortestNonProjectiveNode == null
					|| nodeRelationLength.get(index) < nodeRelationLength.get(shortestNonProjectiveNode.getIndex()) 
					)) {
//					|| (nodeRelationLength.get(index) == nodeRelationLength.get(shortestNonProjectiveNode.getIndex())))) {
				shortestNonProjectiveNode = pdg.getDependencyNode(index);
			}
		}
		return shortestNonProjectiveNode;
	}


	private void computeRelationLength(DependencyStructure pdg) throws MaltChainedException {
		nodeRelationLength.add(0);
		for (int index : pdg.getTokenIndices()) {
			nodeRelationLength.add(Math.abs(pdg.getDependencyNode(index).getIndex() - pdg.getDependencyNode(index).getHead().getIndex()));
		}
	}

	private void assignPseudoProjectiveDeprels(DependencyStructure pdg) throws MaltChainedException {
		int newLabelCode;
		for (int index : pdg.getTokenIndices()) {
			if (!isCoveredRoot(pdg.getDependencyNode(index))) {
				if (this.markingStrategy == PseudoProjectiveEncoding.HEAD || this.markingStrategy == PseudoProjectiveEncoding.PATH
						|| this.markingStrategy == PseudoProjectiveEncoding.HEADPATH) {
					if (this.markingStrategy == PseudoProjectiveEncoding.PATH) {
						if (nodeLifted.get(index)) {
							newLabelCode = ppliftedSymbolTable.getSymbolStringToCode("#true#");
						} else {
							newLabelCode = ppliftedSymbolTable.getSymbolStringToCode("#false#");
						}
						pdg.getDependencyNode(index).getHeadEdge().addLabel(ppliftedSymbolTable, newLabelCode);
					} else {
						if (nodeLifted.get(index)) {
							newLabelCode = ppliftedSymbolTable.addSymbol(deprelSymbolTable.getSymbolCodeToString(pdg.getDependencyNode(
									headDeprel.get(index).getIndex()).getHeadEdge().getLabelCode(deprelSymbolTable)));
						} else {
							newLabelCode = ppliftedSymbolTable.getSymbolStringToCode("#false#");
						}
						pdg.getDependencyNode(index).getHeadEdge().addLabel(ppliftedSymbolTable, newLabelCode);
					}
				}

				if (this.markingStrategy == PseudoProjectiveEncoding.PATH || this.markingStrategy == PseudoProjectiveEncoding.HEADPATH) {
					if (nodePath.get(index)) {
						newLabelCode = pppathSymbolTable.getSymbolStringToCode("#true#");
					} else {
						newLabelCode = pppathSymbolTable.getSymbolStringToCode("#false#");
					}
					pdg.getDependencyNode(index).getHeadEdge().addLabel(pppathSymbolTable, newLabelCode);
				}

			} else if (!(rootAttachment == CoveredRootAttachment.NONE || rootAttachment == CoveredRootAttachment.IGNORE)) {
				pdg.getDependencyNode(index).getHeadEdge().addLabel(ppcoveredRootSymbolTable, ppcoveredRootSymbolTable.getSymbolStringToCode("#true#"));
			}
		}
	}

	private void setLabel(DependencyNode node, String label) throws MaltChainedException {
		// node.getLabelCode().clear();
		node.getHeadEdge().getLabelSet().put(deprelSymbolTable, deprelSymbolTable.addSymbol(label));
	}

	private void assignPseudoProjectiveDeprelsForMerge(DependencyStructure pdg) throws MaltChainedException {
		Vector<String> originalDeprel = new Vector<String>();
		String newLabel;
		originalDeprel.add(null);
		for (int index : pdg.getTokenIndices()) {
			originalDeprel.add(deprelSymbolTable.getSymbolCodeToString(pdg.getDependencyNode(index).getHeadEdge().getLabelCode(deprelSymbolTable)));
		}
		for (int index : pdg.getTokenIndices()) {
			newLabel = null;
			if (!isCoveredRoot(pdg.getDependencyNode(index))) {
				if (markingStrategy == PseudoProjectiveEncoding.HEAD) {
					if (nodeLifted.get(index)) {
						newLabel = deprelSymbolTable.getSymbolCodeToString(pdg.getDependencyNode(index).getHeadEdge().getLabelCode(deprelSymbolTable)) + "|"
								+ originalDeprel.get(headDeprel.get(index).getIndex());
						// } else {
						// newLabel = deprelSymbolTable.getSymbolCodeToString(pdg.getDependencyNode(index).getHeadEdge().getLabelCode(deprelSymbolTable));
					}
				} else if (markingStrategy == PseudoProjectiveEncoding.PATH) {
					if (nodeLifted.get(index) && nodePath.get(index)) {
						newLabel = deprelSymbolTable.getSymbolCodeToString(pdg.getDependencyNode(index).getHeadEdge().getLabelCode(deprelSymbolTable)) + "|%";
					} else if (nodeLifted.get(index) && !nodePath.get(index)) {
						newLabel = deprelSymbolTable.getSymbolCodeToString(pdg.getDependencyNode(index).getHeadEdge().getLabelCode(deprelSymbolTable)) + "|";
					} else if (!nodeLifted.get(index) && nodePath.get(index)) {
						newLabel = deprelSymbolTable.getSymbolCodeToString(pdg.getDependencyNode(index).getHeadEdge().getLabelCode(deprelSymbolTable)) + "%";
					}
				} else if (markingStrategy == PseudoProjectiveEncoding.HEADPATH) {
					if (nodeLifted.get(index) && nodePath.get(index)) {
						newLabel = deprelSymbolTable.getSymbolCodeToString(pdg.getDependencyNode(index).getHeadEdge().getLabelCode(deprelSymbolTable)) + "|"
								+ originalDeprel.get(headDeprel.get(index).getIndex()) + "%";
					} else if (nodeLifted.get(index) && !nodePath.get(index)) {
						newLabel = deprelSymbolTable.getSymbolCodeToString(pdg.getDependencyNode(index).getHeadEdge().getLabelCode(deprelSymbolTable)) + "|"
								+ originalDeprel.get(headDeprel.get(index).getIndex());
					} else if (!nodeLifted.get(index) && nodePath.get(index)) {
						newLabel = originalDeprel.get(pdg.getDependencyNode(index).getIndex()) + "%";
					}
				} else if (markingStrategy == PseudoProjectiveEncoding.TRACE) {
					if (nodeLifted.get(index)) {
						newLabel = deprelSymbolTable.getSymbolCodeToString(pdg.getDependencyNode(index).getHeadEdge().getLabelCode(deprelSymbolTable)) + "|";
					}
				}
			} else if (!(rootAttachment == CoveredRootAttachment.NONE || rootAttachment == CoveredRootAttachment.IGNORE)) {
				newLabel = deprelSymbolTable.getSymbolCodeToString(pdg.getDependencyNode(index).getHeadEdge().getLabelCode(deprelSymbolTable)) + "|null";
			}
			if (newLabel != null) {
				setLabel(pdg.getDependencyNode(index), newLabel);
			}
		}
	}

	public void deprojectivize(DependencyStructure pdg) throws MaltChainedException {
		initDeprojeciviztion(pdg);

		for (int index : pdg.getTokenIndices()) {
			if (pdg.getDependencyNode(index).getHeadEdge().hasLabel(deprelSymbolTable)) {
				if (pdg.getDependencyNode(index).getHeadEdge().hasLabel(pppathSymbolTable)
						&& pppathSymbolTable.getSymbolCodeToString(pdg.getDependencyNode(index).getHeadEdge().getLabelCode(pppathSymbolTable)).equals("#true#")) {
					setPath(pdg.getDependencyNode(index));
				}
				if (pdg.getDependencyNode(index).getHeadEdge().hasLabel(ppliftedSymbolTable)
						&& !ppliftedSymbolTable.getSymbolCodeToString(pdg.getDependencyNode(index).getHeadEdge().getLabelCode(ppliftedSymbolTable)).equals("#false#")) {
					nodeLifted.set(index, true);
					if (!ppliftedSymbolTable.getSymbolCodeToString(pdg.getDependencyNode(index).getHeadEdge().getLabelCode(ppliftedSymbolTable)).equals("#true#")) {
						synacticHeadDeprel.set(index, ppliftedSymbolTable.getSymbolCodeToString(pdg.getDependencyNode(index).getHeadEdge()
								.getLabelCode(ppliftedSymbolTable)));
					}
				}
			}
		}
		deattachCoveredRootsForDeprojectivization(pdg);
		if (markingStrategy == PseudoProjectiveEncoding.HEAD && needsDeprojectivizeWithHead(pdg)) {
			deprojectivizeWithHead(pdg, pdg.getDependencyRoot());
		} else if (markingStrategy == PseudoProjectiveEncoding.PATH) {
			deprojectivizeWithPath(pdg, pdg.getDependencyRoot());
		} else if (markingStrategy == PseudoProjectiveEncoding.HEADPATH) {
			deprojectivizeWithHeadAndPath(pdg, pdg.getDependencyRoot());
		}
	}

	private void initDeprojeciviztion(DependencyStructure pdg) {
		nodeLifted.clear();
		nodePath.clear();
		synacticHeadDeprel.clear();
		for (int index : pdg.getDependencyIndices()) {
			nodeLifted.add(false);
			nodePath.add(false);
			synacticHeadDeprel.add(null);
		}
	}

	private void deattachCoveredRootsForDeprojectivization(DependencyStructure pdg) throws MaltChainedException {
		for (int index : pdg.getTokenIndices()) {
			if (pdg.getDependencyNode(index).getHeadEdge().hasLabel(deprelSymbolTable)) {
				if (pdg.getDependencyNode(index).getHeadEdge().hasLabel(ppcoveredRootSymbolTable)
						&& ppcoveredRootSymbolTable.getSymbolCodeToString(pdg.getDependencyNode(index).getHeadEdge().getLabelCode(ppcoveredRootSymbolTable)).equals(
								"#true#")) {
					pdg.moveDependencyEdge(pdg.getDependencyRoot().getIndex(), pdg.getDependencyNode(index).getIndex());
				}
			}
		}
	}

	// Check whether there is at least one node in the specified dependency structure that can be lifted.
	// If this is not the case, there is no need to call deprojectivizeWithHead.

	private boolean needsDeprojectivizeWithHead(DependencyStructure pdg) throws MaltChainedException {
		for (int index : pdg.getDependencyIndices()) {
			if (nodeLifted.get(index)) {
				DependencyNode node = pdg.getDependencyNode(index);
				if (breadthFirstSearchSortedByDistanceForHead(pdg, node.getHead(), node, synacticHeadDeprel.get(index)) != null) {
					return true;
				}
		    }
		}
		return false;
	}

	private boolean deprojectivizeWithHead(DependencyStructure pdg, DependencyNode node) throws MaltChainedException {
		boolean success = true, childSuccess = false;
		int i, childAttempts = 2;
		DependencyNode child, possibleSyntacticHead;
		String syntacticHeadDeprel;
		if (nodeLifted.get(node.getIndex())) {
			syntacticHeadDeprel = synacticHeadDeprel.get(node.getIndex());
			possibleSyntacticHead = breadthFirstSearchSortedByDistanceForHead(pdg, node.getHead(), node, syntacticHeadDeprel);
			if (possibleSyntacticHead != null) {
				pdg.moveDependencyEdge(possibleSyntacticHead.getIndex(), node.getIndex());
				nodeLifted.set(node.getIndex(), false);
			} else {
				success = false;
			}
		}
		while (!childSuccess && childAttempts > 0) {
			childSuccess = true;
			Vector<DependencyNode> children = new Vector<DependencyNode>();
			i = 0;
			while ((child = node.getLeftDependent(i)) != null) {
				children.add(child);
				i++;
			}
			i = 0;
			while ((child = node.getRightDependent(i)) != null) {
				children.add(child);
				i++;
			}
			for (i = 0; i < children.size(); i++) {
				child = children.get(i);
				if (!deprojectivizeWithHead(pdg, child)) {
					childSuccess = false;
				}
			}
			childAttempts--;
		}
		return childSuccess && success;
	}

	private DependencyNode breadthFirstSearchSortedByDistanceForHead(DependencyStructure dg, DependencyNode start, DependencyNode avoid, String syntacticHeadDeprel)
			throws MaltChainedException {
		DependencyNode dependent;
		String dependentDeprel;
		Vector<DependencyNode> nodes = new Vector<DependencyNode>();
		nodes.addAll(findAllDependentsVectorSortedByDistanceToPProjNode(dg, start, avoid, false));
		while (nodes.size() > 0) {
			dependent = nodes.remove(0);
			if (dependent.getHeadEdge().hasLabel(deprelSymbolTable)) {
				dependentDeprel = deprelSymbolTable.getSymbolCodeToString(dependent.getHeadEdge().getLabelCode(deprelSymbolTable));
				if (dependentDeprel.equals(syntacticHeadDeprel)) {
					return dependent;
				}
			}
			nodes.addAll(findAllDependentsVectorSortedByDistanceToPProjNode(dg, dependent, avoid, false));
		}
		return null;
	}

	
	private Vector<DependencyNode> findAllDependentsVectorSortedByDistanceToPProjNode(DependencyStructure dg, DependencyNode governor, DependencyNode avoid,
			boolean percentOnly) {
		Vector<DependencyNode> output = new Vector<DependencyNode>();
		SortedSet<DependencyNode> dependents = new TreeSet<DependencyNode>();
		dependents.addAll(governor.getLeftDependents());
		dependents.addAll(governor.getRightDependents());


		DependencyNode[] deps = new DependencyNode[dependents.size()];
		int[] distances = new int[dependents.size()];
		int i = 0;
		for (DependencyNode dep : dependents) {
			distances[i] = Math.abs(dep.getIndex() - avoid.getIndex());
			deps[i] = dep;
			i++;
		}
		if (distances.length > 1) {
			int smallest;
			int n = distances.length;
			int tmpDist;
			DependencyNode tmpDep;
			for (i=0; i < n; i++) {
				smallest = i;
				for (int j=i; j < n; j++) {
					if (distances[j] < distances[smallest]) {
						smallest = j;
					}
				}
				if (smallest != i) {
					tmpDist = distances[smallest];
					distances[smallest] = distances[i];
					distances[i] = tmpDist;
					tmpDep = deps[smallest];
					deps[smallest] = deps[i];
					deps[i] = tmpDep;
				}
			}
		}
		for (i=0; i<distances.length;i++) {
			if (deps[i] != avoid && (!percentOnly || (percentOnly && nodePath.get(deps[i].getIndex())))) {
				output.add(deps[i]);
			}
		}
		return output;
	}
	
	private Vector<DependencyNode> findAllDependentsVectorSortedByDistanceToPProjNode2(DependencyStructure dg, DependencyNode governor, DependencyNode avoid,
			boolean percentOnly) {
		int i, j;
		Vector<DependencyNode> dependents = new Vector<DependencyNode>();
		DependencyNode leftChild, rightChild;

		i = governor.getLeftDependentCount() - 1;
		j = 0;
		leftChild = governor.getLeftDependent(i--);
		rightChild = governor.getRightDependent(j++);

		while (leftChild != null && rightChild != null) {
			if (leftChild == avoid) {
				leftChild = governor.getLeftDependent(i--);
			} else if (rightChild == avoid) {
				rightChild = governor.getRightDependent(j++);
			} else if (Math.abs(leftChild.getIndex() - avoid.getIndex()) < Math.abs(rightChild.getIndex() - avoid.getIndex())) {
				if (!percentOnly || (percentOnly && nodePath.get(leftChild.getIndex()))) {
					dependents.add(leftChild);
				}
				leftChild = governor.getLeftDependent(i--);
			} else {
				if (!percentOnly || (percentOnly && nodePath.get(rightChild.getIndex()))) {
					dependents.add(rightChild);
				}
				rightChild = governor.getRightDependent(j++);
			}
		}
		while (leftChild != null) {
			if (leftChild != avoid && (!percentOnly || (percentOnly && nodePath.get(leftChild.getIndex())))) {
				dependents.add(leftChild);
			}
			leftChild = governor.getLeftDependent(i--);
		}
		while (rightChild != null) {
			if (rightChild != avoid && (!percentOnly || (percentOnly && nodePath.get(rightChild.getIndex())))) {
				dependents.add(rightChild);
			}
			rightChild = governor.getRightDependent(j++);
		}
		return dependents;
	}

	private boolean deprojectivizeWithPath(DependencyStructure pdg, DependencyNode node) throws MaltChainedException {
		boolean success = true, childSuccess = false;
		int i, childAttempts = 2;
		DependencyNode child, possibleSyntacticHead;
		if (node.hasHead() && node.getHeadEdge().isLabeled() && nodeLifted.get(node.getIndex()) && nodePath.get(node.getIndex())) {
			possibleSyntacticHead = breadthFirstSearchSortedByDistanceForPath(pdg, node.getHead(), node);
			if (possibleSyntacticHead != null) {
				pdg.moveDependencyEdge(possibleSyntacticHead.getIndex(), node.getIndex());
				nodeLifted.set(node.getIndex(), false);
			} else {
				success = false;
			}
		}
		if (node.hasHead() && node.getHeadEdge().isLabeled() && nodeLifted.get(node.getIndex())) {
			possibleSyntacticHead = breadthFirstSearchSortedByDistanceForPath(pdg, node.getHead(), node);
			if (possibleSyntacticHead != null) {
				pdg.moveDependencyEdge(possibleSyntacticHead.getIndex(), node.getIndex());
				nodeLifted.set(node.getIndex(), false);
			} else {
				success = false;
			}
		}
		while (!childSuccess && childAttempts > 0) {
			childSuccess = true;
			Vector<DependencyNode> children = new Vector<DependencyNode>();
			i = 0;
			while ((child = node.getLeftDependent(i)) != null) {
				children.add(child);
				i++;
			}
			i = 0;
			while ((child = node.getRightDependent(i)) != null) {
				children.add(child);
				i++;
			}
			for (i = 0; i < children.size(); i++) {
				child = children.get(i);
				if (!deprojectivizeWithPath(pdg, child)) {
					childSuccess = false;
				}
			}
			childAttempts--;
		}
		return childSuccess && success;
	}

	private DependencyNode breadthFirstSearchSortedByDistanceForPath(DependencyStructure dg, DependencyNode start, DependencyNode avoid) {
		DependencyNode dependent;
		Vector<DependencyNode> nodes = new Vector<DependencyNode>(), newNodes;
		nodes.addAll(findAllDependentsVectorSortedByDistanceToPProjNode(dg, start, avoid, true));
		while (nodes.size() > 0) {
			dependent = nodes.remove(0);
			if (((newNodes = findAllDependentsVectorSortedByDistanceToPProjNode(dg, dependent, avoid, true)).size()) == 0) {
				return dependent;
			}
			nodes.addAll(newNodes);
		}
		return null;
	}

	private boolean deprojectivizeWithHeadAndPath(DependencyStructure pdg, DependencyNode node) throws MaltChainedException {
		boolean success = true, childSuccess = false;
		int i, childAttempts = 2;
		DependencyNode child, possibleSyntacticHead;
		if (node.hasHead() && node.getHeadEdge().isLabeled() && nodeLifted.get(node.getIndex()) && nodePath.get(node.getIndex())) {
			possibleSyntacticHead = breadthFirstSearchSortedByDistanceForHeadAndPath(pdg, node.getHead(), node, synacticHeadDeprel.get(node
					.getIndex()));
			if (possibleSyntacticHead != null) {
				pdg.moveDependencyEdge(possibleSyntacticHead.getIndex(), node.getIndex());
				nodeLifted.set(node.getIndex(), false);
			} else {
				success = false;
			}
		}
		if (node.hasHead() && node.getHeadEdge().isLabeled() && nodeLifted.get(node.getIndex())) {
			possibleSyntacticHead = breadthFirstSearchSortedByDistanceForHeadAndPath(pdg, node.getHead(), node, synacticHeadDeprel.get(node
					.getIndex()));
			if (possibleSyntacticHead != null) {
				pdg.moveDependencyEdge(possibleSyntacticHead.getIndex(), node.getIndex());
				nodeLifted.set(node.getIndex(), false);
			} else {
				success = false;
			}
		}
		while (!childSuccess && childAttempts > 0) {
			childSuccess = true;
			Vector<DependencyNode> children = new Vector<DependencyNode>();
			i = 0;
			while ((child = node.getLeftDependent(i)) != null) {
				children.add(child);
				i++;
			}
			i = 0;
			while ((child = node.getRightDependent(i)) != null) {
				children.add(child);
				i++;
			}
			for (i = 0; i < children.size(); i++) {
				child = children.get(i);
				if (!deprojectivizeWithHeadAndPath(pdg, child)) {
					childSuccess = false;
				}
			}
			childAttempts--;
		}
		return childSuccess && success;
	}

	private DependencyNode breadthFirstSearchSortedByDistanceForHeadAndPath(DependencyStructure dg, DependencyNode start, DependencyNode avoid, String syntacticHeadDeprelCode)
			throws MaltChainedException {
		DependencyNode dependent;
		Vector<DependencyNode> nodes = new Vector<DependencyNode>(), newNodes = null, secondChance = new Vector<DependencyNode>();
		nodes.addAll(findAllDependentsVectorSortedByDistanceToPProjNode(dg, start, avoid, true));
		while (nodes.size() > 0) {
			dependent = nodes.remove(0);
			if (((newNodes = findAllDependentsVectorSortedByDistanceToPProjNode(dg, dependent, avoid, true)).size()) == 0
					&& deprelSymbolTable.getSymbolCodeToString(dependent.getHeadEdge().getLabelCode(deprelSymbolTable)).equals(syntacticHeadDeprelCode)) {
				return dependent;
			}
			nodes.addAll(newNodes);
			if (deprelSymbolTable.getSymbolCodeToString(dependent.getHeadEdge().getLabelCode(deprelSymbolTable)).equals(syntacticHeadDeprelCode)
					&& newNodes.size() != 0) {
				secondChance.add(dependent);
			}
		}
		if (secondChance.size() > 0) {
			return secondChance.firstElement();
		}
		return null;
	}
}
