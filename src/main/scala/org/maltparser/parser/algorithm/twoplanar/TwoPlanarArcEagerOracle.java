package org.maltparser.parser.algorithm.twoplanar;

import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.syntaxgraph.DependencyStructure;
import org.maltparser.core.syntaxgraph.edge.Edge;
import org.maltparser.core.syntaxgraph.node.DependencyNode;
import org.maltparser.parser.DependencyParserConfig;
import org.maltparser.parser.Oracle;
import org.maltparser.parser.ParserConfiguration;
import org.maltparser.parser.history.GuideUserHistory;
import org.maltparser.parser.history.action.GuideUserAction;
/**
 * @author Carlos Gomez Rodriguez
 *
 */
public class TwoPlanarArcEagerOracle extends Oracle {

	public TwoPlanarArcEagerOracle(DependencyParserConfig manager, GuideUserHistory history) throws MaltChainedException {
		super(manager, history);
		setGuideName("Two-Planar");
	}
	
	/**
	 * Give this map an edge in the gold standard data, and it will tell you whether, given the links already created by the oracle, it is possible 
	 * to create the corresponding edge in one of the planes, in any plane, or in none at all (the latter will happen in non-2-planar structures). 
	 */
	private static final int ANY_PLANE = 0;
	private static final int FIRST_PLANE = 1;
	private static final int SECOND_PLANE = 2;
	private static final int NO_PLANE = 3;
	private Map<Edge,Integer> linksToPlanes = new IdentityHashMap<Edge,Integer>();
	
	public GuideUserAction predict(DependencyStructure gold, ParserConfiguration config) throws MaltChainedException {
		TwoPlanarConfig planarConfig = (TwoPlanarConfig)config;
		DependencyStructure dg = planarConfig.getDependencyGraph();
		DependencyNode activeStackPeek = planarConfig.getActiveStack().peek();
		DependencyNode inactiveStackPeek = planarConfig.getInactiveStack().peek();
		int activeStackPeekIndex = activeStackPeek.getIndex();
		int inactiveStackPeekIndex = inactiveStackPeek.getIndex();
		int inputPeekIndex = planarConfig.getInput().peek().getIndex();
		
		//System.out.println("Initting crossings");
		
		if ( crossingsGraph == null ) initCrossingsGraph(gold);
		
		//System.out.println("Crossings initted");
		
		if (!activeStackPeek.isRoot() && gold.getTokenNode(activeStackPeekIndex).getHead().getIndex() == inputPeekIndex
				&& !checkIfArcExists ( dg , inputPeekIndex , activeStackPeekIndex ) )  {
			if ( planarConfig.getStackActivityState() == TwoPlanarConfig.FIRST_STACK )
			{
				propagatePlaneConstraint(gold.getTokenNode(activeStackPeekIndex).getHeadEdge(), FIRST_PLANE );
			}
			else
			{
				propagatePlaneConstraint(gold.getTokenNode(activeStackPeekIndex).getHeadEdge(), SECOND_PLANE );
			}
			//System.out.println("From " + inputPeekIndex + " to " + activeStackPeekIndex);
			return updateActionContainers(TwoPlanar.LEFTARC, gold.getTokenNode(activeStackPeekIndex).getHeadEdge().getLabelSet());	
		} 
		
		else if (gold.getTokenNode(inputPeekIndex).getHead().getIndex() == activeStackPeekIndex
				&& !checkIfArcExists ( dg , activeStackPeekIndex , inputPeekIndex ) ) {
			if ( planarConfig.getStackActivityState() == TwoPlanarConfig.FIRST_STACK )
			{
				propagatePlaneConstraint(gold.getTokenNode(inputPeekIndex).getHeadEdge(), FIRST_PLANE );
			}
			else
			{
				propagatePlaneConstraint(gold.getTokenNode(inputPeekIndex).getHeadEdge(), SECOND_PLANE );
			}
			//System.out.println("From " + activeStackPeekIndex + " to " + inputPeekIndex);
			return updateActionContainers(TwoPlanar.RIGHTARC, gold.getTokenNode(inputPeekIndex).getHeadEdge().getLabelSet());
		}
		
		else if (!inactiveStackPeek.isRoot() && gold.getTokenNode(inactiveStackPeekIndex).getHead().getIndex() == inputPeekIndex
				&& !checkIfArcExists ( dg , inputPeekIndex , inactiveStackPeekIndex ) )  {
			//need to create link, but on the other plane!!
			//TODO is this if branch really necessary? i.e. will this happen? (later branches already switch)
			//System.out.println("Switch one");
			return updateActionContainers(TwoPlanar.SWITCH, null);
		}
		
		else if (gold.getTokenNode(inputPeekIndex).getHead().getIndex() == inactiveStackPeekIndex
				&& !checkIfArcExists ( dg , inactiveStackPeekIndex , inputPeekIndex ) ) {
			//need to create link, but on the other plane!!
			//TODO is this if branch really necessary? i.e. will this happen? (later branches already switch)
			//System.out.println("Switch two");
			return updateActionContainers(TwoPlanar.SWITCH, null);
		}
		
		else if ( getFirstPendingLinkOnActivePlane(planarConfig,gold) != null )
		{
			//System.out.println("Reduce one");
			return updateActionContainers(TwoPlanar.REDUCE, null);
		}
		
		else if ( getFirstPendingLinkOnInactivePlane(planarConfig,gold) != null )
		{
			//System.out.println("Switch for reducing");
			return updateActionContainers(TwoPlanar.SWITCH, null);
		}
		
		//TODO: double reduce somehow? (check if reduced node is not covered by links of the other plane, or something like that).
	
		else 
		{
			//System.out.println("Shift");
			return updateActionContainers(TwoPlanar.SHIFT, null);
		}
		
	}
	
	private boolean checkIfArcExists ( DependencyStructure dg , int index1 , int index2 ) throws MaltChainedException
	{
		return dg.getTokenNode(index2).hasHead() && dg.getTokenNode(index2).getHead().getIndex() == index1;
	}
	
	public void finalizeSentence(DependencyStructure dependencyGraph) throws MaltChainedException {
		crossingsGraph = null;
		linksToPlanes.clear();
	}
	
	public void terminate() throws MaltChainedException {}

	
	
	private static boolean cross ( Edge e1 , Edge e2 )
	{
		int xSource = e1.getSource().getIndex();
		int xTarget = e1.getTarget().getIndex();
		int ySource = e2.getSource().getIndex();
		int yTarget = e2.getTarget().getIndex();
		int xMin = Math.min(xSource,xTarget);
		int xMax = Math.max(xSource,xTarget);
		int yMin = Math.min(ySource,yTarget);
		int yMax = Math.max(ySource,yTarget);
		//System.out.println(xMin+":"+xMax+":"+yMin+":"+yMax);
		return ( xMin < yMin && yMin < xMax && xMax < yMax ) || ( yMin < xMin && xMin < yMax && yMax < xMax );
	}
	
	private Map<Edge,List<Edge>> crossingsGraph = null;
	
	private void initCrossingsGraph ( DependencyStructure dg )
	{
		crossingsGraph = new IdentityHashMap<Edge,List<Edge>>();
		SortedSet<Edge> edges = dg.getEdges();
		//System.out.println(edges.size());
		//System.out.println(dg.nEdges());
		for (Iterator<Edge> iterator1 = edges.iterator(); iterator1.hasNext();) {
			Edge edge1 = iterator1.next();
			for (Iterator<Edge> iterator2 = edges.iterator(); iterator2.hasNext();) {
				Edge edge2 = iterator2.next();
				if ( edge1.getSource().getIndex() < edge2.getSource().getIndex() && cross(edge1,edge2) )
				{
					//System.out.println("Crossing!");
					List<Edge> crossingEdge1 = crossingsGraph.get(edge1);
					if ( crossingEdge1 == null ) { crossingEdge1 = new LinkedList<Edge>(); crossingsGraph.put(edge1, crossingEdge1); }
					crossingEdge1.add(edge2);
					List<Edge> crossingEdge2 = crossingsGraph.get(edge2);
					if ( crossingEdge2 == null ) { crossingEdge2 = new LinkedList<Edge>(); crossingsGraph.put(edge2 , crossingEdge2); }
					crossingEdge2.add(edge1);
				}
			}
		}
	}
	
	private List<Edge> getCrossingEdges ( Edge e )
	{
		return crossingsGraph.get(e);
	}
	
	private void setPlaneConstraint ( Edge e , int requiredPlane )
	{
		linksToPlanes.put(e, requiredPlane);
	}
	
	private int getPlaneConstraint ( Edge e )
	{
		Integer constr = linksToPlanes.get(e);
		if ( constr == null )
		{
			setPlaneConstraint(e,ANY_PLANE);
			return ANY_PLANE;
		}
		else return constr;
	}
	
	private void propagatePlaneConstraint ( Edge e , int requiredPlane )
	{
		setPlaneConstraint(e,requiredPlane);
		if ( requiredPlane == FIRST_PLANE || requiredPlane == SECOND_PLANE )
		{
			List<Edge> crossingEdges = getCrossingEdges(e);
			if ( crossingEdges != null )
			{
				for (Iterator<Edge> iterator = crossingEdges.iterator(); iterator.hasNext();) 
				{
					Edge crossingEdge = iterator.next();
					assert ( requiredPlane == FIRST_PLANE || requiredPlane == SECOND_PLANE );
					int crossingEdgeConstraint = getPlaneConstraint(crossingEdge);
					if ( crossingEdgeConstraint == ANY_PLANE ) 
					{ 
						if ( requiredPlane == FIRST_PLANE )
							propagatePlaneConstraint(crossingEdge,SECOND_PLANE);
						else if ( requiredPlane == SECOND_PLANE )
							propagatePlaneConstraint(crossingEdge,FIRST_PLANE);
					}
					else if ( crossingEdgeConstraint == NO_PLANE ) ;		
					else if ( crossingEdgeConstraint == FIRST_PLANE )
					{
						if ( requiredPlane == FIRST_PLANE )
							propagatePlaneConstraint(crossingEdge,NO_PLANE);
					}
					else if ( crossingEdgeConstraint == SECOND_PLANE )
					{
						if ( requiredPlane == SECOND_PLANE )
							propagatePlaneConstraint(crossingEdge,NO_PLANE);
					}
				}
			}
		}
	}

	/**
	 * Decides in which plane link e should be created.
	 */
	private int getLinkDecision ( Edge e , TwoPlanarConfig config )
	{
		int constraint = getPlaneConstraint ( e );
		if ( constraint == ANY_PLANE )
		{
			//choose active plane
			if ( config.getStackActivityState() == TwoPlanarConfig.FIRST_STACK )
				return FIRST_PLANE;
			else
				return SECOND_PLANE;
		}
		else return constraint;
	}
	
	
	/**
	 * Gets the shortest pending link between (to or from) the input node and a node to the left of the top of the active stack,
	 * such that the link can be established on the active plane.
	 * @param config
	 * @return
	 */
	private Edge getFirstPendingLinkOnActivePlane ( TwoPlanarConfig config , DependencyStructure gold ) throws MaltChainedException
	{
		return getFirstPendingLinkOnPlane ( config , gold , config.getStackActivityState() == TwoPlanarConfig.FIRST_STACK ? FIRST_PLANE : SECOND_PLANE , 
				config.getActiveStack().peek().getIndex() );
	}
	
	/**
	 * Gets the shortest pending link between (to or from) the input node and a node to the left of the top of the inactive stack, 
	 * such that the link can be established on the inactive plane.
	 * @param config
	 * @return
	 */
	private Edge getFirstPendingLinkOnInactivePlane ( TwoPlanarConfig config , DependencyStructure gold ) throws MaltChainedException
	{
		return getFirstPendingLinkOnPlane ( config , gold , config.getStackActivityState() == TwoPlanarConfig.FIRST_STACK ? SECOND_PLANE : FIRST_PLANE ,
				config.getInactiveStack().peek().getIndex() );
	}
	
	private Edge getFirstPendingLinkOnAnyPlane ( TwoPlanarConfig config , DependencyStructure gold ) throws MaltChainedException
	{
		Edge e1 = getFirstPendingLinkOnActivePlane ( config , gold );
		Edge e2 = getFirstPendingLinkOnInactivePlane ( config , gold );
		int left1 = Math.min(e1.getSource().getIndex(), e1.getTarget().getIndex());
		int left2 = Math.min(e2.getSource().getIndex(), e2.getTarget().getIndex());
		if ( left1 > left2 ) return e1;
		else return e2;
	}
	
	/**
	 * Gets the shortest pending link between (to or from) the input node and a node to the left of rightmostLimit, such that the link
	 * can be established on the given plane.
	 * @param config
	 * @param plane
	 * @param rightmostLimit
	 * @return
	 */
	private Edge getFirstPendingLinkOnPlane ( TwoPlanarConfig config , DependencyStructure gold ,  int plane , int rightmostLimit )
		throws MaltChainedException
	{
		TwoPlanarConfig planarConfig = (TwoPlanarConfig)config;
		//DependencyStructure dg = planarConfig.getDependencyGraph(); -> no need, if rightmostLimit is well chosen, due to algorithm invariants
		int inputPeekIndex = planarConfig.getInput().peek().getIndex();
		
		Edge current = null;
		int maxIndex;
		if ( planarConfig.getRootHandling() == TwoPlanarConfig.NORMAL )
			maxIndex = -1; //count links from dummy root
		else
			maxIndex = 0; //do not count links from dummy root
		
		if ( gold.getTokenNode(inputPeekIndex).hasLeftDependent() && 
				gold.getTokenNode(inputPeekIndex).getLeftmostDependent().getIndex() < rightmostLimit)
		{
			SortedSet<DependencyNode> dependents = gold.getTokenNode(inputPeekIndex).getLeftDependents();
			for (Iterator<DependencyNode> iterator = dependents.iterator(); iterator.hasNext();) {
				DependencyNode dependent = (DependencyNode) iterator.next();
				if ( dependent.getIndex() > maxIndex && dependent.getIndex() < rightmostLimit 
						&& getLinkDecision(dependent.getHeadEdge(),config) == plane )
				{
					maxIndex = dependent.getIndex();
					current = dependent.getHeadEdge();
				}
			}
		}
		
		//at this point, current is the first left-pointing link, but we have to check right-pointing link as well
		
		//System.out.println("in" + inputPeekIndex + " rl" + rightmostLimit);
		if ( gold.getTokenNode(inputPeekIndex).getHead().getIndex() < rightmostLimit )
		{
			//System.out.println(":");
			if ( gold.getTokenNode(inputPeekIndex).getHead().getIndex() > maxIndex && 
					getLinkDecision(gold.getTokenNode(inputPeekIndex).getHeadEdge(),config) == plane )
			{
				//System.out.println("::");
				current = gold.getTokenNode(inputPeekIndex).getHeadEdge();
			}
		}
		
		return current;
		
		
	}
	

}
