package org.maltparser.core.flow.item;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.flow.FlowChartInstance;
import org.maltparser.core.flow.spec.ChartItemSpecification;
import org.maltparser.core.flow.system.elem.ChartElement;
/**
*
*
* @author Johan Hall
*/
public abstract class ChartItem {
	protected FlowChartInstance flowChartinstance;
	protected ChartItemSpecification chartItemSpecification;
	
	// Signals
	public final static int CONTINUE = 1;
	public final static int TERMINATE = 2;
	public final static int NEWITERATION = 3;
	
	public ChartItem() {  }
	
	/**
	 * Initialize the chart item
	 * 
	 * @param flowChartinstance the flow chart instance that the chart item belongs to
	 * @param chartItemSpecification a specification of the chart item
	 * @throws MaltChainedException
	 */
	public void initialize(FlowChartInstance flowChartinstance, ChartItemSpecification chartItemSpecification) throws MaltChainedException {
		setFlowChartInstance(flowChartinstance);
		setChartItemSpecification(chartItemSpecification);
	}
	
	/**
	 * Cause the chart item to perform the preprocess tasks
	 * 
	 * @param signal returned by the previous chart item
	 * @return true if every thing is ok, otherwise false
	 * @throws MaltChainedException
	 */
	public abstract int preprocess(int signal) throws MaltChainedException;
	
	/**
	 * Cause the chart item to perform the process task (for every sentence)
	 * 
	 * @param signal returned by the previous chart item
	 * @return true if it is ready to perform the next sentence, otherwise false
	 * @throws MaltChainedException
	 */
	public abstract int process(int signal) throws MaltChainedException;
	
	/**
	 * Cause the chart item to perform the postprocess tasks
	 * 
	 * @param signal returned by the previous chart item
	 * @return true if every thing is ok, otherwise false
	 * @throws MaltChainedException
	 */
	public abstract int postprocess(int signal) throws MaltChainedException;
	
	/**
	 * Terminates and cleans up the chart item
	 * 
	 * @throws MaltChainedException
	 */
	public abstract void terminate() throws MaltChainedException;

	/**
	 * Returns the flow chart instance that the chart item belongs to
	 * 
	 * @return the flow chart instance that the chart item belongs to
	 */
	public FlowChartInstance getFlowChartInstance() {
		return flowChartinstance;
	}

	/**
	 * Sets the flow chart instance that the chart item belongs to
	 * 
	 * @param flowChartinstance a flow chart instance
	 */
	protected void setFlowChartInstance(FlowChartInstance flowChartinstance) {
		this.flowChartinstance = flowChartinstance;
	}

	/**
	 * Returns the option container index
	 * 
	 * @return the option container index
	 */
	public int getOptionContainerIndex() {
		return flowChartinstance.getOptionContainerIndex();
	}

	/**
	 * Returns the chart element in the flow chart system description
	 * 
	 * @param key a chart element key
	 * @return the chart element in the flow chart system description
	 */
	public ChartElement getChartElement(String key) {
		return flowChartinstance.getFlowChartManager().getFlowChartSystem().getChartElement(key);
	}
	
	/**
	 * Returns a chart item specification
	 * 
	 * @return a chart item specification
	 */
	public ChartItemSpecification getChartItemSpecification() {
		return chartItemSpecification;
	}

	/**
	 * Sets the specification of the chart item
	 * 
	 * @param chartItemSpecification a chart item specification
	 */
	public void setChartItemSpecification(ChartItemSpecification chartItemSpecification) {
		this.chartItemSpecification = chartItemSpecification;
	}
}
