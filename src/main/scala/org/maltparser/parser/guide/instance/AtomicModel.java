package org.maltparser.parser.guide.instance;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Formatter;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.FeatureVector;
import org.maltparser.core.feature.function.FeatureFunction;
import org.maltparser.core.feature.function.Modifiable;
import org.maltparser.core.syntaxgraph.DependencyStructure;
import org.maltparser.ml.LearningMethod;
import org.maltparser.parser.guide.ClassifierGuide;
import org.maltparser.parser.guide.GuideException;
import org.maltparser.parser.guide.Model;
import org.maltparser.parser.history.action.SingleDecision;


/**

@author Johan Hall
@since 1.0
*/
public class AtomicModel implements InstanceModel {
	private Model parent;
	private String modelName;
	private FeatureVector featureVector;
	private int index;
	private int frequency = 0;
	private LearningMethod method;

	
	/**
	 * Constructs an atomic model.
	 * 
	 * @param index the index of the atomic model (-1..n), where -1 is special value (used by a single model 
	 * or the master divide model) and n is number of divide models.
	 * @param features the feature vector used by the atomic model.
	 * @param parent the parent guide model.
	 * @throws MaltChainedException
	 */
	public AtomicModel(int index, FeatureVector features, Model parent) throws MaltChainedException {
		setParent(parent);
		setIndex(index);
		if (index == -1) {
			setModelName(parent.getModelName()+".");
		} else {
			setModelName(parent.getModelName()+"."+new Formatter().format("%03d", index)+".");
		}
		setFeatures(features);
		setFrequency(0);
		initMethod();
		if (getGuide().getGuideMode() == ClassifierGuide.GuideMode.BATCH && index == -1 && getGuide().getConfiguration().getConfigurationDir().getInfoFileWriter() != null) {
			try {
				getGuide().getConfiguration().getConfigurationDir().getInfoFileWriter().write(method.toString());
				getGuide().getConfiguration().getConfigurationDir().getInfoFileWriter().flush();
			} catch (IOException e) {
				throw new GuideException("Could not write learner settings to the information file. ", e);
			}
		}
	}
	
	public void addInstance(SingleDecision decision) throws MaltChainedException {
		try {
			method.addInstance(decision, featureVector);
		} catch (NullPointerException e) {
			throw new GuideException("The learner cannot be found. ", e);
		}
	}

	
	public void noMoreInstances() throws MaltChainedException {
		try {
			method.noMoreInstances();
		} catch (NullPointerException e) {
			throw new GuideException("The learner cannot be found. ", e);
		}
	}
	
	public void finalizeSentence(DependencyStructure dependencyGraph) throws MaltChainedException {
		try {
			method.finalizeSentence(dependencyGraph);
		} catch (NullPointerException e) {
			throw new GuideException("The learner cannot be found. ", e);
		}
	}

	public boolean predict(SingleDecision decision) throws MaltChainedException {
		try {
//			if (getGuide().getGuideMode() == ClassifierGuide.GuideMode.BATCH) {
//				throw new GuideException("Cannot predict during batch training. ");
//			}
			return method.predict(featureVector, decision);
		} catch (NullPointerException e) {
			throw new GuideException("The learner cannot be found. ", e);
		}
	}

	public FeatureVector predictExtract(SingleDecision decision) throws MaltChainedException {
		try {
//			if (getGuide().getGuideMode() == ClassifierGuide.GuideMode.BATCH) {
//				throw new GuideException("Cannot predict during batch training. ");
//			}
			if (method.predict(featureVector, decision)) {
				return featureVector;
			}
			return null;
		} catch (NullPointerException e) {
			throw new GuideException("The learner cannot be found. ", e);
		}
	}
	
	public FeatureVector extract() throws MaltChainedException {
		return featureVector;
	}
	
	public void terminate() throws MaltChainedException {
		if (method != null) {
			method.terminate();
			method = null;
		}
		featureVector = null;
		parent = null;
	}
	
	/**
	 * Moves all instance from this atomic model into the destination atomic model and add the divide feature.
	 * This method is used by the feature divide model to sum up all model below a certain threshold.
	 * 
	 * @param model the destination atomic model 
	 * @param divideFeature the divide feature
	 * @param divideFeatureIndexVector the divide feature index vector
	 * @throws MaltChainedException
	 */
	public void moveAllInstances(AtomicModel model, FeatureFunction divideFeature, ArrayList<Integer> divideFeatureIndexVector) throws MaltChainedException {
		if (method == null) {
			throw new GuideException("The learner cannot be found. ");
		} else if (model == null) {
			throw new GuideException("The guide model cannot be found. ");
		} else if (divideFeature == null) {
			throw new GuideException("The divide feature cannot be found. ");
		} else if (divideFeatureIndexVector == null) {
			throw new GuideException("The divide feature index vector cannot be found. ");
		}
		((Modifiable)divideFeature).setFeatureValue(index);
		method.moveAllInstances(model.getMethod(), divideFeature, divideFeatureIndexVector);
		method.terminate();
		method = null;
	}
	
	/**
	 * Invokes the train() of the learning method 
	 * 
	 * @throws MaltChainedException
	 */
	public void train() throws MaltChainedException {
		try {
			method.train(featureVector);
			method.terminate();
			method = null;
			
		} catch (NullPointerException e) {	
			throw new GuideException("The learner cannot be found. ", e);
		}
		

	}
	
	/**
	 * Initialize the learning method according to the option --learner-method.
	 * 
	 * @throws MaltChainedException
	 */
	public void initMethod() throws MaltChainedException {
		Class<?> clazz = (Class<?>)getGuide().getConfiguration().getOptionValue("guide", "learner");
		Class<?>[] argTypes = { org.maltparser.parser.guide.instance.InstanceModel.class, java.lang.Integer.class };
		Object[] arguments = new Object[2];
		arguments[0] = this;
		if (getGuide().getGuideMode() == ClassifierGuide.GuideMode.CLASSIFY) {
			arguments[1] = LearningMethod.CLASSIFY;
		} else if (getGuide().getGuideMode() == ClassifierGuide.GuideMode.BATCH) {
			arguments[1] = LearningMethod.BATCH;
		} 

		try {	
			Constructor<?> constructor = clazz.getConstructor(argTypes);
			this.method = (LearningMethod)constructor.newInstance(arguments);
		} catch (NoSuchMethodException e) {
			throw new GuideException("The learner class '"+clazz.getName()+"' cannot be initialized. ", e);
		} catch (InstantiationException e) {
			throw new GuideException("The learner class '"+clazz.getName()+"' cannot be initialized. ", e);
		} catch (IllegalAccessException e) {
			throw new GuideException("The learner class '"+clazz.getName()+"' cannot be initialized. ", e);
		} catch (InvocationTargetException e) {
			throw new GuideException("The learner class '"+clazz.getName()+"' cannot be initialized. ", e);
		}
	}
	
	
	
	/**
	 * Returns the parent guide model
	 * 
	 * @return the parent guide model
	 */
	public Model getParent() throws MaltChainedException {
		if (parent == null) {
			throw new GuideException("The atomic model can only be used by a parent model. ");
		}
		return parent;
	}

	/**
	 * Sets the parent guide model
	 * 
	 * @param parent the parent guide model
	 */
	protected void setParent(Model parent) {
		this.parent = parent;
	}

	public String getModelName() {
		return modelName;
	}

	/**
	 * Sets the name of the atomic model
	 * 
	 * @param modelName the name of the atomic model
	 */
	protected void setModelName(String modelName) {
		this.modelName = modelName;
	}

	/**
	 * Returns the feature vector used by this atomic model
	 * 
	 * @return a feature vector object
	 */
	public FeatureVector getFeatures() {
		return featureVector;
	}

	/**
	 * Sets the feature vector used by the atomic model.
	 * 
	 * @param features a feature vector object
	 */
	protected void setFeatures(FeatureVector features) {
		this.featureVector = features;
	}

	public ClassifierGuide getGuide() {
		return parent.getGuide();
	}
	
	/**
	 * Returns the index of the atomic model
	 * 
	 * @return the index of the atomic model
	 */
	public int getIndex() {
		return index;
	}

	/**
	 * Sets the index of the model (-1..n), where -1 is a special value.
	 * 
	 * @param index index value (-1..n) of the atomic model
	 */
	protected void setIndex(int index) {
		this.index = index;
	}

	/**
	 * Returns the frequency (number of instances)
	 * 
	 * @return the frequency (number of instances)
	 */
	public int getFrequency() {
		return frequency;
	}
	
	/**
	 * Increase the frequency by 1
	 */
	public void increaseFrequency() {
		if (parent instanceof InstanceModel) {
			((InstanceModel)parent).increaseFrequency();
		}
		frequency++;
	}
	
	public void decreaseFrequency() {
		if (parent instanceof InstanceModel) {
			((InstanceModel)parent).decreaseFrequency();
		}
		frequency--;
	}
	/**
	 * Sets the frequency (number of instances)
	 * 
	 * @param frequency (number of instances)
	 */
	protected void setFrequency(int frequency) {
		this.frequency = frequency;
	} 
	
	/**
	 * Returns a learner object
	 * 
	 * @return a learner object
	 */
	public LearningMethod getMethod() {
		return method;
	}
	
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(method.toString());
		return sb.toString();
	}
}
