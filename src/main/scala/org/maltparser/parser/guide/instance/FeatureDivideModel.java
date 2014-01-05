package org.maltparser.parser.guide.instance;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.SortedMap;

import java.util.ArrayList;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.regex.Pattern;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.FeatureException;
import org.maltparser.core.feature.FeatureVector;
import org.maltparser.core.feature.function.FeatureFunction;
import org.maltparser.core.feature.function.Modifiable;
import org.maltparser.core.feature.value.SingleFeatureValue;
import org.maltparser.core.syntaxgraph.DependencyStructure;
import org.maltparser.parser.guide.ClassifierGuide;
import org.maltparser.parser.guide.GuideException;
import org.maltparser.parser.guide.Model;
import org.maltparser.parser.history.action.SingleDecision;

/**
The feature divide model is used for divide the training instances into several models according to
a divide feature. Usually this strategy decrease the training and classification time, but can also decrease 
the accuracy of the parser.  

@author Johan Hall
@since 1.0
*/
public class FeatureDivideModel implements InstanceModel {
	private Model parent;
	private final SortedMap<Integer,AtomicModel> divideModels;
	private FeatureVector masterFeatureVector;
	private FeatureVector divideFeatureVector;
	private int frequency = 0;
	private FeatureFunction divideFeature;
	private int divideThreshold;
	private AtomicModel masterModel;
	private ArrayList<Integer> divideFeatureIndexVector;
	
	/**
	 * Constructs a feature divide model.
	 * 
	 * @param features the feature vector used by the atomic model.
	 * @param parent the parent guide model.
	 * @throws MaltChainedException
	 */
	public FeatureDivideModel(FeatureVector features, Model parent) throws MaltChainedException {
		setParent(parent);
		setFrequency(0);
		initSplitParam(features);
		divideModels = new TreeMap<Integer,AtomicModel>();
		if (getGuide().getGuideMode() == ClassifierGuide.GuideMode.BATCH) {
			masterModel = new AtomicModel(-1, masterFeatureVector, this);
		} else if (getGuide().getGuideMode() == ClassifierGuide.GuideMode.CLASSIFY) {
			load();
		}
	}
	
	public void addInstance(SingleDecision decision) throws MaltChainedException {
		if (getGuide().getGuideMode() == ClassifierGuide.GuideMode.CLASSIFY) {
			throw new GuideException("Can only add instance during learning. ");
		} else if (!(divideFeature.getFeatureValue() instanceof SingleFeatureValue)) {
			throw new GuideException("The divide feature does not have a single value. ");
		}
		
		divideFeature.update();
		if (divideModels != null) { 
			if (!divideModels.containsKey(((SingleFeatureValue)divideFeature.getFeatureValue()).getIndexCode())) {
				divideModels.put(((SingleFeatureValue)divideFeature.getFeatureValue()).getIndexCode(), new AtomicModel(((SingleFeatureValue)divideFeature.getFeatureValue()).getIndexCode(), divideFeatureVector, this));
			}
			divideModels.get(((SingleFeatureValue)divideFeature.getFeatureValue()).getIndexCode()).addInstance(decision);
		} else {
			throw new GuideException("The feature divide models cannot be found. ");
		}
	}
	
	public void noMoreInstances() throws MaltChainedException {
//		if (getGuide().getGuideMode() == Guide.GuideMode.CLASSIFY) {
//			throw new GuideException("Can only finish all data during learning. ");
//		}
		
		if (divideModels != null) {
//			divideFeature.updateCardinality();
			for (Integer index : divideModels.keySet()) {
				divideModels.get(index).noMoreInstances();
			}
			final TreeSet<Integer> removeSet = new TreeSet<Integer>();
			for (Integer index : divideModels.keySet()) {
				if (divideModels.get(index).getFrequency() <= divideThreshold) {
					divideModels.get(index).moveAllInstances(masterModel, divideFeature, divideFeatureIndexVector);
					removeSet.add(index);
				}
			}
			for (Integer index : removeSet) {
				divideModels.remove(index);
			}
			masterModel.noMoreInstances();

		} else {
			throw new GuideException("The feature divide models cannot be found. ");
		}
	}

	public void finalizeSentence(DependencyStructure dependencyGraph) throws MaltChainedException {
//		if (getGuide().getGuideMode() == Guide.GuideMode.CLASSIFY) {
//			throw new GuideException("Can only finish sentence during learning. ");
//		}

		if (divideModels != null) { 
			for (AtomicModel divideModel : divideModels.values()) {
				divideModel.finalizeSentence(dependencyGraph);
			}
		} else {
			throw new GuideException("The feature divide models cannot be found. ");
		}
	}

	public boolean predict(SingleDecision decision) throws MaltChainedException {
		if (getGuide().getGuideMode() == ClassifierGuide.GuideMode.BATCH) {
			throw new GuideException("Can only predict during parsing. ");
		} else if (!(divideFeature.getFeatureValue() instanceof SingleFeatureValue)) {
			throw new GuideException("The divide feature does not have a single value. ");
		}
		
		//divideFeature.update();
		if (divideModels != null && divideModels.containsKey(((SingleFeatureValue)divideFeature.getFeatureValue()).getIndexCode())) {
			return divideModels.get(((SingleFeatureValue)divideFeature.getFeatureValue()).getIndexCode()).predict(decision);
		} else if (masterModel != null && masterModel.getFrequency() > 0) {
			return masterModel.predict(decision);
		} else {
			getGuide().getConfiguration().getConfigLogger().info("Could not predict the next parser decision because there is " +
					"no divide or master model that covers the divide value '"+((SingleFeatureValue)divideFeature.getFeatureValue()).getIndexCode()+"', as default" +
							" class code '1' is used. ");
			
			decision.addDecision(1); // default prediction
			//classCodeTable.getEmptyKBestList().addKBestItem(1); 
		}
		return true;
	}

	public FeatureVector predictExtract(SingleDecision decision) throws MaltChainedException {
		return getAtomicModel().predictExtract(decision);
	}
	
	public FeatureVector extract() throws MaltChainedException {
		return getAtomicModel().extract();
	}
	
	private AtomicModel getAtomicModel() throws MaltChainedException {
		if (getGuide().getGuideMode() == ClassifierGuide.GuideMode.BATCH) {
			throw new GuideException("Can only predict during parsing. ");
		} else if (!(divideFeature.getFeatureValue() instanceof SingleFeatureValue)) {
			throw new GuideException("The divide feature does not have a single value. ");
		}
		
		if (divideModels != null && divideModels.containsKey(((SingleFeatureValue)divideFeature.getFeatureValue()).getIndexCode())) {
			return divideModels.get(((SingleFeatureValue)divideFeature.getFeatureValue()).getIndexCode());
		} else if (masterModel != null && masterModel.getFrequency() > 0) {
			return masterModel;
		} else {
			getGuide().getConfiguration().getConfigLogger().info("Could not predict the next parser decision because there is " +
					"no divide or master model that covers the divide value '"+((SingleFeatureValue)divideFeature.getFeatureValue()).getIndexCode()+"', as default" +
							" class code '1' is used. ");
		}
		return null;
	}
	
	public void terminate() throws MaltChainedException {
		if (divideModels != null) {
			for (AtomicModel divideModel : divideModels.values()) {	
				divideModel.terminate();
			}
		}
		if (masterModel != null) {
			masterModel.terminate();
		}
	}
	
	public void train() throws MaltChainedException {
		for (AtomicModel divideModel : divideModels.values()) {
			divideModel.train();
		}
		masterModel.train();
		save();
		for (AtomicModel divideModel : divideModels.values()) {
			divideModel.terminate();
		}
		masterModel.terminate();
	}
	
	/**
	 * Initialize the feature split parameters and the split feature vector and master feature vector
	 * according to the behavior strategy.
	 * 
	 * @param featureVector the parent guide model's feature vector.
	 * @throws MaltChainedException
	 */
	protected void initSplitParam(FeatureVector featureVector) throws MaltChainedException {
		if (getGuide().getConfiguration().getOptionValue("guide", "data_split_column") == null 
				|| getGuide().getConfiguration().getOptionValue("guide", "data_split_column").toString().length() == 0) {
			throw new GuideException("The option '--guide-data_split_column' cannot be found, when initializing the data split. ");
		}
		if (getGuide().getConfiguration().getOptionValue("guide", "data_split_structure") == null 
				|| getGuide().getConfiguration().getOptionValue("guide", "data_split_structure").toString().length() == 0) {
			throw new GuideException("The option '--guide-data_split_structure' cannot be found, when initializing the data split. ");
		}
		try {
			final String spec = "InputColumn(" + getGuide().getConfiguration().getOptionValue("guide", "data_split_column").toString().trim()+
							", "+getGuide().getConfiguration().getOptionValue("guide", "data_split_structure").toString().trim() +")";
			divideFeature = featureVector.getFeatureModel().identifyFeature(spec);
		} catch (FeatureException e) {
			throw new GuideException("The data split feature 'InputColumn("+getGuide().getConfiguration().getOptionValue("guide", "data_split_column").toString()+", "+getGuide().getConfiguration().getOptionValue("guide", "data_split_structure").toString()+") cannot be initialized. ", e);
		}
		if (!(divideFeature instanceof Modifiable)) {
			throw new GuideException("The data split feature 'InputColumn("+getGuide().getConfiguration().getOptionValue("guide", "data_split_column").toString()+", "+getGuide().getConfiguration().getOptionValue("guide", "data_split_structure").toString()+") does not implement Modifiable interface. ");
		}
		divideFeatureIndexVector = new ArrayList<Integer>();
		for (int i = 0; i < featureVector.size(); i++) {
			if (featureVector.get(i).equals(divideFeature)) {
				divideFeatureIndexVector.add(i);
			}
		}
		
//		if ((Boolean)getGuide().getConfiguration().getOptionValue("malt0.4", "behavior") == true) {
//			/* MaltParser 0.4 removes the divide feature for all divide models. For the "Sum-up" model or
//			 * master model adds the divide feature in the end of the feature vector.
//			 */
//			masterFeatureVector = (FeatureVector)featureVector.clone();
//			for (Integer i : divideFeatureIndexVector) {
//				masterFeatureVector.remove(masterFeatureVector.get(i));
//			}
//			for (Integer i : divideFeatureIndexVector) {
//				masterFeatureVector.add(featureVector.get(i));
//			}
//		
//			divideFeatureVector = (FeatureVector)featureVector.clone();
//			for (Integer i : divideFeatureIndexVector) {
//				divideFeatureVector.remove(divideFeatureVector.get(i));
//			}
//		} else {
			masterFeatureVector = featureVector;
			divideFeatureVector = (FeatureVector)featureVector.clone();
			for (Integer i : divideFeatureIndexVector) {
				divideFeatureVector.remove(divideFeatureVector.get(i));
			}
//		}
		try {
			if (getGuide().getConfiguration().getOptionValue("guide", "data_split_threshold").toString() != null) {
				divideThreshold = Integer.parseInt(getGuide().getConfiguration().getOptionValue("guide", "data_split_threshold").toString());
			} else {
				divideThreshold = 0;
			}
		} catch (NumberFormatException e) {
			throw new GuideException("The --guide-data_split_threshold option is not an integer value. ", e);
		}
	}
	
	/**
	 * Saves the feature divide model settings .fsm file.
	 * 
	 * @throws MaltChainedException
	 */
	protected void save() throws MaltChainedException {
		try {
			final BufferedWriter out = new BufferedWriter(getGuide().getConfiguration().getConfigurationDir().getOutputStreamWriter(getModelName()+".dsm"));
			out.write(masterModel.getIndex() + "\t" + masterModel.getFrequency() + "\n");

			if (divideModels != null) {
				for (AtomicModel divideModel : divideModels.values()) {
					out.write(divideModel.getIndex() + "\t" + divideModel.getFrequency() + "\n");
	        	}
			}
			out.close();
		} catch (IOException e) {
			throw new GuideException("Could not write to the guide model settings file '"+getModelName()+".dsm"+"', when " +
					"saving the guide model settings to file. ", e);
		}
	}
	
	/**
	 * Loads the feature divide model settings .fsm file.
	 * 
	 * @throws MaltChainedException
	 */
	protected void load() throws MaltChainedException {
		try {
			final BufferedReader in = new BufferedReader(getGuide().getConfiguration().getConfigurationDir().getInputStreamReaderFromConfigFile(getModelName()+".dsm"));
			final Pattern tabPattern = Pattern.compile("\t");
			while(true) {
				String line = in.readLine();
				if(line == null) break;
				String[] cols = tabPattern.split(line);
				if (cols.length != 2) { 
					throw new GuideException("");
				}
				int code = -1;
				int freq = 0;
				try {
					code = Integer.parseInt(cols[0]);
					freq = Integer.parseInt(cols[1]);
				} catch (NumberFormatException e) {
					throw new GuideException("Could not convert a string value into an integer value when loading the feature divide model settings (.fsm). ", e);
				}
				if (code == -1) { 
					masterModel = new AtomicModel(-1, masterFeatureVector, this);
					masterModel.setFrequency(freq);
				} else if (divideModels != null) {
					divideModels.put(code, new AtomicModel(code, divideFeatureVector, this));
					divideModels.get(code).setFrequency(freq);
				}
				setFrequency(getFrequency()+freq);
			}
			in.close();
		} catch (IOException e) {
			throw new GuideException("Could not read from the guide model settings file '"+getModelName()+".dsm"+"', when " +
					"loading the guide model settings. ", e);
		}	
	}
	
	/**
	 * Returns the parent model
	 * 
	 * @return the parent model
	 */
	public Model getParent() {
		return parent;
	}

	public ClassifierGuide getGuide() {
		return parent.getGuide();
	}
	
	/**
	 * Sets the parent model
	 * 
	 * @param parent the parent model
	 */
	protected void setParent(Model parent) throws MaltChainedException {
		this.parent = parent;
	}


	public String getModelName() throws MaltChainedException {
		try {
			return parent.getModelName();
		} catch (NullPointerException e) {
			throw new GuideException("The parent guide model cannot be found. ", e);
		}
	}

	/**
	 * Returns the "sum-up" or master feature vector
	 * 
	 * @return a feature vector object
	 */
	public FeatureVector getMasterFeatureVector() {
		return masterFeatureVector;
	}

	/**
	 * Returns the divide feature vector
	 * 
	 * @return a feature vector object
	 */
	public FeatureVector getDivideFeatureVector() {
		return divideFeatureVector;
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


	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		//TODO
		return sb.toString();
	}
}
