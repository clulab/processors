package org.maltparser.parser.guide;


import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.FeatureModel;
import org.maltparser.core.feature.FeatureModelManager;
import org.maltparser.core.feature.FeatureVector;
import org.maltparser.core.feature.system.FeatureEngine;
import org.maltparser.core.plugin.PluginLoader;
import org.maltparser.core.syntaxgraph.DependencyStructure;
import org.maltparser.parser.DependencyParserConfig;
import org.maltparser.parser.guide.decision.DecisionModel;
import org.maltparser.parser.history.GuideHistory;
import org.maltparser.parser.history.action.GuideDecision;
import org.maltparser.parser.history.action.MultipleDecision;
import org.maltparser.parser.history.action.SingleDecision;
import org.maltparser.parser.history.container.TableContainer.RelationToNextDecision;


/**
 * The guide is used by a parsing algorithm to predict the next parser action during parsing and to
 * add a instance to the training instance set during learning.

@author Johan Hall
@since 1.0
*/
public class SingleGuide implements ClassifierGuide {
	private final DependencyParserConfig configuration;
	private final GuideHistory history;
	private final GuideMode guideMode;
	private final FeatureModelManager featureModelManager;
	private final FeatureModel featureModel;
	private DecisionModel decisionModel = null;
	private String guideName;
	
	public SingleGuide(DependencyParserConfig configuration, GuideHistory history, GuideMode guideMode) throws MaltChainedException {
		this.configuration = configuration;
		
		this.guideMode = guideMode;
		final FeatureEngine system = new FeatureEngine();
		system.load("/appdata/features/ParserFeatureSystem.xml");
		system.load(PluginLoader.instance());
		featureModelManager = new FeatureModelManager(system, getConfiguration().getConfigurationDir());

		// initialize history
		this.history = history;
		Class<?> kBestListClass = null;
		int kBestSize = 1;
		if (guideMode == ClassifierGuide.GuideMode.CLASSIFY) {
			kBestListClass = (Class<?>)getConfiguration().getOptionValue("guide", "kbest_type");
			kBestSize = ((Integer)getConfiguration().getOptionValue("guide", "kbest")).intValue();
		}
		history.setKBestListClass(kBestListClass);
		history.setKBestSize(kBestSize);
		history.setSeparator(getConfiguration().getOptionValue("guide", "classitem_separator").toString());

		// initialize feature model
		String featureModelFileName = getConfiguration().getOptionValue("guide", "features").toString().trim();

		if (featureModelFileName.endsWith(".par")) {
			String markingStrategy = getConfiguration().getOptionValue("pproj", "marking_strategy").toString().trim();
			String coveredRoot = getConfiguration().getOptionValue("pproj", "covered_root").toString().trim();
			featureModelManager.loadParSpecification(featureModelFileName, markingStrategy, coveredRoot);
		} else {
			featureModelManager.loadSpecification(featureModelFileName);
		}
		if (getConfiguration().getConfigLogger().isInfoEnabled()) {
			getConfiguration().getConfigLogger().info("  Feature model        : " + featureModelFileName+"\n");
			if (getGuideMode() == ClassifierGuide.GuideMode.BATCH) {
				getConfiguration().getConfigLogger().info("  Learner              : " + getConfiguration().getOptionValueString("guide", "learner").toString()+"\n");
			} else {
				getConfiguration().getConfigLogger().info("  Classifier           : " + getConfiguration().getOptionValueString("guide", "learner")+"\n");	
			}
		}
		featureModel = getFeatureModelManager().getFeatureModel(getConfiguration().getOptionValue("guide", "features").toString(), 0, getConfiguration().getRegistry());
		if (getGuideMode() == ClassifierGuide.GuideMode.BATCH && getConfiguration().getConfigurationDir().getInfoFileWriter() != null) {
			try {
				getConfiguration().getConfigurationDir().getInfoFileWriter().write("\nFEATURE MODEL\n");
				getConfiguration().getConfigurationDir().getInfoFileWriter().write(featureModel.toString());
				getConfiguration().getConfigurationDir().getInfoFileWriter().flush();
			} catch (IOException e) {
				throw new GuideException("Could not write feature model specification to configuration information file. ", e);
			}
		}

	}
		
	public void addInstance(GuideDecision decision) throws MaltChainedException {
		if (decisionModel == null) {
			if (decision instanceof SingleDecision) {
				initDecisionModel((SingleDecision)decision);
			} else if (decision instanceof MultipleDecision && decision.numberOfDecisions() > 0) {
				initDecisionModel(((MultipleDecision)decision).getSingleDecision(0));
			}
		}
		decisionModel.addInstance(decision);
	}
	
	public void finalizeSentence(DependencyStructure dependencyGraph) throws MaltChainedException {
		if (decisionModel != null) {
			decisionModel.finalizeSentence(dependencyGraph);
		}
	}
	
	public void noMoreInstances() throws MaltChainedException {
		if (decisionModel != null) {
			decisionModel.noMoreInstances();
		} else {
			configuration.getConfigLogger().debug("The guide cannot create any models because there is no decision model. ");
		}
	}
	
	public void terminate() throws MaltChainedException {
		if (decisionModel != null) {
			decisionModel.terminate();
			decisionModel = null;
		}
	}

	public void predict(GuideDecision decision) throws MaltChainedException {
		if (decisionModel == null) {
			if (decision instanceof SingleDecision) {
				initDecisionModel((SingleDecision)decision);
			} else if (decision instanceof MultipleDecision && decision.numberOfDecisions() > 0) {
				initDecisionModel(((MultipleDecision)decision).getSingleDecision(0));
			}
		}
		decisionModel.predict(decision);
	}

	public FeatureVector predictExtract(GuideDecision decision) throws MaltChainedException {
		if (decisionModel == null) {
			if (decision instanceof SingleDecision) {
				initDecisionModel((SingleDecision)decision);
			} else if (decision instanceof MultipleDecision && decision.numberOfDecisions() > 0) {
				initDecisionModel(((MultipleDecision)decision).getSingleDecision(0));
			}
		}
		return decisionModel.predictExtract(decision);
	}
	
	public FeatureVector extract() throws MaltChainedException {
		return decisionModel.extract();
	}
	
	public boolean predictFromKBestList(GuideDecision decision) throws MaltChainedException {
		if (decisionModel != null) {
			return decisionModel.predictFromKBestList(decision);
		} else {
			throw new GuideException("The decision model cannot be found. ");
		}
	}
	
	public DecisionModel getDecisionModel() {
		return decisionModel;
	}

	public DependencyParserConfig getConfiguration() {
		return configuration;
	}
	
	public GuideHistory getHistory() {
		return history;
	}
	
	public GuideMode getGuideMode() {
		return guideMode;
	}
	
	public FeatureModelManager getFeatureModelManager() {
		return featureModelManager;
	}
	
	protected void initDecisionModel(SingleDecision decision) throws MaltChainedException {
		Class<?> decisionModelClass = null;
		if (decision.getRelationToNextDecision() == RelationToNextDecision.SEQUANTIAL) {
			decisionModelClass = org.maltparser.parser.guide.decision.SeqDecisionModel.class;
		} else if (decision.getRelationToNextDecision() == RelationToNextDecision.BRANCHED) {
			decisionModelClass = org.maltparser.parser.guide.decision.BranchedDecisionModel.class;
		} else if (decision.getRelationToNextDecision() == RelationToNextDecision.NONE) {
			decisionModelClass = org.maltparser.parser.guide.decision.OneDecisionModel.class;
		}

		if (decisionModelClass == null) {
			throw new GuideException("Could not find an appropriate decision model for the relation to the next decision"); 
		}
		
		try {
			Class<?>[] argTypes = { org.maltparser.parser.guide.ClassifierGuide.class, org.maltparser.core.feature.FeatureModel.class };
			Object[] arguments = new Object[2];
			arguments[0] = this;
			arguments[1] = featureModel;
			Constructor<?> constructor = decisionModelClass.getConstructor(argTypes);
			decisionModel = (DecisionModel)constructor.newInstance(arguments);
		} catch (NoSuchMethodException e) {
			throw new GuideException("The decision model class '"+decisionModelClass.getName()+"' cannot be initialized. ", e);
		} catch (InstantiationException e) {
			throw new GuideException("The decision model class '"+decisionModelClass.getName()+"' cannot be initialized. ", e);
		} catch (IllegalAccessException e) {
			throw new GuideException("The decision model class '"+decisionModelClass.getName()+"' cannot be initialized. ", e);
		} catch (InvocationTargetException e) {
			throw new GuideException("The decision model class '"+decisionModelClass.getName()+"' cannot be initialized. ", e);
		}
	}
	
	public String getGuideName() {
		return guideName;
	}

	public void setGuideName(String guideName) {
		this.guideName = guideName;
	}

	public String toString() {
		final StringBuilder sb = new StringBuilder();
		return sb.toString();
	}
}
