package org.maltparser.parser.guide.decision;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.FeatureModel;
import org.maltparser.core.feature.FeatureVector;
import org.maltparser.core.syntaxgraph.DependencyStructure;
import org.maltparser.parser.DependencyParserConfig;
import org.maltparser.parser.guide.ClassifierGuide;
import org.maltparser.parser.guide.GuideException;
import org.maltparser.parser.guide.instance.AtomicModel;
import org.maltparser.parser.guide.instance.FeatureDivideModel;
import org.maltparser.parser.guide.instance.InstanceModel;
import org.maltparser.parser.history.action.GuideDecision;
import org.maltparser.parser.history.action.MultipleDecision;
import org.maltparser.parser.history.action.SingleDecision;
import org.maltparser.parser.history.container.TableContainer.RelationToNextDecision;
/**
*
* @author Johan Hall
* @since 1.1
**/
public class SeqDecisionModel implements DecisionModel {
	private ClassifierGuide guide;
	private String modelName;
	private FeatureModel featureModel;
	private InstanceModel instanceModel;
	private int decisionIndex;
	private DecisionModel prevDecisionModel;
	private DecisionModel nextDecisionModel;
	private String branchedDecisionSymbols;
	
	public SeqDecisionModel(ClassifierGuide guide, FeatureModel featureModel) throws MaltChainedException {
		this.branchedDecisionSymbols = "";
		setGuide(guide);
		setFeatureModel(featureModel);
		setDecisionIndex(0);
		setModelName("sdm"+decisionIndex);
		setPrevDecisionModel(null);
	}
	
	public SeqDecisionModel(ClassifierGuide guide, DecisionModel prevDecisionModel, String branchedDecisionSymbol) throws MaltChainedException {
		if (branchedDecisionSymbol != null && branchedDecisionSymbol.length() > 0) {
			this.branchedDecisionSymbols = branchedDecisionSymbol;
		} else {
			this.branchedDecisionSymbols = "";
		}
		setGuide(guide);
		setFeatureModel(prevDecisionModel.getFeatureModel());
		setDecisionIndex(prevDecisionModel.getDecisionIndex() + 1);
		setPrevDecisionModel(prevDecisionModel);
		if (branchedDecisionSymbols != null && branchedDecisionSymbols.length() > 0) {
			setModelName("sdm"+decisionIndex+branchedDecisionSymbols);
		} else {
			setModelName("sdm"+decisionIndex);
		}
	}
	
	public void updateFeatureModel() throws MaltChainedException {
		featureModel.update();
	}
	
	public void finalizeSentence(DependencyStructure dependencyGraph) throws MaltChainedException {
		if (instanceModel != null) {
			instanceModel.finalizeSentence(dependencyGraph);
		}
		if (nextDecisionModel != null) {
			nextDecisionModel.finalizeSentence(dependencyGraph);
		}
	}
	
	public void noMoreInstances() throws MaltChainedException {
		if (guide.getGuideMode() == ClassifierGuide.GuideMode.CLASSIFY) {
			throw new GuideException("The decision model could not create it's model. ");
		}
		if (instanceModel != null) {
			instanceModel.noMoreInstances();
			instanceModel.train();
		}
		if (nextDecisionModel != null) {
			nextDecisionModel.noMoreInstances();
		}
	}

	public void terminate() throws MaltChainedException {
		if (instanceModel != null) {
			instanceModel.terminate();
			instanceModel = null;
		}
		if (nextDecisionModel != null) {
			nextDecisionModel.terminate();
			nextDecisionModel = null;
		}
	}
	
	public void addInstance(GuideDecision decision) throws MaltChainedException {
		if (decision instanceof SingleDecision) {
			throw new GuideException("A sequantial decision model expect a sequence of decisions, not a single decision. ");
		}
		featureModel.update();
		final SingleDecision singleDecision = ((MultipleDecision)decision).getSingleDecision(decisionIndex);
		if (instanceModel == null) {
			initInstanceModel(singleDecision.getTableContainer().getTableContainerName());
		}
		instanceModel.addInstance(singleDecision);
		if (singleDecision.continueWithNextDecision() && decisionIndex+1 < decision.numberOfDecisions()) {
			if (nextDecisionModel == null) {
				initNextDecisionModel(((MultipleDecision)decision).getSingleDecision(decisionIndex+1), branchedDecisionSymbols);
			}
			nextDecisionModel.addInstance(decision);
		}
	}
	
	public boolean predict(GuideDecision decision) throws MaltChainedException {
		if (decision instanceof SingleDecision) {
			throw new GuideException("A sequantial decision model expect a sequence of decisions, not a single decision. ");
		}
		featureModel.update();
		final SingleDecision singleDecision = ((MultipleDecision)decision).getSingleDecision(decisionIndex);
		if (instanceModel == null) {
			initInstanceModel(singleDecision.getTableContainer().getTableContainerName());
		}

		boolean success = instanceModel.predict(singleDecision);
		if (singleDecision.continueWithNextDecision() && decisionIndex+1 < decision.numberOfDecisions()) {
			if (nextDecisionModel == null) {
				initNextDecisionModel(((MultipleDecision)decision).getSingleDecision(decisionIndex+1), branchedDecisionSymbols);
			}
			success = nextDecisionModel.predict(decision) && success;
		}
		return success;
	}
	
	public FeatureVector predictExtract(GuideDecision decision) throws MaltChainedException {
		if (decision instanceof SingleDecision) {
			throw new GuideException("A sequantial decision model expect a sequence of decisions, not a single decision. ");
		}
		featureModel.update();
		final SingleDecision singleDecision = ((MultipleDecision)decision).getSingleDecision(decisionIndex);
		if (instanceModel == null) {
			initInstanceModel(singleDecision.getTableContainer().getTableContainerName());
		}

		FeatureVector fv = instanceModel.predictExtract(singleDecision);
		if (singleDecision.continueWithNextDecision() && decisionIndex+1 < decision.numberOfDecisions()) {
			if (nextDecisionModel == null) {
				initNextDecisionModel(((MultipleDecision)decision).getSingleDecision(decisionIndex+1), branchedDecisionSymbols);
			}
			nextDecisionModel.predictExtract(decision);
		}
		return fv;
	}
	
	public FeatureVector extract() throws MaltChainedException {
		featureModel.update();
		return instanceModel.extract(); // TODO handle many feature vectors
	}
	
	public boolean predictFromKBestList(GuideDecision decision) throws MaltChainedException {
		if (decision instanceof SingleDecision) {
			throw new GuideException("A sequantial decision model expect a sequence of decisions, not a single decision. ");
		}
		
		boolean success = false;
		final SingleDecision singleDecision = ((MultipleDecision)decision).getSingleDecision(decisionIndex);
		// TODO develop different strategies for resolving which kBestlist that should be used
		if (nextDecisionModel != null && singleDecision.continueWithNextDecision()) {
			success = nextDecisionModel.predictFromKBestList(decision);
		}
		if (!success) {
			success = singleDecision.updateFromKBestList();
			if (success && singleDecision.continueWithNextDecision() && decisionIndex+1 < decision.numberOfDecisions()) {
				if (nextDecisionModel == null) {
					initNextDecisionModel(((MultipleDecision)decision).getSingleDecision(decisionIndex+1), branchedDecisionSymbols);
				}
				nextDecisionModel.predict(decision);
			}
		}
		return success;
	}
	

	public ClassifierGuide getGuide() {
		return guide;
	}

	public String getModelName() {
		return modelName;
	}
	
	public FeatureModel getFeatureModel() {
		return featureModel;
	}

	public int getDecisionIndex() {
		return decisionIndex;
	}

	public DecisionModel getPrevDecisionModel() {
		return prevDecisionModel;
	}

	public DecisionModel getNextDecisionModel() {
		return nextDecisionModel;
	}
	
	private void setPrevDecisionModel(DecisionModel prevDecisionModel) {
		this.prevDecisionModel = prevDecisionModel;
	}
	
	private void setNextDecisionModel(DecisionModel nextDecisionModel) {
		this.nextDecisionModel = nextDecisionModel;
	}

	private void setFeatureModel(FeatureModel featureModel) {
		this.featureModel = featureModel;
	}
	
	private void setDecisionIndex(int decisionIndex) {
		this.decisionIndex = decisionIndex;
	}

	private void setModelName(String modelName) {
		this.modelName = modelName;
	}
	
	private void setGuide(ClassifierGuide guide) {
		this.guide = guide;
	}
	
	private void initInstanceModel(String subModelName) throws MaltChainedException {
		FeatureVector fv = featureModel.getFeatureVector(branchedDecisionSymbols+"."+subModelName);
		if (fv == null) {
			fv = featureModel.getFeatureVector(subModelName);
		}
		if (fv == null) {
			fv = featureModel.getMainFeatureVector();
		}
		
		DependencyParserConfig c = guide.getConfiguration();
		
		if (c.getOptionValue("guide", "data_split_column").toString().length() == 0) {
			instanceModel = new AtomicModel(-1, fv, this);
		} else {
			instanceModel = new FeatureDivideModel(fv, this);
		}
	}
	
	private void initNextDecisionModel(SingleDecision decision, String branchedDecisionSymbol) throws MaltChainedException {
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
			Class<?>[] argTypes = { org.maltparser.parser.guide.ClassifierGuide.class, org.maltparser.parser.guide.decision.DecisionModel.class,
									java.lang.String.class };
			Object[] arguments = new Object[3];
			arguments[0] = getGuide();
			arguments[1] = this;
			arguments[2] = branchedDecisionSymbol;
			Constructor<?> constructor = decisionModelClass.getConstructor(argTypes);
			setNextDecisionModel((DecisionModel)constructor.newInstance(arguments));
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
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(modelName + ", ");
		sb.append(nextDecisionModel.toString());
		return sb.toString();
	}
}
