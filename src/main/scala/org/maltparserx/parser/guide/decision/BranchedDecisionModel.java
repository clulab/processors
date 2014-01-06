package org.maltparserx.parser.guide.decision;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;


import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.feature.FeatureModel;
import org.maltparserx.core.feature.FeatureVector;
import org.maltparserx.core.helper.HashMap;
import org.maltparserx.core.syntaxgraph.DependencyStructure;
import org.maltparserx.parser.DependencyParserConfig;
import org.maltparserx.parser.guide.ClassifierGuide;
import org.maltparserx.parser.guide.GuideException;
import org.maltparserx.parser.guide.instance.AtomicModel;
import org.maltparserx.parser.guide.instance.FeatureDivideModel;
import org.maltparserx.parser.guide.instance.InstanceModel;
import org.maltparserx.parser.history.action.GuideDecision;
import org.maltparserx.parser.history.action.MultipleDecision;
import org.maltparserx.parser.history.action.SingleDecision;
import org.maltparserx.parser.history.container.TableContainer.RelationToNextDecision;
/**
*
* @author Johan Hall
* @since 1.1
**/
public class BranchedDecisionModel implements DecisionModel {
	private ClassifierGuide guide;
	private String modelName;
	private FeatureModel featureModel;
	private InstanceModel instanceModel;
	private int decisionIndex;
	private DecisionModel parentDecisionModel;
	private HashMap<Integer,DecisionModel> children;
	private String branchedDecisionSymbols;
	
	public BranchedDecisionModel(ClassifierGuide guide, FeatureModel featureModel) throws MaltChainedException {
		this.branchedDecisionSymbols = "";
		setGuide(guide);
		setFeatureModel(featureModel);
		setDecisionIndex(0);
		setModelName("bdm"+decisionIndex);
		setParentDecisionModel(null);
	}
	
	public BranchedDecisionModel(ClassifierGuide guide, DecisionModel parentDecisionModel, String branchedDecisionSymbol) throws MaltChainedException {
		if (branchedDecisionSymbol != null && branchedDecisionSymbol.length() > 0) {
			this.branchedDecisionSymbols = branchedDecisionSymbol;
		} else {
			this.branchedDecisionSymbols = "";
		}
		setGuide(guide);
		setParentDecisionModel(parentDecisionModel);
		setDecisionIndex(parentDecisionModel.getDecisionIndex() + 1);
		setFeatureModel(parentDecisionModel.getFeatureModel());
		if (branchedDecisionSymbols != null && branchedDecisionSymbols.length() > 0) {
			setModelName("bdm"+decisionIndex+branchedDecisionSymbols);
		} else {
			setModelName("bdm"+decisionIndex);
		}
		this.parentDecisionModel = parentDecisionModel;
	}
	
	public void updateFeatureModel() throws MaltChainedException {
		featureModel.update();
	}
	
//	public void updateCardinality() throws MaltChainedException {
//		featureModel.updateCardinality();
//	}
	

	public void finalizeSentence(DependencyStructure dependencyGraph) throws MaltChainedException {
		if (instanceModel != null) {
			instanceModel.finalizeSentence(dependencyGraph);
		}
		if (children != null) {
			for (DecisionModel child : children.values()) {
				child.finalizeSentence(dependencyGraph);
			}
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
		if (children != null) {
			for (DecisionModel child : children.values()) {
				child.noMoreInstances();
			}
		}
	}

	public void terminate() throws MaltChainedException {
		if (instanceModel != null) {
			instanceModel.terminate();
			instanceModel = null;
		}
		if (children != null) {
			for (DecisionModel child : children.values()) {
				child.terminate();
			}
		}
	}
	
	public void addInstance(GuideDecision decision) throws MaltChainedException {
		if (decision instanceof SingleDecision) {
			throw new GuideException("A branched decision model expect more than one decisions. ");
		}
		featureModel.update();
		final SingleDecision singleDecision = ((MultipleDecision)decision).getSingleDecision(decisionIndex);
		if (instanceModel == null) {
			initInstanceModel(singleDecision.getTableContainer().getTableContainerName());
		}
		
		instanceModel.addInstance(singleDecision);
		if (decisionIndex+1 < decision.numberOfDecisions()) {
			if (singleDecision.continueWithNextDecision()) {
				if (children == null) {
					children = new HashMap<Integer,DecisionModel>();
				}
				DecisionModel child = children.get(singleDecision.getDecisionCode());
				if (child == null) {
					child = initChildDecisionModel(((MultipleDecision)decision).getSingleDecision(decisionIndex+1), 
							branchedDecisionSymbols+(branchedDecisionSymbols.length() == 0?"":"_")+singleDecision.getDecisionSymbol());
					children.put(singleDecision.getDecisionCode(), child);
				}
				child.addInstance(decision);
			}
		}
	}
	
	public boolean predict(GuideDecision decision) throws MaltChainedException {
//		if (decision instanceof SingleDecision) {
//			throw new GuideException("A branched decision model expect more than one decisions. ");
//		}
		featureModel.update();
		final SingleDecision singleDecision = ((MultipleDecision)decision).getSingleDecision(decisionIndex);
		if (instanceModel == null) {
			initInstanceModel(singleDecision.getTableContainer().getTableContainerName());
		}
		instanceModel.predict(singleDecision);
		if (decisionIndex+1 < decision.numberOfDecisions()) {
			if (singleDecision.continueWithNextDecision()) {
				if (children == null) {
					children = new HashMap<Integer,DecisionModel>();
				}
				DecisionModel child = children.get(singleDecision.getDecisionCode());
				if (child == null) {
					child = initChildDecisionModel(((MultipleDecision)decision).getSingleDecision(decisionIndex+1), 
							branchedDecisionSymbols+(branchedDecisionSymbols.length() == 0?"":"_")+singleDecision.getDecisionSymbol());
					children.put(singleDecision.getDecisionCode(), child);
				}
				child.predict(decision);
			}
		}

		return true;
	}
	
	public FeatureVector predictExtract(GuideDecision decision) throws MaltChainedException {
		if (decision instanceof SingleDecision) {
			throw new GuideException("A branched decision model expect more than one decisions. ");
		}
		featureModel.update();
		final SingleDecision singleDecision = ((MultipleDecision)decision).getSingleDecision(decisionIndex);
		if (instanceModel == null) {
			initInstanceModel(singleDecision.getTableContainer().getTableContainerName());
		}
		FeatureVector fv = instanceModel.predictExtract(singleDecision);
		if (decisionIndex+1 < decision.numberOfDecisions()) {
			if (singleDecision.continueWithNextDecision()) {
				if (children == null) {
					children = new HashMap<Integer,DecisionModel>();
				}
				DecisionModel child = children.get(singleDecision.getDecisionCode());
				if (child == null) {
					child = initChildDecisionModel(((MultipleDecision)decision).getSingleDecision(decisionIndex+1), 
							branchedDecisionSymbols+(branchedDecisionSymbols.length() == 0?"":"_")+singleDecision.getDecisionSymbol());
					children.put(singleDecision.getDecisionCode(), child);
				}
				child.predictExtract(decision);
			}
		}

		return fv;
	}
	
	public FeatureVector extract() throws MaltChainedException {
		featureModel.update();
		return instanceModel.extract(); // TODO handle many feature vectors
	}
	
	public boolean predictFromKBestList(GuideDecision decision) throws MaltChainedException {
		if (decision instanceof SingleDecision) {
			throw new GuideException("A branched decision model expect more than one decisions. ");
		}
		
		boolean success = false;
		final SingleDecision singleDecision = ((MultipleDecision)decision).getSingleDecision(decisionIndex);
		if (decisionIndex+1 < decision.numberOfDecisions()) {
			if (singleDecision.continueWithNextDecision()) {
				if (children == null) {
					children = new HashMap<Integer,DecisionModel>();
				}
				DecisionModel child = children.get(singleDecision.getDecisionCode());
				if (child != null) {
					success = child.predictFromKBestList(decision);
				}
				
			}
		}
		if (!success) {
			success = singleDecision.updateFromKBestList();
			if (decisionIndex+1 < decision.numberOfDecisions()) {
				if (singleDecision.continueWithNextDecision()) {
					if (children == null) {
						children = new HashMap<Integer,DecisionModel>();
					}
					DecisionModel child = children.get(singleDecision.getDecisionCode());
					if (child == null) {
						child = initChildDecisionModel(((MultipleDecision)decision).getSingleDecision(decisionIndex+1), 
								branchedDecisionSymbols+(branchedDecisionSymbols.length() == 0?"":"_")+singleDecision.getDecisionSymbol());
						children.put(singleDecision.getDecisionCode(), child);
					}
					child.predict(decision);
				}
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

	public DecisionModel getParentDecisionModel() {
		return parentDecisionModel;
	}

	private void setFeatureModel(FeatureModel featureModel) {
		this.featureModel = featureModel;
	}
	
	private void setDecisionIndex(int decisionIndex) {
		this.decisionIndex = decisionIndex;
	}
	
	private void setParentDecisionModel(DecisionModel parentDecisionModel) {
		this.parentDecisionModel = parentDecisionModel;
	}

	private void setModelName(String modelName) {
		this.modelName = modelName;
	}
	
	private void setGuide(ClassifierGuide guide) {
		this.guide = guide;
	}
	
	
	private DecisionModel initChildDecisionModel(SingleDecision decision, String branchedDecisionSymbol) throws MaltChainedException {
		Class<?> decisionModelClass = null;
		if (decision.getRelationToNextDecision() == RelationToNextDecision.SEQUANTIAL) {
			decisionModelClass = org.maltparserx.parser.guide.decision.SeqDecisionModel.class;
		} else if (decision.getRelationToNextDecision() == RelationToNextDecision.BRANCHED) {
			decisionModelClass = org.maltparserx.parser.guide.decision.BranchedDecisionModel.class;
		} else if (decision.getRelationToNextDecision() == RelationToNextDecision.NONE) {
			decisionModelClass = org.maltparserx.parser.guide.decision.OneDecisionModel.class;
		}

		if (decisionModelClass == null) {
			throw new GuideException("Could not find an appropriate decision model for the relation to the next decision"); 
		}
		
		try {
			Class<?>[] argTypes = { org.maltparserx.parser.guide.ClassifierGuide.class, org.maltparserx.parser.guide.decision.DecisionModel.class,
						java.lang.String.class };
			Object[] arguments = new Object[3];
			arguments[0] = getGuide();
			arguments[1] = this;
			arguments[2] = branchedDecisionSymbol;
			Constructor<?> constructor = decisionModelClass.getConstructor(argTypes);
			return (DecisionModel)constructor.newInstance(arguments);
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
	
	private void initInstanceModel(String subModelName) throws MaltChainedException {
		FeatureVector fv = featureModel.getFeatureVector(branchedDecisionSymbols+"."+subModelName);
		if (fv == null) {
			fv = featureModel.getFeatureVector(subModelName);
		}
		if (fv == null) {
			fv = featureModel.getMainFeatureVector();
		}
		
		DependencyParserConfig c = guide.getConfiguration();
		
//		if (c.getOptionValue("guide", "tree_automatic_split_order").toString().equals("yes") ||
//				(c.getOptionValue("guide", "tree_split_columns")!=null &&
//			c.getOptionValue("guide", "tree_split_columns").toString().length() > 0) ||
//			(c.getOptionValue("guide", "tree_split_structures")!=null &&
//			c.getOptionValue("guide", "tree_split_structures").toString().length() > 0)) {
//			instanceModel = new DecisionTreeModel(fv, this); 
//		}else 
		if (c.getOptionValue("guide", "data_split_column").toString().length() == 0) {
			instanceModel = new AtomicModel(-1, fv, this);
		} else {
			instanceModel = new FeatureDivideModel(fv, this);
		}
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(modelName + ", ");
		for (DecisionModel model : children.values()) {
			sb.append(model.toString() + ", ");
		}
		return sb.toString();
	}
}
