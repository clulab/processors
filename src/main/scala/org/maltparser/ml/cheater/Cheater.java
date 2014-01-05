package org.maltparser.ml.cheater;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Set;
import java.util.jar.JarEntry;


import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.FeatureVector;
import org.maltparser.core.feature.function.FeatureFunction;
import org.maltparser.core.feature.value.FeatureValue;
import org.maltparser.core.feature.value.MultipleFeatureValue;
import org.maltparser.core.feature.value.SingleFeatureValue;
import org.maltparser.core.syntaxgraph.DependencyStructure;
import org.maltparser.ml.LearningMethod;
import org.maltparser.parser.DependencyParserConfig;
import org.maltparser.parser.guide.instance.InstanceModel;
import org.maltparser.parser.history.action.SingleDecision;

public class Cheater implements LearningMethod {
	public enum Verbostity {
		SILENT, ERROR, ALL
	}
	protected InstanceModel owner;
	protected int learnerMode;
	protected String name;
	protected int numberOfInstances;
	protected boolean excludeNullValues;
//	private int[] cardinalities;
	private String cheaterFileName;
	private BufferedWriter cheaterWriter = null;
	private boolean saveCheatAction;
	private BufferedWriter instanceOutput = null; 
	private ArrayList<Integer> cheatValues;
	private int cheaterPosition;
	private Verbostity verbosity;
	
	public Cheater(InstanceModel owner, Integer learnerMode) throws MaltChainedException {
		setOwner(owner);
		setLearningMethodName("cheater");
		setLearnerMode(learnerMode.intValue());
		setNumberOfInstances(0);
		verbosity = Verbostity.SILENT;
		initSpecialParameters();
	
		if (learnerMode == BATCH) {
			if (!saveCheatAction) {
				instanceOutput = new BufferedWriter(getInstanceOutputStreamWriter(".ins"));
			} else {
				try {
					if (cheaterFileName != null && !cheaterFileName.equals("")) {
						cheaterWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(cheaterFileName)));
					}
				} catch (Exception e) {
					throw new CheaterException("", e);
				}
			}
		} 
	}
	
	public void addInstance(SingleDecision decision, FeatureVector featureVector) throws MaltChainedException {
		if (featureVector == null) {
			throw new CheaterException("The feature vector cannot be found");
		} else if (decision == null) {
			throw new CheaterException("The decision cannot be found");
		}	
		if (saveCheatAction && cheaterWriter != null) {
			try {
				cheaterWriter.write(decision.getDecisionCode()+"\n");
			} catch (IOException e) {
				throw new CheaterException("The cheater learner cannot write to the cheater file. ", e);
			}
		} else {
			StringBuilder sb = new StringBuilder();
			try {
				sb.append(decision.getDecisionCode()+"\t");
				int n = featureVector.size();
				for (int i = 0; i < n; i++) {
					FeatureValue featureValue = featureVector.get(i).getFeatureValue();
					if (excludeNullValues == true && featureValue.isNullValue()) {
						sb.append("-1");
					} else {
						if (featureValue instanceof SingleFeatureValue) {
							sb.append(((SingleFeatureValue)featureValue).getIndexCode()+"");
						} else if (featureValue instanceof MultipleFeatureValue) {
							Set<Integer> values = ((MultipleFeatureValue)featureValue).getCodes();
							int j=0;
							for (Integer value : values) {
								sb.append(value.toString());
								if (j != values.size()-1) {
									sb.append("|");
								}
								j++;
							}
						}
					}
	//				if (i < n-1) {
						sb.append('\t');
	//				}
				}
				sb.append('\n');
				instanceOutput.write(sb.toString());
				instanceOutput.flush();
				increaseNumberOfInstances();
			} catch (IOException e) {
				throw new CheaterException("The cheater learner cannot write to the instance file. ", e);
			}
		}
	}
	
	public void train(FeatureVector featureVector) throws MaltChainedException {
		if (featureVector == null) {
			throw new CheaterException("The feature vector cannot be found. ");
		} else if (owner == null) {
			throw new CheaterException("The parent guide model cannot be found. ");
		}
//		if (!saveCheatAction) {
//			cardinalities = getCardinalities(featureVector);
//			maltSVMFormat2OriginalSVMFormat(getInstanceInputStreamReader(".ins"), getInstanceOutputStreamWriter(".ins.tmp"), cardinalities);
//			saveCardinalities(getInstanceOutputStreamWriter(".car"), cardinalities);
//		}
	}
	
	
	public boolean predict(FeatureVector featureVector, SingleDecision decision) throws MaltChainedException {
//		if (cardinalities == null) {
//			if (getConfigFileEntry(".car") != null) {
//				cardinalities = loadCardinalities(getInstanceInputStreamReaderFromConfigFile(".car"));
//			} else {
//				cardinalities = getCardinalities(featureVector);
//			}
//		}
		if (cheatValues == null) {
			if (cheaterFileName == null || cheaterFileName.equals("")) {
				throw new CheaterException("The cheater file name is assigned. ");
			}
			try {
				BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(cheaterFileName)));
				String line = "";
				cheatValues = new ArrayList<Integer>();
				while ((line = reader.readLine()) != null) {
					cheatValues.add(Integer.parseInt(line));
				}
				cheaterPosition = 0;
				reader.close();
				cheaterWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(cheaterFileName + ".ins")));
			} catch (Exception e) {
				throw new CheaterException("Couldn't find or read from the cheater file '"+cheaterFileName+"'", e);
			}
		}
		
//		int offset = 1;
//		int i = 0;
		int decisionValue = 0;
		StringBuilder csb = new StringBuilder();
		if (cheaterPosition < cheatValues.size()) {
			decisionValue = cheatValues.get(cheaterPosition++);
			csb.append(decisionValue + " ");
		} else {
			throw new CheaterException("Not enough cheat values to complete all sentences. ");
		}
		

//		for (FeatureFunction feature : featureVector) {
//			final FeatureValue featureValue = feature.getFeatureValue();
//			if (!(excludeNullValues == true && featureValue.isNullValue())) {
//				if (featureValue instanceof SingleFeatureValue) {
//					if (((SingleFeatureValue)featureValue).getCode() < cardinalities[i]) {
//						csb.append((((SingleFeatureValue)featureValue).getCode() + offset) + ":" + "1 ");
//					}
//				} else if (featureValue instanceof MultipleFeatureValue) {
//					for (Integer value : ((MultipleFeatureValue)featureValue).getCodes()) {
//						if (value < cardinalities[i]) {
//							csb.append((value + offset) + ":" + "1 ");
//						}
//					}
//				}
//			}
//			offset += cardinalities[i];
//			i++;
//		}
		csb.setLength(csb.length()-1);
		csb.append('\n');
		try {
			cheaterWriter.write(csb.toString());
			cheaterWriter.flush();
		} catch (Exception e) {
			throw new CheaterException("", e);
		}
		try {
			decision.getKBestList().add(decisionValue);
		} catch (Exception e) {
			decision.getKBestList().add(-1);
		}
		return true;
	}
	
	public void finalizeSentence(DependencyStructure dependencyGraph) throws MaltChainedException { }
	
	public void moveAllInstances(LearningMethod method, FeatureFunction divideFeature, ArrayList<Integer> divideFeatureIndexVector) throws MaltChainedException { }

	public void noMoreInstances() throws MaltChainedException {
		closeInstanceWriter();
		closeCheaterWriter();
	}
	
	public void terminate() throws MaltChainedException { 
		closeInstanceWriter();
		closeCheaterWriter();
		owner = null;
	}
	
	protected void closeCheaterWriter() throws MaltChainedException {
		try {
			if (cheaterWriter != null) {
				cheaterWriter.flush();
				cheaterWriter.close();
				cheaterWriter = null;
			}
		} catch (IOException e) {
			throw new CheaterException("The cheater learner cannot close the cheater file. ", e);
		}
	}
	
	protected void closeInstanceWriter() throws MaltChainedException {
		try {
			if (instanceOutput != null) {
				instanceOutput.flush();
				instanceOutput.close();
				instanceOutput = null;
			}
		} catch (IOException e) {
			throw new CheaterException("The cheater learner cannot close the instance file. ", e);
		}
	}
	
//	private int[] getCardinalities(FeatureVector featureVector) {
//		int[] cardinalities = new int[featureVector.size()];
//		int i = 0;
//		for (FeatureFunction feature : featureVector) {
//			cardinalities[i++] = feature.getFeatureValue().getCardinality();
//		}
//		return cardinalities;
//	}
//	
//	private void saveCardinalities(OutputStreamWriter osw, int[] cardinalities) throws MaltChainedException {
//		final BufferedWriter out = new BufferedWriter(osw);
//		try {
//			for (int i = 0, n = cardinalities.length; i < n; i++) {
//				out.write(Integer.toString(cardinalities[i]));
//				if (i < n - 1) {
//					out.write(',');
//				}
//			}
//			out.write('\n');
//			out.close();
//		} catch (IOException e) {
//			throw new CheaterException("", e);
//		}
//	}
//	
//	private int[] loadCardinalities(InputStreamReader isr) throws MaltChainedException {
//		int[] cardinalities = null;
//		try {
//			final BufferedReader in = new BufferedReader(isr); 
//			String line;
//			if ((line = in.readLine()) != null) {
//				String[] items = line.split(",");
//				cardinalities = new int[items.length];
//				for (int i = 0; i < items.length; i++) {
//					cardinalities[i] = Integer.parseInt(items[i]);
//				}
// 			}
//			in.close();
//		} catch (IOException e) {
//			throw new CheaterException("", e);
//		} catch (NumberFormatException e) {
//			throw new CheaterException("", e);
//		}
//		return cardinalities;
//	}
	
	protected void initSpecialParameters() throws MaltChainedException {
		if (getConfiguration().getOptionValue("singlemalt", "null_value") != null && getConfiguration().getOptionValue("singlemalt", "null_value").toString().equalsIgnoreCase("none")) {
			excludeNullValues = true;
		} else {
			excludeNullValues = false;
		}
		saveCheatAction = ((Boolean)getConfiguration().getOptionValue("cheater", "save_cheat_action")).booleanValue();

		if (!getConfiguration().getOptionValue("cheater", "cheater_file").toString().equals("")) {
			cheaterFileName = getConfiguration().getOptionValue("cheater", "cheater_file").toString();
		}
		if (getConfiguration().getOptionValue("liblinear", "verbosity") != null) {
			verbosity = Verbostity.valueOf(getConfiguration().getOptionValue("cheater", "verbosity").toString().toUpperCase());
		}
	}
	
	public static void maltSVMFormat2OriginalSVMFormat(InputStreamReader isr, OutputStreamWriter osw, int[] cardinalities) throws MaltChainedException {
		try {
			final BufferedReader in = new BufferedReader(isr);
			final BufferedWriter out = new BufferedWriter(osw);

			int c;
			int j = 0;
			int offset = 1;
			int code = 0;
			while(true) {
				c = in.read();
				if (c == -1) {
					break;
				}
				
				if (c == '\t' || c == '|') {
					if (j == 0) {
						out.write(Integer.toString(code));
						j++;
					} else {
						if (code != -1) {
							out.write(' ');
							out.write(Integer.toString(code+offset));
							out.write(":1");
						}
						if (c == '\t') {
							offset += cardinalities[j-1];
							j++;
						}
					}
					code = 0;
				} else if (c == '\n') {
					j = 0;
					offset = 1;
					out.write('\n');
					code = 0;
				} else if (c == '-') {
					code = -1;
				} else if (code != -1) {
					if (c > 47 && c < 58) {
						code = code * 10 + (c-48);
					} else {
						throw new CheaterException("The instance file contain a non-integer value, when converting the Malt SVM format into Liblinear format.");
					}
				}	
			}			
			in.close();	
			out.close();
		} catch (IOException e) {
			throw new CheaterException("Cannot read from the instance file, when converting the Malt SVM format into Liblinear format. ", e);
		}
	}
	
	public BufferedWriter getInstanceWriter() {
		return instanceOutput;
	}
	
	public InstanceModel getOwner() {
		return owner;
	}

	protected void setOwner(InstanceModel owner) {
		this.owner = owner;
	}
	
	public int getLearnerMode() {
		return learnerMode;
	}

	public void setLearnerMode(int learnerMode) throws MaltChainedException {
		this.learnerMode = learnerMode;
	}
	
	public String getLearningMethodName() {
		return name;
	}
	
	public DependencyParserConfig getConfiguration() throws MaltChainedException {
		return owner.getGuide().getConfiguration();
	}
	
	public int getNumberOfInstances() throws MaltChainedException {
		return numberOfInstances;
	}

	public void increaseNumberOfInstances() {
		numberOfInstances++;
		owner.increaseFrequency();
	}
	
	public void decreaseNumberOfInstances() {
		numberOfInstances--;
		owner.decreaseFrequency();
	}
	
	protected void setNumberOfInstances(int numberOfInstances) {
		this.numberOfInstances = 0;
	}

	protected void setLearningMethodName(String name) {
		this.name = name;
	}
	
	protected OutputStreamWriter getInstanceOutputStreamWriter(String suffix) throws MaltChainedException {
		return getConfiguration().getConfigurationDir().getAppendOutputStreamWriter(owner.getModelName()+getLearningMethodName()+suffix);
	}
	
	protected InputStreamReader getInstanceInputStreamReader(String suffix) throws MaltChainedException {
		return getConfiguration().getConfigurationDir().getInputStreamReader(owner.getModelName()+getLearningMethodName()+suffix);
	}
	
	protected InputStreamReader getInstanceInputStreamReaderFromConfigFile(String suffix) throws MaltChainedException {
		return getConfiguration().getConfigurationDir().getInputStreamReaderFromConfigFile(owner.getModelName()+getLearningMethodName()+suffix);
	}
	
	protected File getFile(String suffix) throws MaltChainedException {
		return getConfiguration().getConfigurationDir().getFile(owner.getModelName()+getLearningMethodName()+suffix);
	}
	
	protected JarEntry getConfigFileEntry(String suffix) throws MaltChainedException {
		return getConfiguration().getConfigurationDir().getConfigFileEntry(owner.getModelName()+getLearningMethodName()+suffix);
	}
	
	
	protected void finalize() throws Throwable {
		try {
			closeInstanceWriter();
			closeCheaterWriter();
		} finally {
			super.finalize();
		}
	}
	
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		sb.append("\nCheater INTERFACE\n");
//		sb.append("  Cheater string: "+paramString+"\n");
		

		return sb.toString();
	}
}
