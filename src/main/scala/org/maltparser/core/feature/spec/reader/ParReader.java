package org.maltparser.core.feature.spec.reader;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.regex.Pattern;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.FeatureException;
import org.maltparser.core.feature.spec.SpecificationModels;
/**
*
*
* @author Johan Hall
*/
public class ParReader implements FeatureSpecReader {
	public enum DataStructures {
		STACK, INPUT, LEFTCONTEXT, RIGHTCONTEXT
	};
	public enum ColumnNames {
		POS, DEP, LEX, LEMMA, CPOS, FEATS
	};
	private EnumMap<ColumnNames, String> columnNameMap;
	private EnumMap<DataStructures, String> dataStructuresMap;
	private boolean useSplitFeats = true;
	private boolean covington = false;
	private boolean pppath;
	private boolean pplifted;
	private boolean ppcoveredRoot;
	
	public ParReader() throws MaltChainedException {
		initializeColumnNameMap();
		initializeDataStructuresMap();
		setPppath(false);
		setPplifted(false);
		setPpcoveredRoot(false);
	}
	
	public void load(URL specModelURL, SpecificationModels featureSpecModels) throws MaltChainedException {
		BufferedReader br = null;
		Pattern tabPattern = Pattern.compile("\t");
		if (specModelURL == null) {
			throw new FeatureException("The feature specification file cannot be found. ");
		}
		try {
			br = new BufferedReader(new InputStreamReader(specModelURL.openStream()));
		} catch (IOException e) {
			throw new FeatureException("Could not read the feature specification file '"+specModelURL.toString()+"'. ", e);
		}		
		
		if (br != null) {
			int specModelIndex = featureSpecModels.getNextIndex();
			String fileLine;
			String items[];
			StringBuilder featureText = new StringBuilder();
			String splitfeats = "";
			ArrayList<String> fileLines = new ArrayList<String>();
			ArrayList<String> orderFileLines = new ArrayList<String>();
			while (true) {
				try {
					fileLine = br.readLine();
				} catch (IOException e) {
					throw new FeatureException("Could not read the feature specification file '"+specModelURL.toString()+"'. ", e);
				}
				if (fileLine == null) {
					break;
				}
				if (fileLine.length() <= 1 && fileLine.trim().substring(0, 2).trim().equals("--")) {
					continue;
				}
				fileLines.add(fileLine);
			}
			try {
				br.close();
			} catch (IOException e) {
				throw new FeatureException("Could not close the feature specification file '"+specModelURL.toString()+"'. ", e);
			}

			for (int j = 0; j < fileLines.size(); j++) {
				orderFileLines.add(fileLines.get(j));
			}

			boolean deprel = false;
			for (int j=0; j < orderFileLines.size(); j++) {
				deprel = false;
				featureText.setLength(0);
				splitfeats = "";
				items = tabPattern.split(orderFileLines.get(j));
				if (items.length < 2) {
					throw new FeatureException("The feature specification file '"+specModelURL.toString()+"' must contain at least two columns.");
				}
				if (!(columnNameMap.containsKey(ColumnNames.valueOf(items[0].trim())) || columnNameMap.containsValue(items[0].trim()))) {
					throw new FeatureException("Column one in the feature specification file '"+specModelURL.toString()+"' contains an unknown value '"+items[0].trim()+"'. ");
				}
				if (items[0].trim().equalsIgnoreCase("DEP") || items[0].trim().equalsIgnoreCase("DEPREL")) {
					featureText.append("OutputColumn(DEPREL, ");
					deprel = true;
				} else {
					if (columnNameMap.containsKey(ColumnNames.valueOf(items[0].trim()))) {
						featureText.append("InputColumn("+columnNameMap.get(ColumnNames.valueOf(items[0].trim()))+", ");
					} else if (columnNameMap.containsValue(items[0].trim())) {
						featureText.append("InputColumn("+items[0].trim()+", ");
					}
					if (items[0].trim().equalsIgnoreCase("FEATS") && isUseSplitFeats()) {
						splitfeats = "Split(";
					}
				}
				if (!(items[1].trim().equalsIgnoreCase("STACK") || items[1].trim().equalsIgnoreCase("INPUT") || items[1].trim().equalsIgnoreCase("CONTEXT"))) {
					throw new FeatureException("Column two in the feature specification file '"+specModelURL.toString()+"' should be either 'STACK', 'INPUT' or 'CONTEXT' (Covington), not '"+items[1].trim()+"'. ");
				}
				int offset = 0;
				if (items.length >= 3) {
					try {
						offset = new Integer(Integer.parseInt(items[2]));
					} catch (NumberFormatException e) {
						throw new FeatureException("The feature specification file '"+specModelURL.toString()+"' contains a illegal integer value. ", e);
					}
				}
				String functionArg = "";
				
				if (items[1].trim().equalsIgnoreCase("CONTEXT")) {
					if (offset >= 0) {
						functionArg = dataStructuresMap.get(DataStructures.valueOf("LEFTCONTEXT"))+"["+offset+"]";
					} else {
						functionArg = dataStructuresMap.get(DataStructures.valueOf("RIGHTCONTEXT"))+"["+Math.abs(offset + 1)+"]";
					}
				} else if (dataStructuresMap.containsKey(DataStructures.valueOf(items[1].trim()))) {
					if (covington == true) {
						if (dataStructuresMap.get(DataStructures.valueOf(items[1].trim())).equalsIgnoreCase("Stack")) {
							functionArg = "Left["+offset+"]";
						} else {
							functionArg = "Right["+offset+"]";
						}
					} else {
						functionArg = dataStructuresMap.get(DataStructures.valueOf(items[1].trim()))+"["+offset+"]";
					}
				} else if (dataStructuresMap.containsValue(items[1].trim())) {
					if (covington == true) {
						if (items[1].trim().equalsIgnoreCase("Stack")) {
							functionArg = "Left["+offset+"]";
						} else {
							functionArg = "Right["+offset+"]";
						}
					} else {
						functionArg = items[1].trim()+"["+offset+"]";
					}
					
				} else {
					throw new FeatureException("Column two in the feature specification file '"+specModelURL.toString()+"' should not contain the value '"+items[1].trim());
				}
	
				int linearOffset = 0;
				int headOffset = 0;
				int depOffset = 0;
				int sibOffset = 0;
				int suffixLength = 0;
				if (items.length >= 4) { linearOffset = new Integer(Integer.parseInt(items[3])); }
				if (items.length >= 5) { headOffset = new Integer(Integer.parseInt(items[4])); }
				if (items.length >= 6) { depOffset = new Integer(Integer.parseInt(items[5])); }
				if (items.length >= 7) { sibOffset = new Integer(Integer.parseInt(items[6])); }
				if (items.length >= 8) { suffixLength = new Integer(Integer.parseInt(items[7])); }
				if (linearOffset < 0) {
					linearOffset = Math.abs(linearOffset);
					for (int i = 0; i < linearOffset; i++) {
						functionArg = "pred("+functionArg+")"; 
					}
				} else if (linearOffset > 0) {
					for (int i = 0; i < linearOffset; i++) {
						functionArg = "succ("+functionArg+")"; 
					}
				} 
				if (headOffset >= 0) {
					for (int i = 0; i < headOffset; i++) {
						functionArg = "head("+functionArg+")"; 
					}
				} else {
					throw new FeatureException("The feature specification file '"+specModelURL.toString()+"' should not contain a negative head function value. ");
				}
				if (depOffset < 0) {
					depOffset = Math.abs(depOffset);
					for (int i = 0; i < depOffset; i++) {
						functionArg = "ldep("+functionArg+")"; 
					}
				} else if (depOffset > 0) {
					for (int i = 0; i < depOffset; i++) {
						functionArg = "rdep("+functionArg+")";
					}							
				}
				if (sibOffset < 0) {
					sibOffset = Math.abs(sibOffset);
					for (int i = 0; i < sibOffset; i++) {
						functionArg = "lsib("+functionArg+")"; 
					}
				} else if (sibOffset > 0) {
					for (int i = 0; i < sibOffset; i++) {
						functionArg = "rsib("+functionArg+")"; 
					}							
				}
				
				if (deprel == true && (pppath == true || pplifted == true || ppcoveredRoot == true)) {
					featureSpecModels.add(specModelIndex, mergePseudoProjColumns(functionArg));
				} else {
					if (suffixLength != 0) {
						featureSpecModels.add(specModelIndex, "Suffix("+featureText.toString()+functionArg+"),"+suffixLength+")");
					} else if (splitfeats.equals("Split(")) {
						featureSpecModels.add(specModelIndex, splitfeats+featureText.toString()+functionArg+"),\\|)");
					} else {
						featureSpecModels.add(specModelIndex, featureText.toString()+functionArg+")");
					}
				}

			}
		}
	}

	private String mergePseudoProjColumns(String functionArg) {
		StringBuilder newFeatureText = new StringBuilder();
		int c = 1; 
		
		if (pplifted == true) { c++; };
		if (pppath == true) { c++; };
		if (ppcoveredRoot == true) { c++; };
		
		if (c == 1) { // no merge
			newFeatureText.append("OutputColumn(DEPREL, ");
			newFeatureText.append(functionArg);
			newFeatureText.append(')');
			return newFeatureText.toString();
		}
		if (c == 2) {
			newFeatureText.append("Merge(");
			newFeatureText.append("OutputColumn(DEPREL, ");
			newFeatureText.append(functionArg);
			newFeatureText.append("), ");
			if (pplifted == true) {
				newFeatureText.append("OutputTable(PPLIFTED, ");
				newFeatureText.append(functionArg);
				newFeatureText.append(")");
			}
			if (pppath == true) {
				newFeatureText.append("OutputTable(PPPATH, ");
				newFeatureText.append(functionArg);
				newFeatureText.append(")");
			}
			if (ppcoveredRoot == true) {
				newFeatureText.append("OutputTable(PPCOVERED, ");
				newFeatureText.append(functionArg);
				newFeatureText.append(")");
			}
			newFeatureText.append(")");
		} else if (c == 3) { // use Merge3 
			int i = 0;
			newFeatureText.append("Merge3(");
			newFeatureText.append("OutputColumn(DEPREL, ");
			newFeatureText.append(functionArg);
			newFeatureText.append("), ");
			i++;
			if (pplifted == true) {
				newFeatureText.append("OutputTable(PPLIFTED, ");
				newFeatureText.append(functionArg);
				i++;
				if (i<3) { 
					newFeatureText.append("), ");
				} else {
					newFeatureText.append(")");
				}
			}
			if (pppath == true) {
				newFeatureText.append("OutputTable(PPPATH, ");
				newFeatureText.append(functionArg);
				i++;
				if (i<3) { 
					newFeatureText.append("), ");
				} else {
					newFeatureText.append(")");
				}
			}
			if (ppcoveredRoot == true) {
				newFeatureText.append("OutputTable(PPCOVERED, ");
				newFeatureText.append(functionArg);
				i++;
				if (i<3) { 
					newFeatureText.append("), ");
				} else {
					newFeatureText.append(")");
				}
			}
			newFeatureText.append(")");
		} else { // c == 4
			newFeatureText.append("Merge(Merge(");
			newFeatureText.append("OutputColumn(DEPREL, ");
			newFeatureText.append(functionArg);
			newFeatureText.append("), ");
			newFeatureText.append("OutputTable(PPLIFTED, ");
			newFeatureText.append(functionArg);
			newFeatureText.append(")), Merge(");
			newFeatureText.append("OutputTable(PPPATH, ");
			newFeatureText.append(functionArg);
			newFeatureText.append("), ");
			newFeatureText.append("OutputTable(PPCOVERED, ");
			newFeatureText.append(functionArg);
			newFeatureText.append(")))");
		}
		return newFeatureText.toString();
	}
	
	public EnumMap<ColumnNames, String> getColumnNameMap() {
		return columnNameMap;
	}

	public void initializeColumnNameMap() {
		columnNameMap = new EnumMap<ColumnNames, String>(ColumnNames.class);
		columnNameMap.put(ColumnNames.POS, "POSTAG");
		columnNameMap.put(ColumnNames.CPOS, "CPOSTAG");
		columnNameMap.put(ColumnNames.DEP, "DEPREL");
		columnNameMap.put(ColumnNames.LEX, "FORM");
		columnNameMap.put(ColumnNames.LEMMA, "LEMMA");
		columnNameMap.put(ColumnNames.FEATS, "FEATS");
	}

	public void setColumnNameMap(EnumMap<ColumnNames, String> columnNameMap) {
		this.columnNameMap = columnNameMap;
	}
	
	public EnumMap<DataStructures, String> getDataStructuresMap() {
		return dataStructuresMap;
	}

	//TODO Fix covington
	public void initializeDataStructuresMap() {
		dataStructuresMap = new EnumMap<DataStructures, String>(DataStructures.class);
		dataStructuresMap.put(DataStructures.STACK, "Stack");
		dataStructuresMap.put(DataStructures.INPUT, "Input");
	}

	public void setDataStructuresMap(EnumMap<DataStructures, String> dataStructuresMap) {
		this.dataStructuresMap = dataStructuresMap;
	}
	
	public boolean isUseSplitFeats() {
		return useSplitFeats;
	}

	public void setUseSplitFeats(boolean useSplitFeats) {
		this.useSplitFeats = useSplitFeats;
	}

	public boolean isCovington() {
		return covington;
	}

	public void setCovington(boolean covington) {
		this.covington = covington;
	}

	public boolean isPppath() {
		return pppath;
	}

	public void setPppath(boolean pppath) {
		this.pppath = pppath;
	}

	public boolean isPplifted() {
		return pplifted;
	}

	public void setPplifted(boolean pplifted) {
		this.pplifted = pplifted;
	}

	public boolean isPpcoveredRoot() {
		return ppcoveredRoot;
	}

	public void setPpcoveredRoot(boolean ppcoveredRoot) {
		this.ppcoveredRoot = ppcoveredRoot;
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("Mapping of column names:\n");
		for (ColumnNames columnName : ColumnNames.values()) {
			sb.append(columnName.toString()+"\t"+columnNameMap.get(columnName)+"\n");
		}
		sb.append("Mapping of data structures:\n");
		for (DataStructures dataStruct : DataStructures.values()) {
			sb.append(dataStruct.toString()+"\t"+dataStructuresMap.get(dataStruct)+"\n");
		}
		sb.append("Split FEATS column: "+useSplitFeats+"\n");
		return sb.toString();
	}
}
