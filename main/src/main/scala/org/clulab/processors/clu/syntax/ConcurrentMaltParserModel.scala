package org.clulab.processors.clu.syntax

import org.maltparser.concurrent.graph.dataformat.DataFormat
import org.maltparser.core.io.dataformat.DataFormatInstance
import org.maltparser.core.lw.parser.{LWSingleMalt, McoModel}
import org.maltparser.core.symbol.SymbolTableHandler

/**
  * This class replicates the org.maltparser.concurrent.ConcurrentMaltParserModel class, which is needed by our local ConcurrentMaltParserService.
  * This code may need to be updated whenever there is a version change in maltparser
  */
class ConcurrentMaltParserModel {
  private var dataFormatInstance:DataFormatInstance = null
  private var concurrentDataFormat:DataFormat = null
  private var parentSymbolTableHandler:SymbolTableHandler = null
  private var singleMalt:LWSingleMalt = null
  private val optionContainer:Int = 0
  private var mcoModel:McoModel = null
  private var markingStrategy:Int = 0
  private var coveredRoot:Boolean = false
  private var defaultRootLabel:String = null


}

  /*
protected ConcurrentMaltParserModel(int _optionContainer, URL _mcoURL) throws MaltChainedException {
  this.optionContainer = _optionContainer;
  this.mcoModel = new McoModel(_mcoURL);
  String inputFormatName = OptionManager.instance().getOptionValue(optionContainer, "input", "format").toString().trim();
  URL inputFormatURL = null;
  try {
  inputFormatURL = mcoModel.getMcoEntryURL(inputFormatName);
} catch(IOException e) {
  throw new MaltChainedException("Couldn't read file "+inputFormatName+" from mco-file ", e);
}
  DataFormatManager dataFormatManager = new DataFormatManager(inputFormatURL, inputFormatURL);
  this.parentSymbolTableHandler = new HashSymbolTableHandler();
  this.dataFormatInstance = dataFormatManager.getInputDataFormatSpec().createDataFormatInstance(this.parentSymbolTableHandler, OptionManager.instance().getOptionValueString(optionContainer, "singlemalt", "null_value"));
  try {
  this.parentSymbolTableHandler.load(mcoModel.getInputStreamReader("symboltables.sym", "UTF-8"));
} catch(IOException e) {
  throw new MaltChainedException("Couldn't read file symboltables.sym from mco-file ", e);
}
  this.defaultRootLabel = OptionManager.instance().getOptionValue(optionContainer, "graph", "root_label").toString().trim();
  this.markingStrategy = LWDeprojectivizer.getMarkingStrategyInt(OptionManager.instance().getOptionValue(optionContainer, "pproj", "marking_strategy").toString().trim());
  this.coveredRoot = !OptionManager.instance().getOptionValue(optionContainer, "pproj", "covered_root").toString().trim().equalsIgnoreCase("none");
  //		final PropagationManager propagationManager = loadPropagationManager(this.optionContainer, mcoModel);
  final FeatureModelManager featureModelManager = loadFeatureModelManager(this.optionContainer, mcoModel);
  this.singleMalt = new LWSingleMalt(this.optionContainer, this.dataFormatInstance, mcoModel, null, featureModelManager);
  this.concurrentDataFormat = DataFormat.parseDataFormatXMLfile(inputFormatURL);
}

  public ConcurrentDependencyGraph parse(String[] tokens) throws MaltChainedException {
  return new ConcurrentDependencyGraph(concurrentDataFormat, internalParse(tokens), defaultRootLabel);
}

  public String[] parseTokens(String[] tokens) throws MaltChainedException {
  LWDependencyGraph outputGraph = internalParse(tokens);
  String[] outputTokens = new String[tokens.length];
  for (int i = 0; i < outputTokens.length; i++) {
  outputTokens[i] = outputGraph.getDependencyNode(i+1).toString();
}
  return outputTokens;
}

  private LWDependencyGraph internalParse(String[] tokens) throws MaltChainedException {
  if (tokens == null || tokens.length == 0) {
  throw new MaltChainedException("Nothing to parse. ");
}

  LWDependencyGraph parseGraph = new LWDependencyGraph(concurrentDataFormat, new ParseSymbolTableHandler(parentSymbolTableHandler), tokens, defaultRootLabel, false);

  singleMalt.parse(parseGraph);
  if (markingStrategy != 0 || coveredRoot) {
  new LWDeprojectivizer().deprojectivize(parseGraph, markingStrategy);
}

  return parseGraph;
}

  public List<String[]> parseSentences(List<String[]> inputSentences) throws MaltChainedException {
  return singleMalt.parseSentences(inputSentences, defaultRootLabel, markingStrategy, coveredRoot, parentSymbolTableHandler, concurrentDataFormat);
}


  private FeatureModelManager loadFeatureModelManager(int optionContainer, McoModel mcoModel) throws MaltChainedException {
  final FeatureEngine system = new FeatureEngine();
  system.load("/appdata/features/ParserFeatureSystem.xml");
  system.load(PluginLoader.instance());
  FeatureModelManager featureModelManager = new FeatureModelManager(system);
  String featureModelFileName = OptionManager.instance().getOptionValue(optionContainer, "guide", "features").toString().trim();
  try {
  if (featureModelFileName.endsWith(".par")) {
  String markingStrategy = OptionManager.instance().getOptionValue(optionContainer, "pproj", "marking_strategy").toString().trim();
  String coveredRoot = OptionManager.instance().getOptionValue(optionContainer, "pproj", "covered_root").toString().trim();
  featureModelManager.loadParSpecification(mcoModel.getMcoEntryURL(featureModelFileName), markingStrategy, coveredRoot);
} else {
  featureModelManager.loadSpecification(mcoModel.getMcoEntryURL(featureModelFileName));
}
} catch(IOException e) {
  throw new MaltChainedException("Couldn't read file "+featureModelFileName+" from mco-file ", e);
}
  return featureModelManager;
}
  */