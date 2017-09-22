package org.clulab.processors.clu.syntax

import java.net.URL

import org.maltparser.concurrent.graph.ConcurrentDependencyGraph
import org.maltparser.concurrent.graph.dataformat.DataFormat
import org.maltparser.core.exception.MaltChainedException
import org.maltparser.core.feature.FeatureModelManager
import org.maltparser.core.feature.system.FeatureEngine
import org.maltparser.core.io.dataformat.{DataFormatInstance, DataFormatManager}
import org.maltparser.core.lw.graph.{LWDependencyGraph, LWDeprojectivizer}
import org.maltparser.core.lw.parser.{LWSingleMalt, McoModel}
import org.maltparser.core.options.OptionManager
import org.maltparser.core.plugin.PluginLoader
import org.maltparser.core.symbol.SymbolTableHandler
import org.maltparser.core.symbol.hash.HashSymbolTableHandler
import org.maltparser.core.symbol.parse.ParseSymbolTableHandler

/**
  * This class replicates the org.maltparser.concurrent.ConcurrentMaltParserModel class, which is needed by our local ConcurrentMaltParserService.
  * This code may need to be updated whenever there is a version change in maltparser
  */
class ConcurrentMaltParserModel {
  private var dataFormatInstance:DataFormatInstance = null
  private var concurrentDataFormat:DataFormat = null
  private var parentSymbolTableHandler:SymbolTableHandler = null
  private var singleMalt:LWSingleMalt = null
  private var optionContainer:Int = 0
  private var mcoModel:McoModel = null
  private var markingStrategy:Int = 0
  private var coveredRoot:Boolean = false
  private var defaultRootLabel:String = null

  def parse(tokens: Array[String]): ConcurrentDependencyGraph = {
    new ConcurrentDependencyGraph(concurrentDataFormat, internalParse(tokens), defaultRootLabel)
  }

  def parseTokens(tokens: Array[String]): Array[String] = {
    val outputGraph = internalParse(tokens)
    val outputTokens = new Array[String](tokens.length)
    for(i <- outputTokens.indices) {
      outputTokens(i) = outputGraph.getDependencyNode(i+1).toString
    }
    outputTokens
  }

  private def internalParse(tokens: Array[String]): LWDependencyGraph = {
    if(tokens == null || tokens.length == 0)
      throw new MaltChainedException("Nothing to parse.")

    val parseGraph = new LWDependencyGraph(concurrentDataFormat,
      new ParseSymbolTableHandler(parentSymbolTableHandler), tokens, defaultRootLabel, false)

    singleMalt.parse(parseGraph)
    
    if (markingStrategy != 0 || coveredRoot) {
      new LWDeprojectivizer().deprojectivize(parseGraph, markingStrategy)
    }

    parseGraph
  }
}

object ConcurrentMaltParserModel {
  def apply(_optionContainer:Int, _mcoURL:URL): ConcurrentMaltParserModel = {
    val m = new ConcurrentMaltParserModel()
    m.optionContainer = _optionContainer
    m.mcoModel = new McoModel(_mcoURL)
    val inputFormatName = OptionManager.instance().getOptionValue(m.optionContainer, "input", "format").toString.trim
    val inputFormatURL:URL = m.mcoModel.getMcoEntryURL(inputFormatName)
    val dataFormatManager = new DataFormatManager(inputFormatURL, inputFormatURL)
    m.parentSymbolTableHandler = new HashSymbolTableHandler()
    m.dataFormatInstance = dataFormatManager.getInputDataFormatSpec.createDataFormatInstance(
      m.parentSymbolTableHandler,
      OptionManager.instance().getOptionValueString(m.optionContainer, "singlemalt", "null_value"))
    m.parentSymbolTableHandler.load(m.mcoModel.getInputStreamReader("symboltables.sym", "UTF-8"))
    m.defaultRootLabel = OptionManager.instance().getOptionValue(m.optionContainer, "graph", "root_label").toString.trim
    m.markingStrategy = LWDeprojectivizer.getMarkingStrategyInt(OptionManager.instance().getOptionValue(m.optionContainer, "pproj", "marking_strategy").toString.trim)
    m.coveredRoot = !OptionManager.instance().getOptionValue(m.optionContainer, "pproj", "covered_root").toString.trim.equalsIgnoreCase("none")
    val featureModelManager = loadFeatureModelManager(m.optionContainer, m.mcoModel)
    m.singleMalt = new LWSingleMalt(m.optionContainer, m.dataFormatInstance, m.mcoModel, null, featureModelManager)
    m.concurrentDataFormat = DataFormat.parseDataFormatXMLfile(inputFormatURL)
    m
  }

  private def loadFeatureModelManager(optionContainer:Int, mcoModel:McoModel): FeatureModelManager = {
    val system = new FeatureEngine
    system.load("/appdata/features/ParserFeatureSystem.xml")
    system.load(PluginLoader.instance())
    val featureModelManager = new FeatureModelManager(system)
    val featureModelFileName = OptionManager.instance().getOptionValue(optionContainer, "guide", "features").toString.trim
    if (featureModelFileName.endsWith(".par")) {
      val markingStrategy = OptionManager.instance().getOptionValue(optionContainer, "pproj", "marking_strategy").toString.trim
      val coveredRoot = OptionManager.instance().getOptionValue(optionContainer, "pproj", "covered_root").toString.trim
      featureModelManager.loadParSpecification(mcoModel.getMcoEntryURL(featureModelFileName), markingStrategy, coveredRoot)
    } else {
      featureModelManager.loadSpecification(mcoModel.getMcoEntryURL(featureModelFileName))
    }
    featureModelManager
  }
}
