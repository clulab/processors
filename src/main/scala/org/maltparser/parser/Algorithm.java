package org.maltparser.parser;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import org.maltparser.core.config.ConfigurationException;
import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.symbol.SymbolTable;
import org.maltparser.core.syntaxgraph.DependencyStructure;
import org.maltparser.core.syntaxgraph.edge.Edge;
import org.maltparser.core.syntaxgraph.node.DependencyNode;
import org.maltparser.parser.guide.ClassifierGuide;
/**
 * @author Johan Hall
 *
 */
public abstract class Algorithm {
	protected DependencyParserConfig manager;
	protected ClassifierGuide classifierGuide;
	protected ParserState parserState;
	protected ParserConfiguration currentParserConfiguration;
	protected boolean diagnostics = false;
	protected BufferedWriter diaWriter;
	/**
	 * Creates a parsing algorithm
	 * 
	 * @param manager a reference to the single malt configuration
	 * @throws MaltChainedException
	 */
	public Algorithm(DependencyParserConfig manager) throws MaltChainedException {
		setManager(manager);
		setDiagnostics((Boolean)manager.getOptionValue("singlemalt", "diagnostics"));
		if (diagnostics) {
			openDiaWriter(manager.getOptionValue("singlemalt", "diafile").toString());
		}
	}
	
	public abstract void terminate() throws MaltChainedException;
	
	public boolean isDiagnostics() {
		return diagnostics;
	}

	public void setDiagnostics(boolean diagnostics) {
		this.diagnostics = diagnostics;
	}


	public BufferedWriter getDiaWriter() {
		return diaWriter;
	}
	
	public void writeToDiaFile(String message) throws MaltChainedException {
		try {
			getDiaWriter().write(message);
		} catch (IOException e) {
			throw new MaltChainedException("Could not write to the diagnostic file. ", e);
		}
	}
	
	public void closeDiaWriter() throws MaltChainedException {
		if (diaWriter != null) {
			try {
				diaWriter.flush();
				diaWriter.close();
			} catch (IOException e) {
				throw new MaltChainedException("Could not close the diagnostic file. ", e);
			}
		}
	}
	
	public void openDiaWriter(String fileName) throws MaltChainedException {
		if (diagnostics) {
			try {
				if (fileName.equals("stdout")) {
					diaWriter = new BufferedWriter(new OutputStreamWriter(System.out));
				} else if (fileName.equals("stderr")) {
					diaWriter = new BufferedWriter(new OutputStreamWriter(System.err));
				} else {
					diaWriter = new BufferedWriter(new FileWriter(fileName));
				}
			} catch (IOException e) {
				throw new MaltChainedException("Could not open the diagnostic file. ", e);
			}
		}
	}

	/**
	 * Returns the classifier guide.
	 * 
	 * @return the classifier guide
	 */
	public ClassifierGuide getGuide() {
		return classifierGuide;
	}
	
	/**
	 * Sets the classifier guide
	 * 
	 * @param guide a classifier guide
	 */
	public void setGuide(ClassifierGuide guide) {
		this.classifierGuide = guide;
	}

	/**
	 * Returns the current active parser configuration
	 * 
	 * @return the current active parser configuration
	 */
	public ParserConfiguration getCurrentParserConfiguration() {
		return currentParserConfiguration;
	}
	
	/**
	 * Sets the current parser configuration
	 * 
	 * @param currentParserConfiguration a parser configuration
	 */
	protected void setCurrentParserConfiguration(ParserConfiguration currentParserConfiguration) {
		this.currentParserConfiguration = currentParserConfiguration;
	}
	
	/**
	 * Returns the parser state
	 * 
	 * @return the parser state
	 */
	public ParserState getParserState() {
		return parserState;
	}
	
	/**
	 * Sets the parser state
	 * 
	 * @param parserState a parser state
	 */
	protected void setParserState(ParserState parserState) {
		this.parserState = parserState;
	}

	/**
	 * Creates a parser factory specified by the --singlemalt-parsing_algorithm option
	 * 
	 * @return a parser factory
	 * @throws MaltChainedException
	 */
	protected AbstractParserFactory makeParserFactory() throws MaltChainedException {
		Class<?> clazz = (Class<?>)manager.getOptionValue("singlemalt", "parsing_algorithm");
		try {	
			Class<?>[] params = new Class<?>[1];
			params[0] = org.maltparser.parser.Algorithm.class;
			Object[] arguments = new Object[params.length];
			arguments[0] = this;
			Constructor<?> constructor = clazz.getConstructor(params);
			return (AbstractParserFactory)constructor.newInstance(arguments);
		} catch (NoSuchMethodException e) {
			throw new ConfigurationException("The parser factory '"+clazz.getName()+"' cannot be initialized. ", e);
		} catch (InstantiationException e) {
			throw new ConfigurationException("The parser factory '"+clazz.getName()+"' cannot be initialized. ", e);
		} catch (IllegalAccessException e) {
			throw new ConfigurationException("The parser factory '"+clazz.getName()+"' cannot be initialized. ", e);
		} catch (InvocationTargetException e) {
			throw new ConfigurationException("The parser factory '"+clazz.getName()+"' cannot be initialized. ", e);			
		}
	}
	
	protected void initParserState(int k) throws MaltChainedException {
		AbstractParserFactory parserFactory = makeParserFactory();
		((SingleMalt)manager).addRegistry(parserFactory.getClass(), parserFactory);
		parserState = new ParserState(this, parserFactory, k);
	}
	
	/**
	 * Returns the single malt configuration
	 * 
	 * @return the single malt configuration
	 */
	public DependencyParserConfig getManager() {
		return manager;
	}

	/**
	 * Sets the single malt configuration
	 * 
	 * @param manager a single malt configuration
	 */
	public void setManager(DependencyParserConfig manager) {
		this.manager = manager;
	}
	
	/**
	 * Copies the edges of the source dependency structure to the target dependency structure
	 * 
	 * @param source a source dependency structure
	 * @param target a target dependency structure
	 * @throws MaltChainedException
	 */
	protected void copyEdges(DependencyStructure source, DependencyStructure target) throws MaltChainedException {
		for (int index : source.getTokenIndices()) {
			DependencyNode snode = source.getTokenNode(index);
			
			if (snode.hasHead()) {
				Edge s = snode.getHeadEdge();
				Edge t = target.addDependencyEdge(s.getSource().getIndex(), s.getTarget().getIndex());
				
				for (SymbolTable table : s.getLabelTypes()) {
					t.addLabel(table, s.getLabelSymbol(table));
				}
			}
		}
	}
	
	protected void copyDynamicInput(DependencyStructure source, DependencyStructure target) throws MaltChainedException {
		for (int index : source.getTokenIndices()) {
			DependencyNode snode = source.getTokenNode(index);
			DependencyNode tnode = target.getTokenNode(index);
			for (SymbolTable table : snode.getLabelTypes()) {
				if (!tnode.hasLabel(table)) {
					tnode.addLabel(table,snode.getLabelSymbol(table));
				}
			}
		}
	}
}
