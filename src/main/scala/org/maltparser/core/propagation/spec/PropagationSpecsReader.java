package org.maltparser.core.propagation.spec;

import java.io.IOException;
import java.net.URL;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.propagation.PropagationException;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * @author Johan Hall
 *
 */
public class PropagationSpecsReader {
	public PropagationSpecsReader() { }
	
	public void load(URL url, PropagationSpecs propagationSpecs) throws MaltChainedException {
        try {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            Element root = null;

            root = db.parse(url.openStream()).getDocumentElement();

            if (root == null) {
            	throw new PropagationException("The propagation specification file '"+url.getFile()+"' cannot be found. ");
            }
            
            readPropagationSpecs(root, propagationSpecs);
        } catch (IOException e) {
        	throw new PropagationException("The propagation specification file '"+url.getFile()+"' cannot be found. ", e);
        } catch (ParserConfigurationException e) {
        	throw new PropagationException("Problem parsing the file "+url.getFile()+". ", e);
        } catch (SAXException e) {
        	throw new PropagationException("Problem parsing the file "+url.getFile()+". ", e);
        }
	}
	
	private void readPropagationSpecs(Element propagations, PropagationSpecs propagationSpecs) throws MaltChainedException {
		NodeList propagationList = propagations.getElementsByTagName("propagation");
		for (int i = 0; i < propagationList.getLength(); i++) {
			readPropagationSpec((Element)propagationList.item(i), propagationSpecs);
		}
	}
	
	private void readPropagationSpec(Element propagation, PropagationSpecs propagationSpecs) throws MaltChainedException {
		int nFrom = propagation.getElementsByTagName("from").getLength();
		if (nFrom < 1 && nFrom > 1) {
			throw new PropagationException("Propagation specification wrongly formatted: Number of 'from' elements is '"+nFrom+"', must be 1.");
		}
		
		int nTo = propagation.getElementsByTagName("to").getLength();
		if (nTo < 1 && nTo > 1) {
			throw new PropagationException("Propagation specification wrongly formatted: Number of 'to' elements is '"+nTo+"', must be 1.");
		}
		
		int nFor = propagation.getElementsByTagName("for").getLength();
		if (nFor > 1) {
			throw new PropagationException("Propagation specification wrongly formatted: Number of 'for' elements is '"+nFor+"', at most 1.");
		}
		
		int nOver = propagation.getElementsByTagName("over").getLength();
		if (nOver > 1) {
			throw new PropagationException("Propagation specification wrongly formatted: Number of 'over' elements is '"+nOver+"',at most 1.");
		}
		String fromText = ((Element)propagation.getElementsByTagName("from").item(0)).getTextContent().trim();
		if (fromText.length() == 0) {
			throw new PropagationException("Propagation specification wrongly formatted: The 'from' element is empty");
		}
		String toText = ((Element)propagation.getElementsByTagName("to").item(0)).getTextContent().trim();
		if (toText.length() == 0) {
			throw new PropagationException("Propagation specification wrongly formatted: The 'to' element is empty");
		}
		String forText = "";
		if (nFor != 0) {
			forText = ((Element)propagation.getElementsByTagName("for").item(0)).getTextContent().trim();
		}
		String overText = "";
		if (nOver != 0) {
			overText = ((Element)propagation.getElementsByTagName("over").item(0)).getTextContent().trim();
		}
		propagationSpecs.add(new PropagationSpec(fromText, toText, forText, overText));
	}
}
