package org.clulab.processors.bionlp.ner;

import edu.stanford.nlp.ie.NERFeatureFactory;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.sequences.Clique;
import edu.stanford.nlp.util.PaddedList;

import java.util.Collection;

/**
 * Feature factory for the Bio NER based on the Stanford CRF
 * User: mihais
 * Date: 2/27/15
 */
public class BioNERFactory <IN extends CoreLabel> extends NERFeatureFactory<IN> {
  @Override
  public Collection<String> getCliqueFeatures(PaddedList<IN> sentence, int tokenPosition, Clique clique) {
    Collection<String> feats = super.getCliqueFeatures(sentence, tokenPosition, clique);

    // add your own custom features here
    // info is the current sentence; the current token is at position

    return feats;
  }
}
