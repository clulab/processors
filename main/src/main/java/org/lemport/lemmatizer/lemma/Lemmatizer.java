//package lemma;
package org.lemport.lemmatizer.lemma;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.InvalidPropertiesFormatException;
import java.util.Properties;

import javax.xml.parsers.ParserConfigurationException;

import org.lemport.lemmatizer.lexicon.Lexicon;
import org.lemport.lemmatizer.normalization.AdverbNormalizer;
import org.lemport.lemmatizer.normalization.AugmentativeNormalizer;
import org.lemport.lemmatizer.normalization.DiminutiveNormalizer;
import org.lemport.lemmatizer.normalization.GenderNameNormalizer;
import org.lemport.lemmatizer.normalization.GenderNormalizer;
import org.lemport.lemmatizer.normalization.NumberNormalizer;
import org.lemport.lemmatizer.normalization.SuperlativeNormalizer;
import org.lemport.lemmatizer.normalization.VerbNormalizer;

import org.xml.sax.SAXException;

import org.lemport.lemmatizer.dictionary.Dictionary;
import org.lemport.lemmatizer.dictionary.DictionaryLoadException;
import org.lemport.lemmatizer.rank.WordRanking;
import org.lemport.lemmatizer.rank.WordRankingLoadException;
import org.lemport.lemmatizer.replacement.Replacement;

/**
 * This class ...
 *
 * @author   Ricardo Rodrigues
 * @version  0.9.9
 */
public class Lemmatizer {
  private static final String DEFAULT_PROP =
      //"resources/config/lemport.properties";
      "org/lemport/lemmatizer/config/lemport.properties";

  /**
   * This field...
   */
  public static final int AUGMENTATIVE = 1;         // Binary 00000001

  /**
   * This field...
   */
  public static final int SUPERLATIVE = 2;          // Binary 00000010

  /**
   * This field...
   */
  public static final int DIMINUTIVE = 4;           // Binary 00000100

  /**
   * This field...
   */
  public static final int GENDER_DECLENSIONS = 8;   // Binary 00001000

  /**
   * This field...
   */
  public static final int GENDER_NAMES = 16;        // Binary 00010000

  /**
   * This field...
   */
  public static final int NUMBER = 32;              // Binary 00100000

  /**
   * This field...
   */
  public static final int ADVERB = 64;              // Binary 01000000

  /**
   * This field...
   */
  public static final int VERB = 128;               // Binary 10000000

  /**
   * This field...
   */
  public static final int ALL = 255;                // Binary 11111111

  private AugmentativeNormalizer augmentativeNormalizer = null;
  private SuperlativeNormalizer superlativeNormalizer = null;
  private DiminutiveNormalizer diminutiveNormalizer = null;
  private GenderNameNormalizer genderNameNormalizer = null;
  private GenderNormalizer genderNormalizer = null;
  private NumberNormalizer numberNormalizer = null;
  private AdverbNormalizer adverbNormalizer = null;
  private VerbNormalizer verbNormalizer = null;
  private String augmentativeTag = null;
  private String superlativeTag = null;
  private String diminutiveTag = null;
  private String genderTag = null;
  private String numberTag = null;
  private String adverbTag = null;
  private String verbTag = null;
  private int flags = 0;                            // Binary 000000000
  private int cacheSize = 0;
  private boolean breakOnHyphen = false;
  private boolean breakOnUnderscore = false;
  private LemmatizerCache cache = null;
  private Dictionary dictionary = null;
  private Lexicon lexicon = null;
  private WordRanking ranking = null;
  private String dictionaryExclusions = null;
  private HashMap<String, String> lexiconConversions = null;

  /**
   * Creates a new <code>Lemmatizer</code> object ...
   * 
   * @throws NumberFormatException ...
   * @throws InvalidPropertiesFormatException ...
   * @throws IOException ...
   * @throws ParserConfigurationException ...
   * @throws SAXException ...
   * @throws DictionaryLoadException ...
   * @throws WordRankingLoadException ...
   */
  public Lemmatizer()
      throws NumberFormatException, InvalidPropertiesFormatException,
      IOException, ParserConfigurationException, SAXException,
      DictionaryLoadException, WordRankingLoadException {
    this(ALL, Short.MAX_VALUE);
  }

  /**
   * Creates a new <code>Lemmatizer</code> object ...
   * 
   * @param  flags ...
   * @throws NumberFormatException ...
   * @throws InvalidPropertiesFormatException ...
   * @throws IOException ...
   * @throws ParserConfigurationException ...
   * @throws SAXException ...
   * @throws DictionaryLoadException ...
   * @throws WordRankingLoadException ...
   */
  public Lemmatizer(int flags)
      throws NumberFormatException, InvalidPropertiesFormatException,
      IOException, ParserConfigurationException, SAXException,
      DictionaryLoadException, WordRankingLoadException {
    this(flags, Short.MAX_VALUE);
  }

  /**
   * Creates a new <code>Lemmatizer</code> object ...
   * 
   * @param  flags ...
   * @param  cacheSize ...
   * @throws NumberFormatException ...
   * @throws InvalidPropertiesFormatException ...
   * @throws IOException ...
   * @throws ParserConfigurationException ...
   * @throws SAXException ...
   * @throws DictionaryLoadException ...
   * @throws WordRankingLoadException ...
   */
  public Lemmatizer(int flags, int cacheSize)
      throws NumberFormatException, InvalidPropertiesFormatException,
      IOException, ParserConfigurationException, SAXException,
      DictionaryLoadException, WordRankingLoadException {
    this(flags, cacheSize, true, true);
  }


  /**
   * Creates a new <code>Lemmatizer</code> object ...
   * 
   * @param  flags ...
   * @param  cacheSize ...
   * @param  breakOnHyphen ...
   * @param  breakOnUnderscore ...
   * @throws InvalidPropertiesFormatException ...
   * @throws IOException ...
   * @throws ParserConfigurationException ...
   * @throws SAXException ...
   * @throws DictionaryLoadException ...
   * @throws NumberFormatException ...
   * @throws WordRankingLoadException ...
   */
  public Lemmatizer(int flags, int cacheSize, boolean breakOnHyphen,
      boolean breakOnUnderscore)
          throws InvalidPropertiesFormatException, IOException,
          ParserConfigurationException, SAXException,
          DictionaryLoadException, NumberFormatException,
          WordRankingLoadException {
    Properties properties = new Properties();
    properties.load(
        this.getClass().getClassLoader().getResourceAsStream(DEFAULT_PROP));
    InputStream adverbDeclensionInput =
        this.getClass().getClassLoader().getResourceAsStream(
            properties.getProperty("adverbDeclensions"));
    InputStream augmentativeDeclensionInput =
        this.getClass().getClassLoader().getResourceAsStream(
            properties.getProperty("augmentativeDeclensions"));
    InputStream diminutiveDeclensionInput =
        this.getClass().getClassLoader().getResourceAsStream(
            properties.getProperty("diminutiveDeclensions"));
    InputStream genderDeclensionInput =
        this.getClass().getClassLoader().getResourceAsStream(
            properties.getProperty("genderDeclensions"));
    InputStream genderNameInput =
        this.getClass().getClassLoader().getResourceAsStream(
            properties.getProperty("genderNames"));
    InputStream numberDeclensionInput =
        this.getClass().getClassLoader().getResourceAsStream(
            properties.getProperty("numberDeclensions"));
    InputStream superlativeDeclensionInput = 
        this.getClass().getClassLoader().getResourceAsStream(
            properties.getProperty("superlativeDeclensions"));
    InputStream irregularVerbConjugationInput =
        this.getClass().getClassLoader().getResourceAsStream(
            properties.getProperty("irregularVerbConjugations"));
    InputStream regularVerbLexemeInput =
        this.getClass().getClassLoader().getResourceAsStream(
            properties.getProperty("regularVerbLexemes"));
    InputStream regularVerbDeclensionInput =
        this.getClass().getClassLoader().getResourceAsStream(
            properties.getProperty("regularVerbDeclensions"));
    InputStream dictionaryInput =
        this.getClass().getClassLoader().getResourceAsStream(
            properties.getProperty("dictionary"));
    InputStream customDictionaryInput =
        this.getClass().getClassLoader().getResourceAsStream(
            properties.getProperty("customDictionary"));
    InputStream wordRankingInput =
        this.getClass().getClassLoader().getResourceAsStream(
            properties.getProperty("wordRanking"));
    String dictionaryExclusions =
        properties.getProperty("dictionaryExclusions");
    HashMap<String, String> lexiconConversions = new HashMap<String, String>();
    String[] conversions = properties.getProperty(
        "lexicalConversions").split(";");
    for (String conversion : conversions) {
      if (conversion.contains(":") && (conversion.indexOf(":") > 0)
          && (conversion.indexOf(":") < conversion.length() - 1)) {
        lexiconConversions.put(conversion.substring(0, conversion.indexOf(":")),
            conversion.substring(conversion.indexOf(":") + 1));
      }
    }
    this.initialize(adverbDeclensionInput, augmentativeDeclensionInput,
        diminutiveDeclensionInput, genderDeclensionInput, genderNameInput,
        numberDeclensionInput, superlativeDeclensionInput,
        irregularVerbConjugationInput, regularVerbLexemeInput,
        regularVerbDeclensionInput, dictionaryInput, customDictionaryInput,
        wordRankingInput, dictionaryExclusions, lexiconConversions, flags,
        cacheSize, breakOnHyphen, breakOnUnderscore);
  }

  /**
   * Creates a new <code>Lemmatizer</code> object ...
   * 
   * @param  adverbDeclensionInput ...
   * @param  augmentativeDeclensionInput ...
   * @param  diminutiveDeclensionInput ...
   * @param  genderDeclensionInput ...
   * @param  genderNameInput ...
   * @param  numberDeclensionInput ...
   * @param  superlativeDeclensionInput ...
   * @param  irregularVerbConjugationInput ...
   * @param  regularVerbLexemeInput ...
   * @param  regularVerbDeclensionInput ...
   * @param  dictionaryInput ...
   * @param  customDictionaryInput ...
   * @param  wordRankingInput ...
   * @param  dictionaryExclusions ...
   * @param  lexiconConversions ...
   * @throws NumberFormatException ...
   * @throws ParserConfigurationException ...
   * @throws SAXException ...
   * @throws IOException ...
   * @throws DictionaryLoadException ...
   * @throws WordRankingLoadException ...
   */
  public Lemmatizer(InputStream adverbDeclensionInput,
      InputStream augmentativeDeclensionInput,
      InputStream diminutiveDeclensionInput,
      InputStream genderDeclensionInput,
      InputStream genderNameInput,
      InputStream numberDeclensionInput,
      InputStream superlativeDeclensionInput,
      InputStream irregularVerbConjugationInput,
      InputStream regularVerbLexemeInput,
      InputStream regularVerbDeclensionInput,
      InputStream dictionaryInput,
      InputStream customDictionaryInput,
      InputStream wordRankingInput,
      String dictionaryExclusions, HashMap<String, String> lexiconConversions)
          throws NumberFormatException, ParserConfigurationException,
          SAXException, IOException, DictionaryLoadException,
          WordRankingLoadException {
    this(adverbDeclensionInput, augmentativeDeclensionInput,
        diminutiveDeclensionInput, genderDeclensionInput, genderNameInput,
        numberDeclensionInput, superlativeDeclensionInput,
        irregularVerbConjugationInput, regularVerbLexemeInput,
        regularVerbDeclensionInput, dictionaryInput, customDictionaryInput,
        wordRankingInput, dictionaryExclusions, lexiconConversions, ALL,
        Short.MAX_VALUE);
  }

  /**
   * Creates a new <code>Lemmatizer</code> object ...
   * 
   * @param  adverbDeclensionInput ...
   * @param  augmentativeDeclensionInput ...
   * @param  diminutiveDeclensionInput ...
   * @param  genderDeclensionInput ...
   * @param  genderNameInput ...
   * @param  numberDeclensionInput ...
   * @param  superlativeDeclensionInput ...
   * @param  irregularVerbConjugationInput ...
   * @param  regularVerbLexemeInput ...
   * @param  regularVerbDeclensionInput ...
   * @param  dictionaryInput ...
   * @param  customDictionaryInput ...
   * @param  wordRankingInput ...
   * @param  dictionaryExclusions ...
   * @param  lexiconConversions ...
   * @param  flags ...
   * @param  cacheSize ...
   * @throws NumberFormatException ...
   * @throws ParserConfigurationException ...
   * @throws SAXException ...
   * @throws IOException ...
   * @throws DictionaryLoadException ...
   * @throws WordRankingLoadException ...
   */
  public Lemmatizer(InputStream adverbDeclensionInput,
      InputStream augmentativeDeclensionInput,
      InputStream diminutiveDeclensionInput,
      InputStream genderDeclensionInput,
      InputStream genderNameInput,
      InputStream numberDeclensionInput,
      InputStream superlativeDeclensionInput,
      InputStream irregularVerbConjugationInput,
      InputStream regularVerbLexemeInput,
      InputStream regularVerbDeclensionInput,
      InputStream dictionaryInput,
      InputStream customDictionaryInput,
      InputStream wordRankingInput,
      String dictionaryExclusions, HashMap<String, String> lexiconConversions,
      int flags, int cacheSize)
          throws NumberFormatException, ParserConfigurationException,
          SAXException, IOException, DictionaryLoadException,
          WordRankingLoadException {
    this(adverbDeclensionInput, augmentativeDeclensionInput,
        diminutiveDeclensionInput, genderDeclensionInput, genderNameInput,
        numberDeclensionInput, superlativeDeclensionInput,
        irregularVerbConjugationInput, regularVerbLexemeInput,
        regularVerbDeclensionInput, dictionaryInput, customDictionaryInput,
        wordRankingInput, dictionaryExclusions, lexiconConversions, flags,
        Short.MAX_VALUE, true, true);
  }

  /**
   * Creates a new <code>Lemmatizer</code> object ...
   * 
   * @param  adverbDeclensionInput ...
   * @param  augmentativeDeclensionInput ...
   * @param  diminutiveDeclensionInput ...
   * @param  genderDeclensionInput ...
   * @param  genderNameInput ...
   * @param  numberDeclensionInput ...
   * @param  superlativeDeclensionInput ...
   * @param  irregularVerbConjugationInput ...
   * @param  regularVerbLexemeInput ...
   * @param  regularVerbDeclensionInput ...
   * @param  dictionaryInput ...
   * @param  customDictionaryInput ...
   * @param  wordRankingInput ...
   * @param  dictionaryExclusions ...
   * @param  lexiconConversions ...
   * @param  flags ...
   * @param  cacheSize ...
   * @param  breakOnHyphen ...
   * @param  breakOnUnderscore ...
   * @throws ParserConfigurationException ...
   * @throws SAXException ...
   * @throws IOException ...
   * @throws DictionaryLoadException ...
   * @throws NumberFormatException ...
   * @throws WordRankingLoadException ...
   */
  public Lemmatizer(InputStream adverbDeclensionInput,
      InputStream augmentativeDeclensionInput,
      InputStream diminutiveDeclensionInput,
      InputStream genderDeclensionInput,
      InputStream genderNameInput,
      InputStream numberDeclensionInput,
      InputStream superlativeDeclensionInput,
      InputStream irregularVerbConjugationInput,
      InputStream regularVerbLexemeInput,
      InputStream regularVerbDeclensionInput,
      InputStream dictionaryInput,
      InputStream customDictionaryInput,
      InputStream wordRankingInput,
      String dictionaryExclusions, HashMap<String, String> lexiconConversions,
      int flags, int cacheSize, boolean breakOnHyphen,
      boolean breakOnUnderscore)
          throws ParserConfigurationException, SAXException, IOException,
          DictionaryLoadException, NumberFormatException,
          WordRankingLoadException {
    this.initialize(adverbDeclensionInput, augmentativeDeclensionInput,
        diminutiveDeclensionInput, genderDeclensionInput, genderNameInput,
        numberDeclensionInput, superlativeDeclensionInput,
        irregularVerbConjugationInput, regularVerbLexemeInput,
        regularVerbDeclensionInput, dictionaryInput, customDictionaryInput,
        wordRankingInput, dictionaryExclusions, lexiconConversions, flags,
        cacheSize, breakOnHyphen, breakOnUnderscore);
  }

  private void initialize(InputStream adverbDeclensionInput,
      InputStream augmentativeDeclensionInput,
      InputStream diminutiveDeclensionInput,
      InputStream genderDeclensionInput,
      InputStream genderNameInput,
      InputStream numberDeclensionInput,
      InputStream superlativeDeclensionInput,
      InputStream irregularVerbConjugationInput,
      InputStream regularVerbLexemeInput,
      InputStream regularVerbDeclensionInput,
      InputStream dictionaryInput,
      InputStream customDictionaryInput,
      InputStream wordRankingInput,
      String dictionaryExclusions, HashMap<String, String> lexiconConversions,
      int flags, int cacheSize, boolean breakOnHyphen,
      boolean breakOnUnderscore)
          throws ParserConfigurationException, SAXException, IOException,
          DictionaryLoadException, NumberFormatException,
          WordRankingLoadException {
    Replacement[] augmentativeDeclensions = Replacement.readReplacements
        (augmentativeDeclensionInput);
    Replacement[] superlativeDeclensions = Replacement.readReplacements(
        superlativeDeclensionInput);
    Replacement[] diminutiveDeclensions = Replacement.readReplacements(
        diminutiveDeclensionInput);
    Replacement[] genderDeclensions = Replacement.readReplacements(
        genderDeclensionInput);
    Replacement[] genderNames = Replacement.readReplacements(
        genderNameInput);
    Replacement[] adverbDeclensions = Replacement.readReplacements(
        adverbDeclensionInput);
    Replacement[] numberDeclensions = Replacement.readReplacements(
        numberDeclensionInput);
    Replacement[] irregularVerbConjugations = Replacement.readReplacements(
        irregularVerbConjugationInput);
    Replacement[] regularVerbLexemes = Replacement.readReplacements(
        regularVerbLexemeInput);
    Replacement[] regularVerbDeclensions = Replacement.readReplacements(
        regularVerbDeclensionInput);

    this.augmentativeNormalizer = new AugmentativeNormalizer(
        augmentativeDeclensions);
    this.superlativeNormalizer = new SuperlativeNormalizer(
        superlativeDeclensions);
    this.diminutiveNormalizer = new DiminutiveNormalizer(diminutiveDeclensions);
    this.genderNameNormalizer = new GenderNameNormalizer(genderNames);
    this.genderNormalizer = new GenderNormalizer(genderDeclensions);
    this.adverbNormalizer = new AdverbNormalizer(adverbDeclensions);
    this.numberNormalizer = new NumberNormalizer(numberDeclensions);
    this.verbNormalizer = new VerbNormalizer(irregularVerbConjugations,
        regularVerbLexemes, regularVerbDeclensions);

    this.augmentativeTag = this.combineReplacementTags(augmentativeDeclensions);
    this.superlativeTag = this.combineReplacementTags(superlativeDeclensions);
    this.diminutiveTag = this.combineReplacementTags(diminutiveDeclensions);
    this.genderTag = this.combineReplacementTags(genderDeclensions) + "|"
        + this.combineReplacementTags(genderNames);
    this.numberTag = this.combineReplacementTags(numberDeclensions);
    this.adverbTag = this.combineReplacementTags(adverbDeclensions);
    this.verbTag = this.combineReplacementTags(irregularVerbConjugations) + "|"
        + this.combineReplacementTags(regularVerbLexemes) + "|"
        + this.combineReplacementTags(regularVerbDeclensions);

    this.setFlags(flags);
    this.cache = new LemmatizerCache(cacheSize);
    this.breakOnHyphen = breakOnHyphen;
    this.breakOnUnderscore = breakOnUnderscore;
    // dictionary, lexicon & ranking
    this.dictionary = new Dictionary(dictionaryInput);
    this.dictionary.load(customDictionaryInput);
    this.lexicon = dictionary.retrieveLexicon();
    this.ranking = new WordRanking(wordRankingInput);
    this.dictionaryExclusions = dictionaryExclusions;
    this.lexiconConversions = lexiconConversions;
  }

  /**
   * This method retrieves the lemma of a given token, when classified with
   * a given <em>PoS tag</em>.
   *
   * @param  token the token whose lemma is wanted
   * @param  tag the <em>PoS tag</em> of the token
   * @return the lemma of the token (when classified with the given tag)
   */
  public String lemmatize(String token, String tag) {
    // normalize token/lemma
    String lemma = token.toLowerCase();
    // map UD tags to palavras tags
    tag = Replacement.fromPalavrasToUDTagset(tag, token);
    // check for token|tag in cache
    LemmatizerCacheKey key = new LemmatizerCacheKey(lemma, tag.toLowerCase());
    if (cache.containsKey(key)) {
      lemma = cache.get(key);
      return lemma;       
    }
    // check dictionary
    String lexPOSTag = tag.toUpperCase();
    if (lexPOSTag.contains("-")) {
      lexPOSTag = lexPOSTag.substring(0, lexPOSTag.indexOf("-"));
    }
    // address pos tag notation differences between open-nlp and label-lex-sw
    for (String conversionKey : lexiconConversions.keySet()) {
      if (lexPOSTag.equals(conversionKey)) {
        lexPOSTag = lexiconConversions.get(conversionKey);
      }
    }
    if (dictionary.contains(lemma, lexPOSTag)
        && !lexPOSTag.matches(dictionaryExclusions)) {
      String[] lemmas = dictionary.retrieveLemmas(lemma, lexPOSTag);
      return ranking.retrieveTopWord(lemmas);
    }
    // check lexicon
    if (lexicon.contains(lemma, lexPOSTag)) {
      cache.put(key, lemma);
      return lemma;          
    }
    // check for composed tokens
    if (breakOnHyphen && lemma.contains("-")) {
      return this.lemmatize(lemma.substring(0, lemma.indexOf("-")), tag)
          + "-" + this.lemmatize(lemma.substring(lemma.indexOf("-") + 1), tag);
    }
    if (breakOnUnderscore && lemma.contains("_")) {
      return this.lemmatize(lemma.substring(0, lemma.indexOf("_")), tag)
          + "_" + this.lemmatize(lemma.substring(lemma.indexOf("_") + 1), tag);
    }
    // use rules
    if (this.checkFlag(ADVERB)
        && tag.toLowerCase().matches(adverbTag)) {
      lemma = adverbNormalizer.normalize(lemma, tag);
      if (lexicon.contains(lemma, lexPOSTag)) {
        cache.put(key, lemma);
        return lemma;          
      }
    }
    if (this.checkFlag(NUMBER)
        && tag.toLowerCase().matches(numberTag)) {
      lemma = numberNormalizer.normalize(lemma, tag);
      if (lexicon.contains(lemma, lexPOSTag)) {
        cache.put(key, lemma);
        return lemma;          
      }
    }
    if (this.checkFlag(SUPERLATIVE)
        && tag.toLowerCase().matches(superlativeTag)) {
      lemma = superlativeNormalizer.normalize(lemma, tag);
      if (lexicon.contains(lemma, lexPOSTag)) {
        cache.put(key, lemma);
        return lemma;          
      }
    }
    if (this.checkFlag(AUGMENTATIVE) &&
        tag.toLowerCase().matches(augmentativeTag)) {
      lemma = augmentativeNormalizer.normalize(lemma, tag);
      if (lexicon.contains(lemma, lexPOSTag)) {
        cache.put(key, lemma);
        return lemma;          
      }
    }
    if (this.checkFlag(DIMINUTIVE)
        && tag.toLowerCase().matches(diminutiveTag)) {
      lemma = diminutiveNormalizer.normalize(lemma, tag);
      if (lexicon.contains(lemma, lexPOSTag)) {
        cache.put(key, lemma);
        return lemma;          
      }
    }
    else if (this.checkFlag(GENDER_DECLENSIONS)
        && tag.toLowerCase().matches(genderTag)) {
      lemma = genderNormalizer.normalize(lemma, tag);
      if (lexicon.contains(lemma, lexPOSTag)) {
        cache.put(key, lemma);
        return lemma;          
      }
    }
    else if (this.checkFlag(GENDER_NAMES)
        && tag.toLowerCase().matches(genderTag)) {
      lemma = genderNameNormalizer.normalize(lemma, tag);
      if (lexicon.contains(lemma, lexPOSTag)) {
        cache.put(key, lemma);
        return lemma;          
      }
    }
    if (this.checkFlag(VERB)
        && tag.toLowerCase().matches(verbTag)) {
      lemma = verbNormalizer.normalize(lemma, tag);
      if (lexicon.contains(lemma, lexPOSTag)) {
        cache.put(key, lemma);
        return lemma;          
      }
    }
    return lemma;
  }

  /**
   * This method ...
   *
   * @param  tokens ...
   * @param  tags ...
   * @return ...
   * @throws LemmatizeException ...
   */
  public String[] lemmatize(String tokens[], String tags[])
      throws LemmatizeException {
    if (tokens.length != tags.length) {
      throw new LemmatizeException("tokens.length: " + tokens.length
          + "; tags.length: " + tags.length);
    }

    String[] lemmas = new String[tokens.length];
    for (int i = 0; i < tokens.length; i++) {
      lemmas[i] = this.lemmatize(tokens[i], tags[i]);
    }
    return lemmas;
  }

  /**
   * This method ...
   * 
   * @return ...
   */
  public int getCacheSize() {
    return cacheSize;
  }

  /**
   * This method ...
   * 
   * @param  cacheSize ...
   */
  public void setCacheSize(int cacheSize) {
    this.cacheSize = cacheSize;
  }

  private String combineReplacementTags(Replacement[] replacements) {
    String combinedTags = new String();
    for (Replacement replacement : replacements) {
      if (combinedTags.length() > 0) {
        if (!("|" + combinedTags + "|").contains(
            "|" + replacement.getTag() + "|")) {
          combinedTags += "|" + replacement.getTag();
        }
      }
      else {
        combinedTags = replacement.getTag();
      }
    }
    return combinedTags;
  }

  /**
   * This method ...
   * 
   * @param  flags ...
   */
  public void setFlags(int flags) {
    // bitmasks
    this.flags = this.flags | flags;
  }

  /**
   * This method ...
   * 
   * @param  flag ...
   * @return ...
   */
  public boolean checkFlag(int flag) {
    // bitmasks
    return (flags & flag) == flag;
  }

  /**
   * This method ...
   */
  public void clearFlags() {
    this.flags = 0;
  }

  /**
   * This method ...
   * 
   * @return ...
   */
  public int getFlags() {
    return this.flags;
  }
}
