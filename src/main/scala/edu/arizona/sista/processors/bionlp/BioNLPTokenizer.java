package edu.arizona.sista.processors.bionlp;

import edu.stanford.nlp.ling.CoreAnnotations;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.process.CoreLabelTokenFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * User: mihais
 * Date: 10/27/14
 */
public class BioNLPTokenizer {

  /** Creates CoreNLP tokens */
  private static final CoreLabelTokenFactory tokenFactory = new CoreLabelTokenFactory();
  /** If true, remove tokens that are standalone dashes. They are likely to hurt the parser. */
  private static final boolean DISCARD_STANDALONE_DASHES = true;

  /**
   * this is a list of all suffixes which we'll split off if they follow a dash
   * e.g. "X-induced" will be split into the three tokens "X", "-", and "induced"
   */
  private static final HashSet<String> VALID_DASH_SUFFIXES = new HashSet<String>(
    Arrays.asList(new String[]{"\\w+ed", "\\w+ing", "(in)?dependent",
      "deficient", "response", "protein", "by", "specific", "like",
      "inducible", "responsive", "gene", "mRNA", "transcription", "cytoplasmic",
      "sensitive", "bound", "driven", "positive", "negative", "dominant",
      "family", "resistant", "activity", "proximal", "defective"}));
  private static final Pattern dashSuffixes = mkDashSuffixes();

  // matches a word followed by a slash followed by a word
  private static final Pattern ANYSLASH_PATTERN = Pattern.compile("(\\w+)(/)(\\w+)");

  private static Pattern mkDashSuffixes() {
    String allSuffixes = makeRegexOr(VALID_DASH_SUFFIXES);
    String allSuffixesRegex = "(\\w+)(-)(" + allSuffixes + ")";
    return Pattern.compile(allSuffixesRegex, Pattern.CASE_INSENSITIVE);
  }

  private static String makeRegexOr(Iterable<String> pieces) {
    StringBuilder suffixBuilder = new StringBuilder();
    for (String suffix : pieces) {
      if (suffixBuilder.length() > 0) {
        suffixBuilder.append("|");
      }
      suffixBuilder.append("(" + suffix + ")");
    }
    return suffixBuilder.toString();
  }

  /**
   * Main postprocessing functionality
   */
  public static List<CoreLabel> postprocessSentence(List<CoreLabel> tokens) {
    // we can't use the regex "(anti)|(non)" since that will create an extra
    // group and confuse breakOnPattern, thus we do an extra pass
    tokens = breakOnPattern(tokens, Pattern.compile("(anti)(-)(\\w+)",
      Pattern.CASE_INSENSITIVE));
    tokens = breakOnPattern(tokens, Pattern.compile("(non)(-)(\\w+)",
      Pattern.CASE_INSENSITIVE));
    tokens = breakOnPattern(tokens, dashSuffixes);
    tokens = breakOnPattern(tokens, ANYSLASH_PATTERN);

    // re-join trailing or preceding - or + to previous digit
    tokens = joinSigns(tokens);

    // convert parens to normalized forms, e.g., -LRB-. better for parsing
    // MS: this messes up BANNER; do not do this!
    // tokens = normalizeParens(tokens);

    return tokens;
  }

  private static List<CoreLabel> joinSigns(List<CoreLabel> tokens) {
    List<CoreLabel> output = new ArrayList<CoreLabel>();
    for (int i = 0; i < tokens.size(); i++) {
      // -/-
      if(i < tokens.size() - 3 &&
        tokens.get(i).endPosition() == tokens.get(i + 1).beginPosition() &&
        tokens.get(i + 1).word().equals("-") &&
        tokens.get(i + 2).word().equals("/") &&
        tokens.get(i + 3).word().equals("-")){
        String word = tokens.get(i).word() +
          tokens.get(i + 1).word() +
          tokens.get(i + 2).word() +
          tokens.get(i + 3).word();
        output.add(tokenFactory.makeToken(word, tokens.get(i).beginPosition(), word.length()));
        i += 3;
        continue;
      }

      // - or +
      if(i < tokens.size() - 1){
        CoreLabel crt = tokens.get(i);
        CoreLabel nxt = tokens.get(i + 1);

        // trailing +
        if(crt.endPosition() == nxt.beginPosition() &&
          ! isParen(crt.word()) &&
          nxt.word().equals("+")){
          String word = crt.word() + nxt.word();
          output.add(tokenFactory.makeToken(word, crt.beginPosition(), word.length()));
          i ++;
          continue;
        }

        // trailing -
        if(crt.endPosition() == nxt.beginPosition() &&
          (i + 2 >= tokens.size() || nxt.endPosition() != tokens.get(i + 2).beginPosition()) &&
          ! isParen(crt.word()) &&
          nxt.word().equals("-")){
          String word = crt.word() + nxt.word();
          output.add(tokenFactory.makeToken(word, crt.beginPosition(), word.length()));
          i ++;
          continue;
        }

        // preceding -
        if(crt.endPosition() == nxt.beginPosition() &&
          (i == 0 || crt.beginPosition() != tokens.get(i - 1).endPosition()) &&
          ! isParen(nxt.word()) &&
          crt.word().equals("-")){
          String word = crt.word() + nxt.word();
          output.add(tokenFactory.makeToken(word, crt.beginPosition(), word.length()));
          i ++;
          continue;
        }
      }

      output.add(tokens.get(i));
    }
    return output;
  }

  private static final String [][] PARENS = {
    { "(", "-LRB-" },
    { ")", "-RRB-" },
    { "[", "-LSB-" },
    { "]", "-RSB-" }
  };

  private static boolean isParen(String s) {
    for(int j = 0; j < PARENS.length; j ++){
      if(s.equals(PARENS[j][0])){
        return true;
      }
    }
    return false;
  }

  private static List<CoreLabel> normalizeParens(List<CoreLabel> tokens) {
    for (int i = 0; i < tokens.size(); i++) {
      CoreLabel token = tokens.get(i);
      for(int j = 0; j < PARENS.length; j ++){
        if(token.word().equals(PARENS[j][0])){
          token.set(CoreAnnotations.TextAnnotation.class, PARENS[j][1]);
        }
      }
    }
    return tokens;
  }

  private static List<CoreLabel> breakOnPattern(List<CoreLabel> tokens, Pattern pattern) {
    List<CoreLabel> output = new ArrayList<CoreLabel>();
    for (int i = 0; i < tokens.size(); i++) {
      CoreLabel token = tokens.get(i);
      Matcher matcher = pattern.matcher(token.word());
      if (matcher.find()) {
        int sepPos = matcher.start(2);
        String s1 = token.word().substring(0, sepPos);
        if(! DISCARD_STANDALONE_DASHES || ! s1.equals("-")){
          output.add(tokenFactory.makeToken(s1, token.beginPosition(), sepPos));
        }
        String sep = matcher.group(2);
        if(! DISCARD_STANDALONE_DASHES || ! sep.equals("-")){
          output.add(tokenFactory.makeToken(sep, token.beginPosition() + sepPos, 1));
        }
        String s3 = token.word().substring(sepPos + 1);
        if(! DISCARD_STANDALONE_DASHES || ! s3.equals("-")){
          output.add(tokenFactory.makeToken(s3, token.beginPosition() + sepPos + 1,
            token.endPosition() - token.beginPosition() - sepPos - 1));
        }

      } else {
        output.add(token);
      }
    }
    return output;
  }
}


