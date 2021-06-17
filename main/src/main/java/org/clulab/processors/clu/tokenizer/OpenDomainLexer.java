// Generated from main/src/main/java/org/clulab/processors/clu/tokenizer/OpenDomainLexer.g by ANTLR 4.9.2

  package org.clulab.processors.clu.tokenizer;

import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class OpenDomainLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.9.2", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		PARENS=1, QUOTES=2, DATE1=3, DATE2=4, NUMBER=5, FRACTION=6, WORD=7, TWITTER_NAME=8, 
		TWITTER_HASHTAG=9, FILENAME=10, PROGRAMMING_LANGUAGES=11, FULLURL=12, 
		LIKELYURL_WWW=13, LIKELYURL_COM=14, EMAIL=15, SGML=16, HTML_CODE=17, SMILEY=18, 
		LETTER_SMILEY=19, EOS=20, WHITESPACE=21, SEQ_OF_UNICODES=22, ErrorCharacter=23;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	private static String[] makeRuleNames() {
		return new String[] {
			"PARENS", "QUOTES", "DATE1", "DATE2", "NUMBER", "FRACTION", "WORD", "TWITTER_NAME", 
			"TWITTER_HASHTAG", "FILENAME", "PROGRAMMING_LANGUAGES", "FULLURL", "LIKELYURL_WWW", 
			"LIKELYURL_COM", "EMAIL", "SGML", "HTML_CODE", "SMILEY", "LETTER_SMILEY", 
			"EOS", "WHITESPACE", "SEQ_OF_UNICODES", "ErrorCharacter", "LOWER_CASE_LETTER", 
			"UPPER_CASE_LETTER", "SPLET", "LETTER", "DIGIT", "ALPHANUM", "NUM", "ONE_TO_TWO_DIGITS", 
			"TWO_TO_FOUR_DIGITS", "ONE_TO_FOUR_DIGITS", "FOUR_DIGITS", "PUNCTUATION", 
			"FILENAME_EXT", "URL_BLOCK1", "URL_BLOCK2", "URL_BLOCK3", "URL_END1", 
			"URL_END2", "URL_END3", "EMAIL_USER", "EMAIL_DOMAIN", "HI_SURROGATE", 
			"LO_SURROGATE", "EYEBROWS", "EYES", "NOSE"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, "PARENS", "QUOTES", "DATE1", "DATE2", "NUMBER", "FRACTION", "WORD", 
			"TWITTER_NAME", "TWITTER_HASHTAG", "FILENAME", "PROGRAMMING_LANGUAGES", 
			"FULLURL", "LIKELYURL_WWW", "LIKELYURL_COM", "EMAIL", "SGML", "HTML_CODE", 
			"SMILEY", "LETTER_SMILEY", "EOS", "WHITESPACE", "SEQ_OF_UNICODES", "ErrorCharacter"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}


	public OpenDomainLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "OpenDomainLexer.g"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	@Override
	public boolean sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
		switch (ruleIndex) {
		case 18:
			return LETTER_SMILEY_sempred((RuleContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean LETTER_SMILEY_sempred(RuleContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0:
			return ! Character.isLetterOrDigit(_input.LA(1));
		}
		return true;
	}

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\31\u029a\b\1\4\2"+
		"\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4"+
		"\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22"+
		"\t\22\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31"+
		"\t\31\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t"+
		" \4!\t!\4\"\t\"\4#\t#\4$\t$\4%\t%\4&\t&\4\'\t\'\4(\t(\4)\t)\4*\t*\4+\t"+
		"+\4,\t,\4-\t-\4.\t.\4/\t/\4\60\t\60\4\61\t\61\4\62\t\62\3\2\3\2\3\2\3"+
		"\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2"+
		"\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\5\2\u0084\n\2\3\3\3\3\3\3\3\3\3\3"+
		"\3\3\5\3\u008c\n\3\3\4\3\4\3\4\3\4\3\4\3\4\3\5\3\5\3\5\3\5\3\5\3\5\3\6"+
		"\5\6\u009b\n\6\3\6\3\6\3\7\3\7\3\7\3\7\3\b\6\b\u00a4\n\b\r\b\16\b\u00a5"+
		"\3\b\3\b\6\b\u00aa\n\b\r\b\16\b\u00ab\7\b\u00ae\n\b\f\b\16\b\u00b1\13"+
		"\b\3\t\3\t\3\t\3\t\5\t\u00b7\n\t\3\t\3\t\3\t\3\t\7\t\u00bd\n\t\f\t\16"+
		"\t\u00c0\13\t\3\n\3\n\6\n\u00c4\n\n\r\n\16\n\u00c5\3\13\6\13\u00c9\n\13"+
		"\r\13\16\13\u00ca\3\13\3\13\6\13\u00cf\n\13\r\13\16\13\u00d0\7\13\u00d3"+
		"\n\13\f\13\16\13\u00d6\13\13\3\13\3\13\3\13\3\f\3\f\3\f\3\f\3\f\3\f\3"+
		"\f\5\f\u00e2\n\f\3\r\3\r\3\r\3\r\3\r\3\r\5\r\u00ea\n\r\3\r\3\r\3\r\3\r"+
		"\3\r\6\r\u00f1\n\r\r\r\16\r\u00f2\3\r\3\r\3\16\3\16\3\16\3\16\3\16\3\16"+
		"\6\16\u00fd\n\16\r\16\16\16\u00fe\3\16\3\16\6\16\u0103\n\16\r\16\16\16"+
		"\u0104\3\16\3\16\5\16\u0109\n\16\3\16\3\16\5\16\u010d\n\16\3\16\3\16\5"+
		"\16\u0111\n\16\3\16\3\16\5\16\u0115\n\16\3\17\6\17\u0118\n\17\r\17\16"+
		"\17\u0119\3\17\3\17\6\17\u011e\n\17\r\17\16\17\u011f\3\17\3\17\3\17\3"+
		"\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3"+
		"\17\3\17\3\17\5\17\u0136\n\17\3\17\3\17\6\17\u013a\n\17\r\17\16\17\u013b"+
		"\3\17\3\17\5\17\u0140\n\17\3\20\3\20\3\20\3\20\3\20\5\20\u0147\n\20\3"+
		"\20\3\20\3\20\3\20\3\20\7\20\u014e\n\20\f\20\16\20\u0151\13\20\3\20\3"+
		"\20\3\20\3\20\3\20\3\20\5\20\u0159\n\20\3\21\3\21\5\21\u015d\n\21\3\21"+
		"\6\21\u0160\n\21\r\21\16\21\u0161\3\21\3\21\3\22\3\22\3\22\5\22\u0169"+
		"\n\22\3\22\3\22\5\22\u016d\n\22\3\22\3\22\5\22\u0171\n\22\3\22\3\22\5"+
		"\22\u0175\n\22\3\22\3\22\3\23\5\23\u017a\n\23\3\23\3\23\5\23\u017e\n\23"+
		"\3\23\3\23\3\23\3\23\3\23\3\23\3\23\3\23\3\23\3\23\5\23\u018a\n\23\3\24"+
		"\5\24\u018d\n\24\3\24\3\24\5\24\u0191\n\24\3\24\3\24\3\24\3\25\3\25\7"+
		"\25\u0198\n\25\f\25\16\25\u019b\13\25\3\26\6\26\u019e\n\26\r\26\16\26"+
		"\u019f\3\26\3\26\3\27\3\27\3\27\3\30\3\30\3\31\3\31\3\32\3\32\3\33\3\33"+
		"\3\33\3\33\3\33\3\33\3\33\3\33\3\33\3\33\3\33\3\33\3\33\3\33\3\33\5\33"+
		"\u01bc\n\33\3\34\3\34\3\34\3\34\3\34\3\34\3\34\5\34\u01c5\n\34\3\35\5"+
		"\35\u01c8\n\35\3\36\3\36\5\36\u01cc\n\36\3\37\6\37\u01cf\n\37\r\37\16"+
		"\37\u01d0\3\37\7\37\u01d4\n\37\f\37\16\37\u01d7\13\37\3\37\3\37\6\37\u01db"+
		"\n\37\r\37\16\37\u01dc\6\37\u01df\n\37\r\37\16\37\u01e0\5\37\u01e3\n\37"+
		"\3 \3 \5 \u01e7\n \3!\3!\3!\5!\u01ec\n!\3!\5!\u01ef\n!\3\"\3\"\5\"\u01f3"+
		"\n\"\3\"\5\"\u01f6\n\"\3\"\5\"\u01f9\n\"\3#\3#\3#\3#\3#\3$\3$\3%\3%\3"+
		"%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3"+
		"%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3"+
		"%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3"+
		"%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3"+
		"%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\3%\5%\u0272\n%\3&\3"+
		"&\3\'\3\'\3(\3(\3)\3)\3*\3*\3+\3+\3,\5,\u0281\n,\3,\7,\u0284\n,\f,\16"+
		",\u0287\13,\3-\6-\u028a\n-\r-\16-\u028b\3.\3.\3/\3/\3\60\3\60\3\61\3\61"+
		"\3\62\3\62\3\62\5\62\u0299\n\62\2\2\63\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21"+
		"\n\23\13\25\f\27\r\31\16\33\17\35\20\37\21!\22#\23%\24\'\25)\26+\27-\30"+
		"/\31\61\2\63\2\65\2\67\29\2;\2=\2?\2A\2C\2E\2G\2I\2K\2M\2O\2Q\2S\2U\2"+
		"W\2Y\2[\2]\2_\2a\2c\2\3\2!\4\2\u201a\u201b\u201e\u201f\5\2//\61\61<<\4"+
		"\2--//\4\2\61\61\u2046\u2046\5\2))/\60aa\4\2EEee\4\2HHhh\t\2##*+..\60"+
		"\60>>@A}\177\4\2*+^^\4\2BB}\177\7\2\61\61>>@@]]__\7\2%&((,,..\60\60\17"+
		"\2\62\62\65\65::DFLLNNQRUUZZdfnnqrzz\b\2\2\"\u0087\u0087\u00a2\u00a2\u2002"+
		"\u200c\u202a\u202b\u3002\u3002\f\2CCGGKKQQWWccggkkqqww.\2\u00af\u00af"+
		"\u0239\u0251\u02c4\u02c7\u02d4\u02e1\u02e7\u037f\u0386\u0387\u03d1\u03d1"+
		"\u03f8\u03f8\u03fe\u0401\u0485\u0489\u04d1\u04d1\u04f8\u0501\u0512\u0527"+
		"\u055c\u0561\u0593\u05bf\u05c1\u05c1\u05c3\u05c4\u05c6\u05c7\u05c9\u05c9"+
		"\u0617\u061c\u063d\u0641\u064d\u0660\u0672\u0672\u06d8\u06f1\u06fc\u0701"+
		"\u0711\u0711\u0713\u0713\u0732\u0781\u07a8\u07b3\u07cc\u07f7\u07fc\u07fc"+
		"\u0902\u0905\u093e\u093e\u0940\u0950\u0953\u0957\u0964\u0965\u0983\u0985"+
		"\u09be\u09c6\u09c9\u09ca\u09cd\u09cf\u09d9\u09d9\u09e4\u09e5\u0a03\u0a05"+
		"\u0a3e\u0a3e\17\2\u0abe\u0ad1\u0b84\u0b84\u0bc0\u0bc4\u0bc8\u0bca\u0bcc"+
		"\u0bcf\u0c03\u0c05\u0c40\u0c58\u0d40\u0d46\u0d48\u0d4a\u0e32\u0e3c\u0e49"+
		"\u0e50\u0eb3\u0ebe\u0eca\u0ecf\4\2\62;\u07c2\u07cb\7\2..\60\60<<\u00af"+
		"\u00af\u066d\u066e\7\2##..\60\60==AA\n\2\13\f\16\17\"\"$$*+>>@@}\177\13"+
		"\2\13\f\16\17\"$*+..\60\60>>@A}\177\f\2\13\f\16\17\"$&&)+.\60>>@Aab}\177"+
		"\n\2\13\f\16\17\"$*+.\60>>@A}\177\n\2\13\f\16\17\"\"$$*+>>@@~~\5\2\62"+
		";C\\c|\13\2\13\f\16\17\"\"$$*+>>@@}\177\u00a2\u00a2\r\2\13\f\16\17\"\""+
		"$$*+..\60\60>>@@}\177\u00a2\u00a2\b\2..\62\62>>@@QQ~\177\7\2::<=??ZZz"+
		"z\t\2)),,//\61\61``eeqq\2\u02fd\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2"+
		"\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2"+
		"\2\2\2\25\3\2\2\2\2\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2\2\2\2\35\3\2\2\2"+
		"\2\37\3\2\2\2\2!\3\2\2\2\2#\3\2\2\2\2%\3\2\2\2\2\'\3\2\2\2\2)\3\2\2\2"+
		"\2+\3\2\2\2\2-\3\2\2\2\2/\3\2\2\2\3\u0083\3\2\2\2\5\u008b\3\2\2\2\7\u008d"+
		"\3\2\2\2\t\u0093\3\2\2\2\13\u009a\3\2\2\2\r\u009e\3\2\2\2\17\u00a3\3\2"+
		"\2\2\21\u00b2\3\2\2\2\23\u00c1\3\2\2\2\25\u00c8\3\2\2\2\27\u00e1\3\2\2"+
		"\2\31\u00e3\3\2\2\2\33\u00f6\3\2\2\2\35\u011d\3\2\2\2\37\u0146\3\2\2\2"+
		"!\u015a\3\2\2\2#\u0165\3\2\2\2%\u0179\3\2\2\2\'\u018c\3\2\2\2)\u0195\3"+
		"\2\2\2+\u019d\3\2\2\2-\u01a3\3\2\2\2/\u01a6\3\2\2\2\61\u01a8\3\2\2\2\63"+
		"\u01aa\3\2\2\2\65\u01ac\3\2\2\2\67\u01c4\3\2\2\29\u01c7\3\2\2\2;\u01cb"+
		"\3\2\2\2=\u01e2\3\2\2\2?\u01e4\3\2\2\2A\u01e8\3\2\2\2C\u01f0\3\2\2\2E"+
		"\u01fa\3\2\2\2G\u01ff\3\2\2\2I\u0271\3\2\2\2K\u0273\3\2\2\2M\u0275\3\2"+
		"\2\2O\u0277\3\2\2\2Q\u0279\3\2\2\2S\u027b\3\2\2\2U\u027d\3\2\2\2W\u0280"+
		"\3\2\2\2Y\u0289\3\2\2\2[\u028d\3\2\2\2]\u028f\3\2\2\2_\u0291\3\2\2\2a"+
		"\u0293\3\2\2\2c\u0298\3\2\2\2ef\7/\2\2fg\7N\2\2gh\7T\2\2hi\7D\2\2i\u0084"+
		"\7/\2\2jk\7/\2\2kl\7T\2\2lm\7T\2\2mn\7D\2\2n\u0084\7/\2\2op\7/\2\2pq\7"+
		"N\2\2qr\7E\2\2rs\7D\2\2s\u0084\7/\2\2tu\7/\2\2uv\7T\2\2vw\7E\2\2wx\7D"+
		"\2\2x\u0084\7/\2\2yz\7/\2\2z{\7N\2\2{|\7U\2\2|}\7D\2\2}\u0084\7/\2\2~"+
		"\177\7/\2\2\177\u0080\7T\2\2\u0080\u0081\7U\2\2\u0081\u0082\7D\2\2\u0082"+
		"\u0084\7/\2\2\u0083e\3\2\2\2\u0083j\3\2\2\2\u0083o\3\2\2\2\u0083t\3\2"+
		"\2\2\u0083y\3\2\2\2\u0083~\3\2\2\2\u0084\4\3\2\2\2\u0085\u008c\7$\2\2"+
		"\u0086\u0087\7b\2\2\u0087\u008c\7b\2\2\u0088\u0089\7)\2\2\u0089\u008c"+
		"\7)\2\2\u008a\u008c\t\2\2\2\u008b\u0085\3\2\2\2\u008b\u0086\3\2\2\2\u008b"+
		"\u0088\3\2\2\2\u008b\u008a\3\2\2\2\u008c\6\3\2\2\2\u008d\u008e\5? \2\u008e"+
		"\u008f\t\3\2\2\u008f\u0090\5? \2\u0090\u0091\t\3\2\2\u0091\u0092\5A!\2"+
		"\u0092\b\3\2\2\2\u0093\u0094\5E#\2\u0094\u0095\t\3\2\2\u0095\u0096\5?"+
		" \2\u0096\u0097\t\3\2\2\u0097\u0098\5? \2\u0098\n\3\2\2\2\u0099\u009b"+
		"\t\4\2\2\u009a\u0099\3\2\2\2\u009a\u009b\3\2\2\2\u009b\u009c\3\2\2\2\u009c"+
		"\u009d\5=\37\2\u009d\f\3\2\2\2\u009e\u009f\5C\"\2\u009f\u00a0\t\5\2\2"+
		"\u00a0\u00a1\5C\"\2\u00a1\16\3\2\2\2\u00a2\u00a4\5;\36\2\u00a3\u00a2\3"+
		"\2\2\2\u00a4\u00a5\3\2\2\2\u00a5\u00a3\3\2\2\2\u00a5\u00a6\3\2\2\2\u00a6"+
		"\u00af\3\2\2\2\u00a7\u00a9\t\6\2\2\u00a8\u00aa\5;\36\2\u00a9\u00a8\3\2"+
		"\2\2\u00aa\u00ab\3\2\2\2\u00ab\u00a9\3\2\2\2\u00ab\u00ac\3\2\2\2\u00ac"+
		"\u00ae\3\2\2\2\u00ad\u00a7\3\2\2\2\u00ae\u00b1\3\2\2\2\u00af\u00ad\3\2"+
		"\2\2\u00af\u00b0\3\2\2\2\u00b0\20\3\2\2\2\u00b1\u00af\3\2\2\2\u00b2\u00b6"+
		"\7B\2\2\u00b3\u00b7\5\61\31\2\u00b4\u00b7\5\63\32\2\u00b5\u00b7\7a\2\2"+
		"\u00b6\u00b3\3\2\2\2\u00b6\u00b4\3\2\2\2\u00b6\u00b5\3\2\2\2\u00b7\u00be"+
		"\3\2\2\2\u00b8\u00bd\5\61\31\2\u00b9\u00bd\5\63\32\2\u00ba\u00bd\7a\2"+
		"\2\u00bb\u00bd\59\35\2\u00bc\u00b8\3\2\2\2\u00bc\u00b9\3\2\2\2\u00bc\u00ba"+
		"\3\2\2\2\u00bc\u00bb\3\2\2\2\u00bd\u00c0\3\2\2\2\u00be\u00bc\3\2\2\2\u00be"+
		"\u00bf\3\2\2\2\u00bf\22\3\2\2\2\u00c0\u00be\3\2\2\2\u00c1\u00c3\7%\2\2"+
		"\u00c2\u00c4\5\67\34\2\u00c3\u00c2\3\2\2\2\u00c4\u00c5\3\2\2\2\u00c5\u00c3"+
		"\3\2\2\2\u00c5\u00c6\3\2\2\2\u00c6\24\3\2\2\2\u00c7\u00c9\5;\36\2\u00c8"+
		"\u00c7\3\2\2\2\u00c9\u00ca\3\2\2\2\u00ca\u00c8\3\2\2\2\u00ca\u00cb\3\2"+
		"\2\2\u00cb\u00d4\3\2\2\2\u00cc\u00ce\7\60\2\2\u00cd\u00cf\5;\36\2\u00ce"+
		"\u00cd\3\2\2\2\u00cf\u00d0\3\2\2\2\u00d0\u00ce\3\2\2\2\u00d0\u00d1\3\2"+
		"\2\2\u00d1\u00d3\3\2\2\2\u00d2\u00cc\3\2\2\2\u00d3\u00d6\3\2\2\2\u00d4"+
		"\u00d2\3\2\2\2\u00d4\u00d5\3\2\2\2\u00d5\u00d7\3\2\2\2\u00d6\u00d4\3\2"+
		"\2\2\u00d7\u00d8\7\60\2\2\u00d8\u00d9\5I%\2\u00d9\26\3\2\2\2\u00da\u00db"+
		"\t\7\2\2\u00db\u00dc\7-\2\2\u00dc\u00e2\7-\2\2\u00dd\u00de\t\7\2\2\u00de"+
		"\u00e2\7%\2\2\u00df\u00e0\t\b\2\2\u00e0\u00e2\7%\2\2\u00e1\u00da\3\2\2"+
		"\2\u00e1\u00dd\3\2\2\2\u00e1\u00df\3\2\2\2\u00e2\30\3\2\2\2\u00e3\u00e4"+
		"\7j\2\2\u00e4\u00e5\7v\2\2\u00e5\u00e6\7v\2\2\u00e6\u00e7\7r\2\2\u00e7"+
		"\u00e9\3\2\2\2\u00e8\u00ea\7u\2\2\u00e9\u00e8\3\2\2\2\u00e9\u00ea\3\2"+
		"\2\2\u00ea\u00eb\3\2\2\2\u00eb\u00ec\7<\2\2\u00ec\u00ed\7\61\2\2\u00ed"+
		"\u00ee\7\61\2\2\u00ee\u00f0\3\2\2\2\u00ef\u00f1\5K&\2\u00f0\u00ef\3\2"+
		"\2\2\u00f1\u00f2\3\2\2\2\u00f2\u00f0\3\2\2\2\u00f2\u00f3\3\2\2\2\u00f3"+
		"\u00f4\3\2\2\2\u00f4\u00f5\5Q)\2\u00f5\32\3\2\2\2\u00f6\u00f7\7y\2\2\u00f7"+
		"\u00f8\7y\2\2\u00f8\u00f9\7y\2\2\u00f9\u00fa\7\60\2\2\u00fa\u0102\3\2"+
		"\2\2\u00fb\u00fd\5M\'\2\u00fc\u00fb\3\2\2\2\u00fd\u00fe\3\2\2\2\u00fe"+
		"\u00fc\3\2\2\2\u00fe\u00ff\3\2\2\2\u00ff\u0100\3\2\2\2\u0100\u0101\7\60"+
		"\2\2\u0101\u0103\3\2\2\2\u0102\u00fc\3\2\2\2\u0103\u0104\3\2\2\2\u0104"+
		"\u0102\3\2\2\2\u0104\u0105\3\2\2\2\u0105\u0108\3\2\2\2\u0106\u0109\5\61"+
		"\31\2\u0107\u0109\5\63\32\2\u0108\u0106\3\2\2\2\u0108\u0107\3\2\2\2\u0109"+
		"\u010c\3\2\2\2\u010a\u010d\5\61\31\2\u010b\u010d\5\63\32\2\u010c\u010a"+
		"\3\2\2\2\u010c\u010b\3\2\2\2\u010d\u0110\3\2\2\2\u010e\u0111\5\61\31\2"+
		"\u010f\u0111\5\63\32\2\u0110\u010e\3\2\2\2\u0110\u010f\3\2\2\2\u0110\u0111"+
		"\3\2\2\2\u0111\u0114\3\2\2\2\u0112\u0115\5\61\31\2\u0113\u0115\5\63\32"+
		"\2\u0114\u0112\3\2\2\2\u0114\u0113\3\2\2\2\u0114\u0115\3\2\2\2\u0115\34"+
		"\3\2\2\2\u0116\u0118\5O(\2\u0117\u0116\3\2\2\2\u0118\u0119\3\2\2\2\u0119"+
		"\u0117\3\2\2\2\u0119\u011a\3\2\2\2\u011a\u011b\3\2\2\2\u011b\u011c\7\60"+
		"\2\2\u011c\u011e\3\2\2\2\u011d\u0117\3\2\2\2\u011e\u011f\3\2\2\2\u011f"+
		"\u011d\3\2\2\2\u011f\u0120\3\2\2\2\u0120\u0135\3\2\2\2\u0121\u0122\7e"+
		"\2\2\u0122\u0123\7q\2\2\u0123\u0136\7o\2\2\u0124\u0125\7p\2\2\u0125\u0126"+
		"\7g\2\2\u0126\u0136\7v\2\2\u0127\u0128\7q\2\2\u0128\u0129\7t\2\2\u0129"+
		"\u0136\7i\2\2\u012a\u012b\7g\2\2\u012b\u012c\7f\2\2\u012c\u0136\7w\2\2"+
		"\u012d\u012e\7p\2\2\u012e\u012f\7c\2\2\u012f\u0130\7o\2\2\u0130\u0136"+
		"\7g\2\2\u0131\u0132\7k\2\2\u0132\u0133\7p\2\2\u0133\u0134\7h\2\2\u0134"+
		"\u0136\7q\2\2\u0135\u0121\3\2\2\2\u0135\u0124\3\2\2\2\u0135\u0127\3\2"+
		"\2\2\u0135\u012a\3\2\2\2\u0135\u012d\3\2\2\2\u0135\u0131\3\2\2\2\u0136"+
		"\u013f\3\2\2\2\u0137\u0139\7\61\2\2\u0138\u013a\5S*\2\u0139\u0138\3\2"+
		"\2\2\u013a\u013b\3\2\2\2\u013b\u0139\3\2\2\2\u013b\u013c\3\2\2\2\u013c"+
		"\u013d\3\2\2\2\u013d\u013e\5U+\2\u013e\u0140\3\2\2\2\u013f\u0137\3\2\2"+
		"\2\u013f\u0140\3\2\2\2\u0140\36\3\2\2\2\u0141\u0142\7(\2\2\u0142\u0143"+
		"\7n\2\2\u0143\u0144\7v\2\2\u0144\u0147\7=\2\2\u0145\u0147\7>\2\2\u0146"+
		"\u0141\3\2\2\2\u0146\u0145\3\2\2\2\u0146\u0147\3\2\2\2\u0147\u0148\3\2"+
		"\2\2\u0148\u0149\5W,\2\u0149\u014f\7B\2\2\u014a\u014b\5Y-\2\u014b\u014c"+
		"\7\60\2\2\u014c\u014e\3\2\2\2\u014d\u014a\3\2\2\2\u014e\u0151\3\2\2\2"+
		"\u014f\u014d\3\2\2\2\u014f\u0150\3\2\2\2\u0150\u0152\3\2\2\2\u0151\u014f"+
		"\3\2\2\2\u0152\u0158\5Y-\2\u0153\u0154\7(\2\2\u0154\u0155\7i\2\2\u0155"+
		"\u0156\7v\2\2\u0156\u0159\7=\2\2\u0157\u0159\7@\2\2\u0158\u0153\3\2\2"+
		"\2\u0158\u0157\3\2\2\2\u0158\u0159\3\2\2\2\u0159 \3\2\2\2\u015a\u015c"+
		"\7>\2\2\u015b\u015d\7\61\2\2\u015c\u015b\3\2\2\2\u015c\u015d\3\2\2\2\u015d"+
		"\u015f\3\2\2\2\u015e\u0160\n\t\2\2\u015f\u015e\3\2\2\2\u0160\u0161\3\2"+
		"\2\2\u0161\u015f\3\2\2\2\u0161\u0162\3\2\2\2\u0162\u0163\3\2\2\2\u0163"+
		"\u0164\7@\2\2\u0164\"\3\2\2\2\u0165\u0168\7(\2\2\u0166\u0169\5\61\31\2"+
		"\u0167\u0169\5\63\32\2\u0168\u0166\3\2\2\2\u0168\u0167\3\2\2\2\u0169\u016c"+
		"\3\2\2\2\u016a\u016d\5\61\31\2\u016b\u016d\5\63\32\2\u016c\u016a\3\2\2"+
		"\2\u016c\u016b\3\2\2\2\u016d\u0170\3\2\2\2\u016e\u0171\5\61\31\2\u016f"+
		"\u0171\5\63\32\2\u0170\u016e\3\2\2\2\u0170\u016f\3\2\2\2\u0170\u0171\3"+
		"\2\2\2\u0171\u0174\3\2\2\2\u0172\u0175\5\61\31\2\u0173\u0175\5\63\32\2"+
		"\u0174\u0172\3\2\2\2\u0174\u0173\3\2\2\2\u0174\u0175\3\2\2\2\u0175\u0176"+
		"\3\2\2\2\u0176\u0177\7=\2\2\u0177$\3\2\2\2\u0178\u017a\5_\60\2\u0179\u0178"+
		"\3\2\2\2\u0179\u017a\3\2\2\2\u017a\u017b\3\2\2\2\u017b\u017d\5a\61\2\u017c"+
		"\u017e\5c\62\2\u017d\u017c\3\2\2\2\u017d\u017e\3\2\2\2\u017e\u0189\3\2"+
		"\2\2\u017f\u018a\t\n\2\2\u0180\u0181\7^\2\2\u0181\u018a\7^\2\2\u0182\u018a"+
		"\t\13\2\2\u0183\u0184\7~\2\2\u0184\u018a\7~\2\2\u0185\u018a\t\f\2\2\u0186"+
		"\u0187\7+\2\2\u0187\u018a\7+\2\2\u0188\u018a\t\r\2\2\u0189\u017f\3\2\2"+
		"\2\u0189\u0180\3\2\2\2\u0189\u0182\3\2\2\2\u0189\u0183\3\2\2\2\u0189\u0185"+
		"\3\2\2\2\u0189\u0186\3\2\2\2\u0189\u0188\3\2\2\2\u018a&\3\2\2\2\u018b"+
		"\u018d\5_\60\2\u018c\u018b\3\2\2\2\u018c\u018d\3\2\2\2\u018d\u018e\3\2"+
		"\2\2\u018e\u0190\5a\61\2\u018f\u0191\5c\62\2\u0190\u018f\3\2\2\2\u0190"+
		"\u0191\3\2\2\2\u0191\u0192\3\2\2\2\u0192\u0193\t\16\2\2\u0193\u0194\6"+
		"\24\2\2\u0194(\3\2\2\2\u0195\u0199\5G$\2\u0196\u0198\5G$\2\u0197\u0196"+
		"\3\2\2\2\u0198\u019b\3\2\2\2\u0199\u0197\3\2\2\2\u0199\u019a\3\2\2\2\u019a"+
		"*\3\2\2\2\u019b\u0199\3\2\2\2\u019c\u019e\t\17\2\2\u019d\u019c\3\2\2\2"+
		"\u019e\u019f\3\2\2\2\u019f\u019d\3\2\2\2\u019f\u01a0\3\2\2\2\u01a0\u01a1"+
		"\3\2\2\2\u01a1\u01a2\b\26\2\2\u01a2,\3\2\2\2\u01a3\u01a4\5[.\2\u01a4\u01a5"+
		"\5]/\2\u01a5.\3\2\2\2\u01a6\u01a7\13\2\2\2\u01a7\60\3\2\2\2\u01a8\u01a9"+
		"\4c|\2\u01a9\62\3\2\2\2\u01aa\u01ab\4C\\\2\u01ab\64\3\2\2\2\u01ac\u01ad"+
		"\7(\2\2\u01ad\u01bb\t\20\2\2\u01ae\u01af\7c\2\2\u01af\u01b0\7e\2\2\u01b0"+
		"\u01b1\7w\2\2\u01b1\u01b2\7v\2\2\u01b2\u01bc\7g\2\2\u01b3\u01b4\7i\2\2"+
		"\u01b4\u01b5\7t\2\2\u01b5\u01b6\7c\2\2\u01b6\u01b7\7x\2\2\u01b7\u01bc"+
		"\7g\2\2\u01b8\u01b9\7w\2\2\u01b9\u01ba\7o\2\2\u01ba\u01bc\7n\2\2\u01bb"+
		"\u01ae\3\2\2\2\u01bb\u01b3\3\2\2\2\u01bb\u01b8\3\2\2\2\u01bc\66\3\2\2"+
		"\2\u01bd\u01c5\5\61\31\2\u01be\u01c5\5\63\32\2\u01bf\u01c5\5\65\33\2\u01c0"+
		"\u01c5\t\21\2\2\u01c1\u01c2\4\u0a40\u0a51\2\u01c2\u01c5\4\u0a83\u0a85"+
		"\2\u01c3\u01c5\t\22\2\2\u01c4\u01bd\3\2\2\2\u01c4\u01be\3\2\2\2\u01c4"+
		"\u01bf\3\2\2\2\u01c4\u01c0\3\2\2\2\u01c4\u01c1\3\2\2\2\u01c4\u01c3\3\2"+
		"\2\2\u01c58\3\2\2\2\u01c6\u01c8\t\23\2\2\u01c7\u01c6\3\2\2\2\u01c8:\3"+
		"\2\2\2\u01c9\u01cc\5\67\34\2\u01ca\u01cc\59\35\2\u01cb\u01c9\3\2\2\2\u01cb"+
		"\u01ca\3\2\2\2\u01cc<\3\2\2\2\u01cd\u01cf\59\35\2\u01ce\u01cd\3\2\2\2"+
		"\u01cf\u01d0\3\2\2\2\u01d0\u01ce\3\2\2\2\u01d0\u01d1\3\2\2\2\u01d1\u01e3"+
		"\3\2\2\2\u01d2\u01d4\59\35\2\u01d3\u01d2\3\2\2\2\u01d4\u01d7\3\2\2\2\u01d5"+
		"\u01d3\3\2\2\2\u01d5\u01d6\3\2\2\2\u01d6\u01de\3\2\2\2\u01d7\u01d5\3\2"+
		"\2\2\u01d8\u01da\t\24\2\2\u01d9\u01db\59\35\2\u01da\u01d9\3\2\2\2\u01db"+
		"\u01dc\3\2\2\2\u01dc\u01da\3\2\2\2\u01dc\u01dd\3\2\2\2\u01dd\u01df\3\2"+
		"\2\2\u01de\u01d8\3\2\2\2\u01df\u01e0\3\2\2\2\u01e0\u01de\3\2\2\2\u01e0"+
		"\u01e1\3\2\2\2\u01e1\u01e3\3\2\2\2\u01e2\u01ce\3\2\2\2\u01e2\u01d5\3\2"+
		"\2\2\u01e3>\3\2\2\2\u01e4\u01e6\59\35\2\u01e5\u01e7\59\35\2\u01e6\u01e5"+
		"\3\2\2\2\u01e6\u01e7\3\2\2\2\u01e7@\3\2\2\2\u01e8\u01e9\59\35\2\u01e9"+
		"\u01eb\59\35\2\u01ea\u01ec\59\35\2\u01eb\u01ea\3\2\2\2\u01eb\u01ec\3\2"+
		"\2\2\u01ec\u01ee\3\2\2\2\u01ed\u01ef\59\35\2\u01ee\u01ed\3\2\2\2\u01ee"+
		"\u01ef\3\2\2\2\u01efB\3\2\2\2\u01f0\u01f2\59\35\2\u01f1\u01f3\59\35\2"+
		"\u01f2\u01f1\3\2\2\2\u01f2\u01f3\3\2\2\2\u01f3\u01f5\3\2\2\2\u01f4\u01f6"+
		"\59\35\2\u01f5\u01f4\3\2\2\2\u01f5\u01f6\3\2\2\2\u01f6\u01f8\3\2\2\2\u01f7"+
		"\u01f9\59\35\2\u01f8\u01f7\3\2\2\2\u01f8\u01f9\3\2\2\2\u01f9D\3\2\2\2"+
		"\u01fa\u01fb\59\35\2\u01fb\u01fc\59\35\2\u01fc\u01fd\59\35\2\u01fd\u01fe"+
		"\59\35\2\u01feF\3\2\2\2\u01ff\u0200\t\25\2\2\u0200H\3\2\2\2\u0201\u0202"+
		"\7d\2\2\u0202\u0203\7c\2\2\u0203\u0272\7v\2\2\u0204\u0205\7d\2\2\u0205"+
		"\u0206\7o\2\2\u0206\u0272\7r\2\2\u0207\u0272\7e\2\2\u0208\u0209\7e\2\2"+
		"\u0209\u020a\7n\2\2\u020a\u020b\7c\2\2\u020b\u020c\7u\2\2\u020c\u0272"+
		"\7u\2\2\u020d\u020e\7e\2\2\u020e\u020f\7i\2\2\u020f\u0272\7k\2\2\u0210"+
		"\u0211\7e\2\2\u0211\u0212\7r\2\2\u0212\u0272\7r\2\2\u0213\u0214\7f\2\2"+
		"\u0214\u0215\7n\2\2\u0215\u0272\7n\2\2\u0216\u0217\7f\2\2\u0217\u0218"+
		"\7q\2\2\u0218\u0272\7e\2\2\u0219\u021a\7f\2\2\u021a\u021b\7q\2\2\u021b"+
		"\u021c\7e\2\2\u021c\u0272\7z\2\2\u021d\u021e\7g\2\2\u021e\u021f\7z\2\2"+
		"\u021f\u0272\7g\2\2\u0220\u0221\7i\2\2\u0221\u0222\7k\2\2\u0222\u0272"+
		"\7h\2\2\u0223\u0224\7i\2\2\u0224\u0272\7|\2\2\u0225\u0272\7j\2\2\u0226"+
		"\u0227\7j\2\2\u0227\u0228\7v\2\2\u0228\u0272\7o\2\2\u0229\u022a\7j\2\2"+
		"\u022a\u022b\7v\2\2\u022b\u022c\7o\2\2\u022c\u0272\7n\2\2\u022d\u022e"+
		"\7l\2\2\u022e\u022f\7c\2\2\u022f\u0272\7t\2\2\u0230\u0231\7l\2\2\u0231"+
		"\u0232\7c\2\2\u0232\u0233\7x\2\2\u0233\u0272\7c\2\2\u0234\u0235\7l\2\2"+
		"\u0235\u0236\7r\2\2\u0236\u0237\7g\2\2\u0237\u0272\7i\2\2\u0238\u0239"+
		"\7l\2\2\u0239\u023a\7r\2\2\u023a\u0272\7i\2\2\u023b\u023c\7o\2\2\u023c"+
		"\u023d\7q\2\2\u023d\u0272\7x\2\2\u023e\u023f\7o\2\2\u023f\u0240\7r\2\2"+
		"\u0240\u0272\7\65\2\2\u0241\u0242\7r\2\2\u0242\u0243\7f\2\2\u0243\u0272"+
		"\7h\2\2\u0244\u0245\7r\2\2\u0245\u0246\7j\2\2\u0246\u0272\7r\2\2\u0247"+
		"\u0248\7r\2\2\u0248\u0272\7n\2\2\u0249\u024a\7r\2\2\u024a\u024b\7p\2\2"+
		"\u024b\u0272\7i\2\2\u024c\u024d\7r\2\2\u024d\u024e\7r\2\2\u024e\u0272"+
		"\7v\2\2\u024f\u0250\7r\2\2\u0250\u0251\7r\2\2\u0251\u0252\7v\2\2\u0252"+
		"\u0272\7z\2\2\u0253\u0254\7r\2\2\u0254\u0272\7u\2\2\u0255\u0256\7r\2\2"+
		"\u0256\u0272\7{\2\2\u0257\u0258\7u\2\2\u0258\u0259\7e\2\2\u0259\u025a"+
		"\7c\2\2\u025a\u025b\7n\2\2\u025b\u0272\7c\2\2\u025c\u025d\7u\2\2\u025d"+
		"\u025e\7s\2\2\u025e\u0272\7n\2\2\u025f\u0260\7v\2\2\u0260\u0261\7c\2\2"+
		"\u0261\u0272\7t\2\2\u0262\u0263\7v\2\2\u0263\u0264\7i\2\2\u0264\u0272"+
		"\7|\2\2\u0265\u0266\7v\2\2\u0266\u0267\7z\2\2\u0267\u0272\7v\2\2\u0268"+
		"\u0269\7y\2\2\u0269\u026a\7c\2\2\u026a\u0272\7x\2\2\u026b\u026c\7z\2\2"+
		"\u026c\u026d\7o\2\2\u026d\u0272\7n\2\2\u026e\u026f\7|\2\2\u026f\u0270"+
		"\7k\2\2\u0270\u0272\7r\2\2\u0271\u0201\3\2\2\2\u0271\u0204\3\2\2\2\u0271"+
		"\u0207\3\2\2\2\u0271\u0208\3\2\2\2\u0271\u020d\3\2\2\2\u0271\u0210\3\2"+
		"\2\2\u0271\u0213\3\2\2\2\u0271\u0216\3\2\2\2\u0271\u0219\3\2\2\2\u0271"+
		"\u021d\3\2\2\2\u0271\u0220\3\2\2\2\u0271\u0223\3\2\2\2\u0271\u0225\3\2"+
		"\2\2\u0271\u0226\3\2\2\2\u0271\u0229\3\2\2\2\u0271\u022d\3\2\2\2\u0271"+
		"\u0230\3\2\2\2\u0271\u0234\3\2\2\2\u0271\u0238\3\2\2\2\u0271\u023b\3\2"+
		"\2\2\u0271\u023e\3\2\2\2\u0271\u0241\3\2\2\2\u0271\u0244\3\2\2\2\u0271"+
		"\u0247\3\2\2\2\u0271\u0249\3\2\2\2\u0271\u024c\3\2\2\2\u0271\u024f\3\2"+
		"\2\2\u0271\u0253\3\2\2\2\u0271\u0255\3\2\2\2\u0271\u0257\3\2\2\2\u0271"+
		"\u025c\3\2\2\2\u0271\u025f\3\2\2\2\u0271\u0262\3\2\2\2\u0271\u0265\3\2"+
		"\2\2\u0271\u0268\3\2\2\2\u0271\u026b\3\2\2\2\u0271\u026e\3\2\2\2\u0272"+
		"J\3\2\2\2\u0273\u0274\n\26\2\2\u0274L\3\2\2\2\u0275\u0276\n\27\2\2\u0276"+
		"N\3\2\2\2\u0277\u0278\n\30\2\2\u0278P\3\2\2\2\u0279\u027a\n\31\2\2\u027a"+
		"R\3\2\2\2\u027b\u027c\n\32\2\2\u027cT\3\2\2\2\u027d\u027e\n\31\2\2\u027e"+
		"V\3\2\2\2\u027f\u0281\t\33\2\2\u0280\u027f\3\2\2\2\u0281\u0285\3\2\2\2"+
		"\u0282\u0284\n\34\2\2\u0283\u0282\3\2\2\2\u0284\u0287\3\2\2\2\u0285\u0283"+
		"\3\2\2\2\u0285\u0286\3\2\2\2\u0286X\3\2\2\2\u0287\u0285\3\2\2\2\u0288"+
		"\u028a\n\35\2\2\u0289\u0288\3\2\2\2\u028a\u028b\3\2\2\2\u028b\u0289\3"+
		"\2\2\2\u028b\u028c\3\2\2\2\u028cZ\3\2\2\2\u028d\u028e\4\ud802\udc01\2"+
		"\u028e\\\3\2\2\2\u028f\u0290\4\udc02\ue001\2\u0290^\3\2\2\2\u0291\u0292"+
		"\t\36\2\2\u0292`\3\2\2\2\u0293\u0294\t\37\2\2\u0294b\3\2\2\2\u0295\u0299"+
		"\t \2\2\u0296\u0297\7\61\2\2\u0297\u0299\7\61\2\2\u0298\u0295\3\2\2\2"+
		"\u0298\u0296\3\2\2\2\u0299d\3\2\2\2C\2\u0083\u008b\u009a\u00a5\u00ab\u00af"+
		"\u00b6\u00bc\u00be\u00c5\u00ca\u00d0\u00d4\u00e1\u00e9\u00f2\u00fe\u0104"+
		"\u0108\u010c\u0110\u0114\u0119\u011f\u0135\u013b\u013f\u0146\u014f\u0158"+
		"\u015c\u0161\u0168\u016c\u0170\u0174\u0179\u017d\u0189\u018c\u0190\u0199"+
		"\u019d\u019f\u01bb\u01c4\u01c7\u01cb\u01d0\u01d5\u01dc\u01e0\u01e2\u01e6"+
		"\u01eb\u01ee\u01f2\u01f5\u01f8\u0271\u0280\u0285\u028b\u0298\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}