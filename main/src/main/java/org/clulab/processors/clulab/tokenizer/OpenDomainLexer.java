// Generated from main/src/main/java/org/clulab/processors/clulab/tokenizer/OpenDomainLexer.g by ANTLR 4.6

  package org.clulab.processors.clulab.tokenizer;

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
	static { RuntimeMetaData.checkVersion("4.6", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		PARENS=1, DATE=2, NUMBER=3, FRACTION=4, WORD=5, EOS=6, WHITESPACE=7, ErrorCharacter=8;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] ruleNames = {
		"PARENS", "DATE", "NUMBER", "FRACTION", "WORD", "EOS", "WHITESPACE", "ErrorCharacter", 
		"LOWER_CASE_LETTER", "UPPER_CASE_LETTER", "DIGIT", "NUM", "ONE_TO_TWO_DIGITS", 
		"TWO_TO_FOUR_DIGITS", "ONE_TO_FOUR_DIGITS", "ALPHANUM", "PUNCTUATION"
	};

	private static final String[] _LITERAL_NAMES = {
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, "PARENS", "DATE", "NUMBER", "FRACTION", "WORD", "EOS", "WHITESPACE", 
		"ErrorCharacter"
	};
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
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\2\n\u00a2\b\1\4\2\t"+
		"\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3"+
		"\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\5\2D\n\2\3\3\3\3\3"+
		"\3\3\3\3\3\3\3\3\4\5\4M\n\4\3\4\3\4\3\5\3\5\3\5\3\5\3\6\6\6V\n\6\r\6\16"+
		"\6W\3\7\6\7[\n\7\r\7\16\7\\\3\b\6\b`\n\b\r\b\16\ba\3\b\3\b\3\t\3\t\3\n"+
		"\3\n\3\13\3\13\3\f\5\fm\n\f\3\r\6\rp\n\r\r\r\16\rq\3\r\7\ru\n\r\f\r\16"+
		"\rx\13\r\3\r\3\r\6\r|\n\r\r\r\16\r}\6\r\u0080\n\r\r\r\16\r\u0081\5\r\u0084"+
		"\n\r\3\16\3\16\5\16\u0088\n\16\3\17\3\17\3\17\5\17\u008d\n\17\3\17\5\17"+
		"\u0090\n\17\3\20\3\20\5\20\u0094\n\20\3\20\5\20\u0097\n\20\3\20\5\20\u009a"+
		"\n\20\3\21\3\21\3\21\5\21\u009f\n\21\3\22\3\22\2\2\23\3\3\5\4\7\5\t\6"+
		"\13\7\r\b\17\t\21\n\23\2\25\2\27\2\31\2\33\2\35\2\37\2!\2#\2\3\2\t\4\2"+
		"//\61\61\4\2--//\4\2\61\61\u2046\u2046\t\2\13\17\"\"\u0087\u0087\u00a2"+
		"\u00a2\u2002\u200c\u202a\u202b\u3002\u3002\4\2\62;\u07c2\u07cb\7\2..\60"+
		"\60<<\u00af\u00af\u066d\u066e\7\2##..\60\60==AA\u00ae\2\3\3\2\2\2\2\5"+
		"\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2"+
		"\2\21\3\2\2\2\3C\3\2\2\2\5E\3\2\2\2\7L\3\2\2\2\tP\3\2\2\2\13U\3\2\2\2"+
		"\rZ\3\2\2\2\17_\3\2\2\2\21e\3\2\2\2\23g\3\2\2\2\25i\3\2\2\2\27l\3\2\2"+
		"\2\31\u0083\3\2\2\2\33\u0085\3\2\2\2\35\u0089\3\2\2\2\37\u0091\3\2\2\2"+
		"!\u009e\3\2\2\2#\u00a0\3\2\2\2%&\7/\2\2&\'\7N\2\2\'(\7T\2\2()\7D\2\2)"+
		"D\7/\2\2*+\7/\2\2+,\7T\2\2,-\7T\2\2-.\7D\2\2.D\7/\2\2/\60\7/\2\2\60\61"+
		"\7N\2\2\61\62\7E\2\2\62\63\7D\2\2\63D\7/\2\2\64\65\7/\2\2\65\66\7T\2\2"+
		"\66\67\7E\2\2\678\7D\2\28D\7/\2\29:\7/\2\2:;\7N\2\2;<\7U\2\2<=\7D\2\2"+
		"=D\7/\2\2>?\7/\2\2?@\7T\2\2@A\7U\2\2AB\7D\2\2BD\7/\2\2C%\3\2\2\2C*\3\2"+
		"\2\2C/\3\2\2\2C\64\3\2\2\2C9\3\2\2\2C>\3\2\2\2D\4\3\2\2\2EF\5\33\16\2"+
		"FG\t\2\2\2GH\5\33\16\2HI\t\2\2\2IJ\5\35\17\2J\6\3\2\2\2KM\t\3\2\2LK\3"+
		"\2\2\2LM\3\2\2\2MN\3\2\2\2NO\5\31\r\2O\b\3\2\2\2PQ\5\37\20\2QR\t\4\2\2"+
		"RS\5\37\20\2S\n\3\2\2\2TV\5!\21\2UT\3\2\2\2VW\3\2\2\2WU\3\2\2\2WX\3\2"+
		"\2\2X\f\3\2\2\2Y[\5#\22\2ZY\3\2\2\2[\\\3\2\2\2\\Z\3\2\2\2\\]\3\2\2\2]"+
		"\16\3\2\2\2^`\t\5\2\2_^\3\2\2\2`a\3\2\2\2a_\3\2\2\2ab\3\2\2\2bc\3\2\2"+
		"\2cd\b\b\2\2d\20\3\2\2\2ef\13\2\2\2f\22\3\2\2\2gh\4c|\2h\24\3\2\2\2ij"+
		"\4C\\\2j\26\3\2\2\2km\t\6\2\2lk\3\2\2\2m\30\3\2\2\2np\5\27\f\2on\3\2\2"+
		"\2pq\3\2\2\2qo\3\2\2\2qr\3\2\2\2r\u0084\3\2\2\2su\5\27\f\2ts\3\2\2\2u"+
		"x\3\2\2\2vt\3\2\2\2vw\3\2\2\2w\177\3\2\2\2xv\3\2\2\2y{\t\7\2\2z|\5\27"+
		"\f\2{z\3\2\2\2|}\3\2\2\2}{\3\2\2\2}~\3\2\2\2~\u0080\3\2\2\2\177y\3\2\2"+
		"\2\u0080\u0081\3\2\2\2\u0081\177\3\2\2\2\u0081\u0082\3\2\2\2\u0082\u0084"+
		"\3\2\2\2\u0083o\3\2\2\2\u0083v\3\2\2\2\u0084\32\3\2\2\2\u0085\u0087\5"+
		"\27\f\2\u0086\u0088\5\27\f\2\u0087\u0086\3\2\2\2\u0087\u0088\3\2\2\2\u0088"+
		"\34\3\2\2\2\u0089\u008a\5\27\f\2\u008a\u008c\5\27\f\2\u008b\u008d\5\27"+
		"\f\2\u008c\u008b\3\2\2\2\u008c\u008d\3\2\2\2\u008d\u008f\3\2\2\2\u008e"+
		"\u0090\5\27\f\2\u008f\u008e\3\2\2\2\u008f\u0090\3\2\2\2\u0090\36\3\2\2"+
		"\2\u0091\u0093\5\27\f\2\u0092\u0094\5\27\f\2\u0093\u0092\3\2\2\2\u0093"+
		"\u0094\3\2\2\2\u0094\u0096\3\2\2\2\u0095\u0097\5\27\f\2\u0096\u0095\3"+
		"\2\2\2\u0096\u0097\3\2\2\2\u0097\u0099\3\2\2\2\u0098\u009a\5\27\f\2\u0099"+
		"\u0098\3\2\2\2\u0099\u009a\3\2\2\2\u009a \3\2\2\2\u009b\u009f\5\23\n\2"+
		"\u009c\u009f\5\25\13\2\u009d\u009f\5\27\f\2\u009e\u009b\3\2\2\2\u009e"+
		"\u009c\3\2\2\2\u009e\u009d\3\2\2\2\u009f\"\3\2\2\2\u00a0\u00a1\t\b\2\2"+
		"\u00a1$\3\2\2\2\26\2CLW\\_alqv}\u0081\u0083\u0087\u008c\u008f\u0093\u0096"+
		"\u0099\u009e\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}