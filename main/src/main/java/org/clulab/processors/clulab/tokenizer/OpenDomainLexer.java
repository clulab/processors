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
		THE=1, OTHER=2, PERIOD=3, CATCH_ALL=4, WHITESPACES=5;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] ruleNames = {
		"THE", "OTHER", "PERIOD", "CATCH_ALL", "WHITESPACES", "LOWER_CASE_LETTER", 
		"UPPER_CASE_LETTER", "DIGIT", "ALPHANUM"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'the'", null, "'.'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, "THE", "OTHER", "PERIOD", "CATCH_ALL", "WHITESPACES"
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
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\2\7\67\b\1\4\2\t\2"+
		"\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\3\2\3"+
		"\2\3\2\3\2\3\3\6\3\33\n\3\r\3\16\3\34\3\4\3\4\3\5\6\5\"\n\5\r\5\16\5#"+
		"\3\6\6\6\'\n\6\r\6\16\6(\3\6\3\6\3\7\3\7\3\b\3\b\3\t\3\t\3\n\3\n\3\n\5"+
		"\n\66\n\n\2\2\13\3\3\5\4\7\5\t\6\13\7\r\2\17\2\21\2\23\2\3\2\3\5\2\13"+
		"\f\16\17\"\"\67\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3"+
		"\2\2\2\3\25\3\2\2\2\5\32\3\2\2\2\7\36\3\2\2\2\t!\3\2\2\2\13&\3\2\2\2\r"+
		",\3\2\2\2\17.\3\2\2\2\21\60\3\2\2\2\23\65\3\2\2\2\25\26\7v\2\2\26\27\7"+
		"j\2\2\27\30\7g\2\2\30\4\3\2\2\2\31\33\5\23\n\2\32\31\3\2\2\2\33\34\3\2"+
		"\2\2\34\32\3\2\2\2\34\35\3\2\2\2\35\6\3\2\2\2\36\37\7\60\2\2\37\b\3\2"+
		"\2\2 \"\n\2\2\2! \3\2\2\2\"#\3\2\2\2#!\3\2\2\2#$\3\2\2\2$\n\3\2\2\2%\'"+
		"\t\2\2\2&%\3\2\2\2\'(\3\2\2\2(&\3\2\2\2()\3\2\2\2)*\3\2\2\2*+\b\6\2\2"+
		"+\f\3\2\2\2,-\4c|\2-\16\3\2\2\2./\4C\\\2/\20\3\2\2\2\60\61\4\62;\2\61"+
		"\22\3\2\2\2\62\66\5\r\7\2\63\66\5\17\b\2\64\66\5\21\t\2\65\62\3\2\2\2"+
		"\65\63\3\2\2\2\65\64\3\2\2\2\66\24\3\2\2\2\7\2\34#(\65\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}