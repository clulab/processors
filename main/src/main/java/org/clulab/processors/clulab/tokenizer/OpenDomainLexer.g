//
// antlr4 grammar for open-domain tokenization
// generate Java classes for this grammar with the command:
//   java -jar antlr-4.6-complete.jar main/src/main/java/org/clulab/processors/clulab/tokenizer/OpenDomain.g
// author: mihai
//

lexer grammar OpenDomainLexer;

options {
  language = Java;
}

@lexer::header {
  package org.clulab.processors.clulab.tokenizer;
}

// parentheses in Treebank and OntoNotes
PARENS: '-LRB-' | '-RRB-' | '-LCB-' | '-RCB-' | '-LSB-' | '-RSB-';

// common date formats
DATE: ONE_TO_TWO_DIGITS ('-'|'/') ONE_TO_TWO_DIGITS ('-'|'/') TWO_TO_FOUR_DIGITS;

// positive or negative numbers
NUMBER: ('-'|'+')? NUM;

// numeric fractions
FRACTION: ONE_TO_FOUR_DIGITS ('/' | '\u2044') ONE_TO_FOUR_DIGITS;

// some other token
WORD: ALPHANUM+ ;

// punctuation
EOS: PUNCTUATION+ ;

// skip all white spaces
WHITESPACE: ('\t'|' '|'\r'|'\n'|'\u000C'| '\u2028'|'\u2029'|'\u000B'|'\u0085'|'\u00A0'|('\u2000'..'\u200A')|'\u3000')+ -> skip ;

// handle characters which failed to match any other token
ErrorCharacter: . ;

//
// self-explanatory fragments used to construct more complex lexer rules
//
fragment LOWER_CASE_LETTER: 'a'..'z';
fragment UPPER_CASE_LETTER: 'A'..'Z';
// TODO: fragment LETTER: 
fragment DIGIT: ('0'..'9')|('\u07C0'..'\u07C9');
fragment NUM: DIGIT+ | (DIGIT* (('.'|':'|','|'\u00AD'|'\u066B'|'\u066C') DIGIT+)+);
fragment ONE_TO_TWO_DIGITS: DIGIT DIGIT?;
fragment TWO_TO_FOUR_DIGITS: DIGIT DIGIT DIGIT? DIGIT?;
fragment ONE_TO_FOUR_DIGITS: DIGIT DIGIT? DIGIT? DIGIT?;
fragment ALPHANUM: LOWER_CASE_LETTER | UPPER_CASE_LETTER | DIGIT;
fragment PUNCTUATION: '.'|'?'|'!'|';'|',';
