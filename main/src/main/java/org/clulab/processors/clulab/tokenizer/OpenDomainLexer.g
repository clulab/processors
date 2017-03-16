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

THE: 'the' ;

NUM: (DIGIT+ '/' DIGIT+) | (DIGIT+ '.' DIGIT+);

// some other token
WORD: ALPHANUM+ ;

// punctuation
EOS: PUNCTUATION+ ;

// not alphanum and not punctuation
// OTHER: ~('a'..'z'|'A'..'Z'|'0'..'9'|'.'|'?'|'!'|';'|','|'\t'|' '|'\r'|'\n'|'\u000C');

// skip all white spaces
WHITESPACES : ( '\t' | ' ' | '\r' | '\n'| '\u000C' )+ -> skip ;

// self-explanatory fragments
fragment LOWER_CASE_LETTER: 'a'..'z';
fragment UPPER_CASE_LETTER: 'A'..'Z';
fragment DIGIT: '0'..'9';
fragment ALPHANUM: LOWER_CASE_LETTER | UPPER_CASE_LETTER | DIGIT;
fragment PUNCTUATION: '.'|'?'|'!'|';'|',';
