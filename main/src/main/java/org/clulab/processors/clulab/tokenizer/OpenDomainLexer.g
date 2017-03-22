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

// left and right double quotes, including Latex quotes
QUOTES: '"' | '``' | '\'\'' | '\u2018' | '\u201c' | '\u2019' | '\u201d';

// common date formats
DATE: ONE_TO_TWO_DIGITS ('-'|'/') ONE_TO_TWO_DIGITS ('-'|'/') TWO_TO_FOUR_DIGITS;

// positive or negative numbers
NUMBER: ('-'|'+')? NUM;

// numeric fractions
FRACTION: ONE_TO_FOUR_DIGITS ('/' | '\u2044') ONE_TO_FOUR_DIGITS;

// some other token
// note that we do not hard code abbreviations in the lexer.
// abbreviations are handled in the org.clulab.processors.clulab.tokenizer.Tokenizer class.
WORD: ALPHANUM+ (('.'|'!'|'?'|'-'|'_'|'\'') ALPHANUM+ )* ;

// Twitter user names and hashtags
TWITTER_NAME: '@' (LOWER_CASE_LETTER|UPPER_CASE_LETTER|'_') (LOWER_CASE_LETTER|UPPER_CASE_LETTER|'_'|DIGIT)*;
TWITTER_HASHTAG: '#' LETTER+;

// typical file names
FILENAME: ALPHANUM+ ('.' ALPHANUM+)* ('.' FILENAME_EXT);

// weird names of programming languages
PROGRAMMING_LANGUAGES: (('c'|'C') '+''+') | (('c'|'C') '#') | (('f'|'F') '#');

// URLs
FULLURL: 'http' 's'? '://' URL_BLOCK1+ URL_END1 ;
LIKELYURL_WWW: 'www.' (URL_BLOCK2+ '.')+ ((LOWER_CASE_LETTER|UPPER_CASE_LETTER) (LOWER_CASE_LETTER|UPPER_CASE_LETTER) (LOWER_CASE_LETTER|UPPER_CASE_LETTER)? (LOWER_CASE_LETTER|UPPER_CASE_LETTER)?) ;
LIKELYURL_COM: ((URL_BLOCK3+ '.')+ ('com'|'net'|'org'|'edu'|'name'|'info')) ('/' URL_END2+ URL_END3)? ;

// Email addresses
EMAIL: ('&lt;'|'<')? EMAIL_USER '@' (EMAIL_DOMAIN '.')* EMAIL_DOMAIN ('&gt;'|'>')? ;

// SGML tags
SGML: '<' '/'? ~('<'|'>'|'.'|','|'!'|'?'|'|'|'('|')'|'{'|'}')+ '>' ;

// HTML characters
HTML_CODE: '&' (LOWER_CASE_LETTER | UPPER_CASE_LETTER) (LOWER_CASE_LETTER | UPPER_CASE_LETTER) (LOWER_CASE_LETTER | UPPER_CASE_LETTER)? (LOWER_CASE_LETTER | UPPER_CASE_LETTER)? ';' ;

// Common smileys
SMILEY: ('<'|'>')? (':'|';'|'=') ('-'|'o'|'*'|'\'')? ('('|')'|'D'|'P'|'d'|'p'|'O'|'\\'|'{'|'@'|'|'|'['|']') ;

// TODO: phone numbers

// punctuation
EOS: PUNCTUATION (WHITESPACE? PUNCTUATION)* ;

// skip all white spaces
WHITESPACE: ('\t'|' '|'\r'|'\n'|'\u000C'| '\u2028'|'\u2029'|'\u000B'|'\u0085'|'\u00A0'|('\u2000'..'\u200A')|'\u3000')+ -> skip ;

// handle characters which failed to match any other token
ErrorCharacter: . ;

//
// self-explanatory fragments used to construct more complex lexer rules
//
fragment LOWER_CASE_LETTER: 'a'..'z';
fragment UPPER_CASE_LETTER: 'A'..'Z';
fragment SPLET: '&' ('a'|'e'|'i'|'o'|'u'|'A'|'E'|'I'|'O'|'U')('acute'|'grave'|'uml');
fragment LETTER: LOWER_CASE_LETTER | UPPER_CASE_LETTER | SPLET | '\u00AD' | ('\u0237'..'\u024F') | ('\u02C2'..'\u02C5') | ('\u02D2'..'\u02DF') | ('\u02E5'..'\u02FF') | ('\u0300'..'\u036F') | ('\u0370'..'\u037D') | '\u0384' | '\u0385' | '\u03CF' | '\u03F6' | ('\u03FC'..'\u03FF') | ('\u0483'..'\u0487') | '\u04CF' | ('\u04F6'..'\u04FF') | ('\u0510'..'\u0525') | ('\u055A'..'\u055F') | ('\u0591'..'\u05BD') | '\u05BF' | '\u05C1' | '\u05C2' | '\u05C4' | '\u05C5' | '\u05C7' | ('\u0615'..'\u061A') | ('\u063B'..'\u063F') | ('\u064B'..'\u065E') | '\u0670' | ('\u06D6'..'\u06EF') | ('\u06FA'..'\u06FF') | '\u070F' | '\u0711' | ('\u0730'..'\u074F') | ('\u0750'..'\u077F') | ('\u07A6'..'\u07B1') | ('\u07CA'..'\u07F5') | '\u07FA' | ('\u0900'..'\u0903') | '\u093C' | ('\u093E'..'\u094E') | ('\u0951'..'\u0955') | ('\u0962'..'\u0963') | ('\u0981'..'\u0983') | ('\u09BC'..'\u09C4') | '\u09C7' | '\u09C8' | ('\u09CB'..'\u09CD') | '\u09D7' | '\u09E2' | '\u09E3' | ('\u0A01'..'\u0A03') | '\u0A3C' | ('\u0A3E'..'\u0A4F') ('\u0A81'..'\u0A83') | ('\u0ABC'..'\u0ACF') | '\u0B82' | ('\u0BBE'..'\u0BC2') | ('\u0BC6'..'\u0BC8') | ('\u0BCA'..'\u0BCD') | ('\u0C01'..'\u0C03') | ('\u0C3E'..'\u0C56') | ('\u0D3E'..'\u0D44') | ('\u0D46'..'\u0D48') | ('\u0E30'..'\u0E3A') | ('\u0E47'..'\u0E4E') | ('\u0EB1'..'\u0EBC') | ('\u0EC8'..'\u0ECD');
fragment DIGIT: ('0'..'9')|('\u07C0'..'\u07C9');
fragment ALPHANUM: LETTER | DIGIT;
fragment NUM: DIGIT+ | (DIGIT* (('.'|':'|','|'\u00AD'|'\u066B'|'\u066C') DIGIT+)+);
fragment ONE_TO_TWO_DIGITS: DIGIT DIGIT?;
fragment TWO_TO_FOUR_DIGITS: DIGIT DIGIT DIGIT? DIGIT?;
fragment ONE_TO_FOUR_DIGITS: DIGIT DIGIT? DIGIT? DIGIT?;
fragment PUNCTUATION: '.'|'?'|'!'|';'|',';
fragment FILENAME_EXT:  'bat'|'bmp'|'c'|'class'|'cgi'|'cpp'|'dll'|'doc'|'docx'|'exe'|'gif'|'gz'|'h'|'htm'|'html'|'jar'|'java'|'jpeg'|'jpg'|'mov'|'mp3'|'pdf'|'php'|'pl'|'png'|'ppt'|'pptx'|'ps'|'py'|'scala'|'sql'|'tar'|'tgz'|'txt'|'wav'|'xml'|'zip';
fragment URL_BLOCK1: ~(' '|'\t'|'\n'|'\f'|'\r'|'"'|'<'|'>'|'|'|'('|')'|'{'|'}');
fragment URL_BLOCK2: ~(' '|'\t'|'\n'|'\f'|'\r'|'"'|'<'|'>'|'|'|'.'|','|'!'|'?'|'('|')'|'{'|'}');
fragment URL_BLOCK3: ~(' '|'\t'|'\n'|'\f'|'\r'|'"'|'`'|'\''|'<'|'>'|'|'|'.'|','|'!'|'?'|'-'|'_'|'$'|'('|')'|'{'|'}');
fragment URL_END1:   ~(' '|'\t'|'\n'|'\f'|'\r'|'"'|'<'|'>'|'|'|'('|')'|'{'|'}'|'-'|'.'|'!'|'?'|',');
fragment URL_END2:   ~(' '|'\t'|'\n'|'\f'|'\r'|'"'|'<'|'>'|'|'|'('|')');
fragment URL_END3:   ~(' '|'\t'|'\n'|'\f'|'\r'|'"'|'<'|'>'|'|'|'.'|'!'|'?'|','|'-'|'('|')'|'{'|'}');
fragment EMAIL_USER: (('a'..'z')|('A'..'Z')|('0'..'9')) ~(' '|'\t'|'\n'|'\f'|'\r'|'"'|'<'|'>'|'|'|'('|')'|'{'|'}'|'\u00A0')* ;
fragment EMAIL_DOMAIN: ~(' '|'\t'|'\n'|'\f'|'\r'|'"'|'<'|'>'|'|'|'('|')'|'{'|'}'|'.'|'\u00A0'|',')+ ;
