---
title: Basic Usage
has_children: false
nav_order: 2
---

# Basic Usage

If you simply need to parse some text files and are not interested in installing from source, please follow the following steps:

1. Download the latest "fat" jar from [here](http://surdeanu.info/mihai/processors/processors-10.0.1-SNAPSHOT.jar).

2. Parse one file at a time using this command:
```
java -jar <JAR FILE NAME> -input <INPUT TEXT FILE> -output <OUTPUT FILE>
```
where the `<JAR FILE NAME>` is the name of the file you downloaded in the previous step, `<INPUT TEXT FILE>` is the input file, which contains plain text (just English for now), and `<OUTPUT FILE>` is the name of the file where the `processors` outputs will be saved. The outputs are saved in a format compatible with the [CoNLL-U format](https://universaldependencies.org/format.html). In particular, the first 9 columns (`ID, FORM, LEMMA, UPOS, XPOS, FEATS, HEAD, DEPREL, DEPS`) are the same as CoNLL-U, with the caveat that `XPOS`, `FEATS`, and `DEPS` are not populated (i.e., they contain `_`). Instead of the 10th column (`MISC`), we use 5 additional columns:

- START_OFFSET: start character offset for the current token.  
- END_OFFSET: end character offset for the current token.  
- ENTITY: named or numeric entity label.  
- ENTITY_NORM: normalized entity value for numeric entities, e.g., "2024-01-01" for the phrase "January 1st, 2024".  
- CHUNK: syntactic chunk label, from the [CoNLL-2000 shared task](https://arxiv.org/pdf/cs/0009008).  

For example, if the input file `input.txt` contains the following raw text:
```
John Doe visited China. His visit was on Jan 1st, 2024.
```
the command line `java -jar <JAR FILE NAME> -input input.txt -output output.txt` produces the following output in `output.txt`:

```
1       John    john    NNP     _       _       2       compound        _       0       4       B-PER   _       B-NP  
2       Doe     doe     NNP     _       _       3       nsubj   _       5       8       I-PER   _       I-NP  
3       visited visit   VBD     _       _       0       root    _       9       16      O       _       B-VP  
4       China   china   NNP     _       _       3       dobj    _       17      22      B-LOC   _       B-NP  
5       .       .       .       _       _       3       punct   _       23      24      O       _       O  

1       His     his     PRP$    _       _       2       nmod:poss       _       26      29      O       _       B-NP  
2       visit   visit   NN      _       _       5       nsubj   _       30      35      O       _       I-NP  
3       was     be      VBD     _       _       5       cop     _       36      39      O       _       B-VP  
4       on      on      IN      _       _       5       case    _       40      42      O       _       B-PP  
5       Jan     jan     NNP     _       _       0       root    _       43      46      B-DATE  2024-01-01      B-NP  
6       1st     1st     CD      _       _       5       nummod  _       47      50      I-DATE  2024-01-01      I-NP  
7       ,       ,       ,       _       _       5       punct   _       51      52      I-DATE  2024-01-01      I-NP  
8       2024    2024    CD      _       _       5       nummod  _       53      57      I-DATE  2024-01-01      I-NP  
9       .       .       .       _       _       5       punct   _       60      61      O       _       O  
  
```

# Slightly less Basic Usage

If `input` is not specified in the command line, i.e., the command line is simply `java -jar <JAR FILE NAME>`, the software starts an interactive shell where the user can type the text to be parsed and the output is displayed when pressing `Enter`.

If `output` is not specified in the command line, the CoNLL-U format will be displayed in the standard output.

The input file can be in one of three possible formats:

1. Raw, natural language text. This is the default option, which requires no additional command line parameters.  
2. If the parameter `-sentences` is specified, the input file should contain one sentence per line. The sentences are not tokenized.  
3. If the parameter `-tokens` is specified, the input file should contain one sentence per line, and sentences must be pre-tokenized using white spaces. For example, if the input file contains one, untokenized sentence per line, as in:
```
John Doe visited China.  
His visit was on Jan 1st, 2024.  
```
the command `java -jar <JAR FILE NAME> -input input.txt -sentences -output output.txt` produces the same output as above (with start end end character offsets adjusted).

If the input file contains one, tokenized sentence per line, as in:
```
John Doe visited China .  
His visit was on Jan 1st , 2024 .  
```
the command `java -jar <JAR FILE NAME> -input input.txt -tokens -output output.txt` produces the same output as above.

