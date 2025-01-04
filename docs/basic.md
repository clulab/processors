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
where the `<JAR FILE NAME>` is the name of the file you downloaded in the previous step, `<INPUT TEXT FILE>` is the input file, which contains plain text (just English for now), and `<OUTPUT FILE>` is the name of the file where the `processors` outputs will be saved. The outputs are saved in a format compatible with the [CoNLL-U format](https://universaldependencies.org/format.html). In particular, the first 9 columns (ID, FORM, LEMMA, UPOS, XPOS, FEATS, HEAD, DEPREL, DEPS) are the same as CoNLL-U, with the caveat that XPOS, FEATS, and DEPS are not populated (i.e., they contain `_`). Instead of the 10 column (MISC), we use 5 additional columns:

a. START_OFFSET: start character offset for the current token.
b. END_OFFSET: end character offset for the current token.
c. ENTITY: named or numeric entity label.
d. ENTITY_NORM: normalized entity value for numeric entities, e.g., "2024-01-01" for the phrase "January 1st, 2024".
e. CHUNK: syntactic chunk label, from the [CoNLL-2000 shared task](https://arxiv.org/pdf/cs/0009008).

A few additional notes:
- If `input` is not specified in the command line, i.e., the command line is `java -jar <JAR FILE NAME>`, the software starts an interactive shell where the user can type the text to be parsed and the output is displayed when pressing `Enter`.
- If `output` is not specified in the command line, the CoNLL-U format will displayed in the standard output.
- The input file can be in one of three possible formats:

