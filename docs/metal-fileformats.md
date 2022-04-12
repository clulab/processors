---
title: File Formats
parent: Metal
has_children: false
nav_order: 4
---

# Data File Formats in Metal

Metal currently supports three file formats, all inspired from the simple tabular format used in the CoNLL shared tasks.

## 1. Simple Format

This format stores one token per line. Each token is represented using two columns, where the first column is the word itself, and the second column contains the label to be learned.
Sentences are separated by empty lines.
This format can be used for many common sequence modeling tasks such as named entity recognition or part-of-speech tagging.
The two examples below show a couple of annotated sentences from these two tasks:

```
Only    O
France  B-LOC
and     O
Britain B-LOC
backed  O
Fischler        B-PER
's      O
proposal        O
.       O

```

```
Pierre  NNP
Vinken  NNP
,       ,
61      CD
years   NNS
old     JJ
,       ,
will    MD
join    VB
the     DT
board   NN
as      IN
a       DT
nonexecutive    JJ
director        NN
Nov.    NNP
29      CD
.       .

Mr.     NNP
Vinken  NNP
is      VBZ
```

## 2. Simple Extended Format

## 3. Full Format

