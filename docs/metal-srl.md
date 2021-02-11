---
title: Semantic Role Labeling
parent: Metal
has_children: false
nav_order: 5
---

# Semantic Role Labeling

Similar to most works on SRL, we decompose the SRL task into two subtasks: (a) identifying verbal and nominal predicates in a given sentence, and (b) identifying arguments for each predicate. As discussed in the previous section, we observed that the predicate identification subtask trains well in a MTL setting. However, the second subtask, argument identification, did not train well in a joint setting. For this reason, we trained it separately, independently of all other tasks.

Our SRL data for argument identification follows the Metal Full format:

```
A       DT      O       O       1       O       2       O       6
record  NN      O       O       1       A1      2       O       6
date    NN      O       A1      1       O       2       A1      6
has     VBZ     O       O       1       O       2       O       6
n't     RB      O       O       1       O       2       AM-NEG  6
been    VBN     O       O       1       O       2       O       6
set     VBN     O       O       1       O       2       O       6
.       .       O       O       1       O       2       O       6
...
```
Please see the File Formats section for details on this format.

The [configuration file](https://github.com/clulab/processors/blob/master/main/src/main/resources/org/clulab/mtl-en-srla.conf) to train the identification of SRL arguments is the following:

```yml
mtl {
  shardsPerEpoch = 10
  maxEpochs = 100
  epochPatience = 5
  numberOfTasks = 1
  batchSize = 1
  trainer = adam

  layers {
    initial {
      learnedWordEmbeddingSize = 128
      charEmbeddingSize = 32
      charRnnStateSize = 16
      posTagEmbeddingSize = 32
      neTagEmbeddingSize = 16
      distanceEmbeddingSize = 16
      distanceWindowSize = 50
      useIsPredicate = true
      c2i = "org/clulab/c2i-en.txt"
      tag2i = "org/clulab/tag2i-en.txt"
      ne2i = "org/clulab/ne2i-en.txt"
    }

    intermediate1 {
      rnnStateSize = 128
      numLayers = 1
      useHighwayConnections = true
      type = "gru"
    }
  }

  task1 {
    name = "En SRL arguments"
    train = "dynet/en/srl/train.args"
    dev = "dynet/en/srl/dev.args"
    test = "dynet/en/srl/test-wsj.args"
    type = "dual"

    layers {
      final {
        inference = "greedy"
      }
    }
  }

}
```
Note that `mtl.task1.type` is set to "dual", which corresponds to the dual Metal task, where the last forward layer concatenates the embeddings of both argument and head for each prediction (hence the "dual" name). For this task type, the initial layer has the following additional parameters:

Parameter | Description | Default value
--- | --- | ---
`m.l.i.posTagEmbeddingSize` | Size of the learned embeddings for POS tags. If -1, no embeddings are created for POS tags. | -1
`m.l.i.neTagEmbeddingSize` | Size of the learned embeddings for NE labels. If -1, no embeddings are created for NE labels. | -1
`m.l.i.distanceEmbeddingSize` | Size of the learned embeddings for the _relative_ distance between the given predicate and the current token. For example, in the sentence "John ate cake", the distance between the predicate "ate" and "John" is -1. If the value of this parameter is -1, no embeddings are created for these distances. | -1 
`m.l.i.distanceWindowSize` | Window size considered for the above distance values. If the value of this parameter is _k_, we consider values in the window [ -_k_, _k_]. All the distances outside this window are mapped to either -_k + 1_ (if the token appears to the left of the predicate), or _k + 1_ (if the token is to the right of the predicate). | -1
`m.l.i.useIsPredicate` | If true, we create a Boolean feature set to 1 if the current token is the predicate, and 0 otherwise. | false
`m.l.i.tag2i` | File that maps POS tags to unique integer indices that are used internally by `Metal`. The first column in this file contains POS tags; the second contains the corresponding index values. If you use Metal for another language, adjust this file to include the relevant POS tags in that language. The POS tag at index 0 is reserved for UNKNOWN. | ["org/clulab/tag2i-en.txt"](https://github.com/clulab/processors/blob/master/main/src/main/resources/org/clulab/tag2i-en.txt)
`m.l.i.ne2i` | File that maps NE labels to unique integer indices that are used internally by `Metal`. The first column in this file contains NE labels; the second contains the corresponding index values. If you use Metal for another language or with a different NER, adjust this file to include the relevant NE labels. The label at index 0 is reserved for UNKNOWN. | ["org/clulab/ne2i-en.txt"](https://github.com/clulab/processors/blob/master/main/src/main/resources/org/clulab/ne2i-en.txt)

