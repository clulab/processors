---
title: Training a Single Task
parent: Metal
has_children: false
nav_order: 1
---

# Training a Single Task

Let's start with a simpler task: named entity recognition (NER). The steps in training such a task in `Metal` are:

## 1. Preparing the datasets

`Metal` expects the data for most of its tasks (note: other file formats are supported, see the File Formats section for details) to be formatted in a simple two-column format, where the first column contains the words, and the second column contains the labels to be learned. An empty line should be included between sentences. For example, our NER dataset looks like this:

```
Peter   B-PER
Blackburn       I-PER

BRUSSELS        B-LOC
1996-08-22      O

The     O
European        B-ORG
Commission      I-ORG
...
```
The labels do not have to be in the Beginning/Inside/Outside (BIO) format shown above. For example, they can be part-of-speech tags (see next section). But, if they are in the BIO format, `Metal` enforces additional constraints at runtime, e.g., an `I-X` tag cannot follow a `B-Y` one. 

## 2. Configuring the task

Edit the task configuration to adjust it to your needs. For example, this is the [configuration file](https://github.com/clulab/processors/blob/master/main/src/main/resources/org/clulab/mtl-en-ner.conf) for the NER models we currently use:

```yml
mtl {
  maxEpochs = 50
  epochPatience = 5
  numberOfTasks = 1

  layers {
    initial {
      learnedWordEmbeddingSize = 128
      charEmbeddingSize = 32
      charRnnStateSize = 16
      c2i = "org/clulab/c2i-en.txt"
    }

    intermediate1 {
      rnnStateSize = 128
      useHighwayConnections = true
      numLayers = 1
    }
  }

  task1 {
    name = "En NER"
    train = "dynet/en/ner/train.txt"
    dev = "dynet/en/ner/dev.txt"
    test = "dynet/en/ner/test.txt"
    type = "basic"

    layers {
      final {
        inference = "viterbi"
      }
    }
  }
}
```

Minimally, you have to adjust the `mtl.task1.train`, `mtl.task1.dev`, and `mtl.task1.test` parameters to point to your dataset. The above configuration should produce (at or near) state-of-the-art performance as is. 
This configuration replicates the LSTM-CRF approach in [Lample et al., 2016](https://arxiv.org/pdf/1603.01360.pdf).
So, if you just want to get started quickly, you can move on to the next step, training a `Metal` classifier. 

For the more curious reader, below is a description of the parameters in the configuration file for a single task:

Parameter | Description | Default value
--- | --- | ---
`mtl.maxEpochs` | Maximum number of epochs for training | 100
`mtl.epochPatience` | Early stopping threshold, i.e., stop training if performance on dev does not improve for this many epochs | 5
`mtl.numberOfTasks` | How many tasks are trained jointly in this MTL setup | N/A
`mtl.learningRate` | Learning rate | 0.001
`mtl.trainer` | Type of optimizer used. Accepted values: "adam", "rmsprop", and "sgd". | "adam"
`mtl.batchSize` | How many sentences are included in each batch | 1
`mtl.layers` | Configuration of layers shared between all tasks | N/A
`m.l.initial` | Initial layer in the network, which creates word embeddings. Each word embedding is a concatenation of at least three vectors: (a) a static embedding, i.e., an embedding that is _not_ updated during training, which by default is set to GloVe embeddings (see the paragraph after the table for information on how to change it); (b) a learned word embedding that is adjusted during training; and (c) a character encoding that is produced by a biLSTM over the characters in the word. For more specialized tasks, there are additional components in the word embedding (see the Semantic Role Labeling section). | N/A
`m.l.i.learnedWordEmbeddingSize` | Size of the learned word embedding | 128
`m.l.i. charEmbeddingSize` | Size of the learned character embedding that is the input to the character biLSTM | 32 
`m.l.i.charRnnStateSize` | Size of the character LSTM's hidden state. Because we use a biLSTM to encode characters, the output state for the biLSTM is twice this value.  | 16 
`m.l.i.c2i` | File that maps characters to unique integer indices that are used internally by `Metal`. The provided file for English contains characters that were extracted from the 1-billion-word-language-modeling-benchmark-r13output. We use two special characters: 0, which stands for UNKNOWN, and 1, which stands for  the virtual character end of sentence (EOS). The first column in this file contains characters stored as integers; the second contains the index value. If you use `Metal` for another language, adjust this file to include the relevant characters in that language. | ["org/clulab/c2i-en.txt"](https://github.com/clulab/processors/blob/master/main/src/main/resources/org/clulab/c2i-en.txt)
`mtl.layers.intermediate1` | The intermediate layer that operates on top of the vectors produced by the initial layer. Currently this layer is always a biLSTM, but it doesn't have to be. Any layer that takes a sequence of vectors and produces another sequence of vectors would work, e.g., CNNs or transformers.  | 
`mtl.layers.intermediate`_i_ | `Metal` accepts up to 10 intermediate layers. Just number them using continuous integers, from `intermediate1` to `intermediate10`. | N/A
`m.l.i.rnnStateSize` | Size of the word-level LSTM's hidden state. Because we use a biLSTM in this layer, the output state for the biLSTM is twice this value. | 128
`m.l.i.useHighwayConnections` | If true, the state vector corresponding to each word concatenates the biLSTM state with the input embedding of the word. | false
`m.l.i.numLayers` | Number of layers in the biLSTM | 1
`mtl.task1` | Information for the actual task to be learned | N/A
`m.t.name` | Name of the task, to be used for logging (any arbitrary text works) | N/A
`m.t.train` | File name with the training dataset in the two-column format described above | N/A
`m.t.dev` | File name with the development dataset | N/A
`m.t.test` | File name with the testing dataset | N/A
`m.t.type` | Type of task to be learned. "basic" indicates any task that is represented in the two-column format described above. See the Semantic Role Labeling section for an example of a different task. | "basic"
`m.t.layers` | Layers that are unique to this task, i.e., are _not_ shared with any other task. This block _must_ contain at least a final layer that produces the labels to be learned. | N/A
`m.t.l.final` | The final layer for this task. This is a feed forward layer with a softmax at the end, where the softmax layer is applied over all labels seen in training. | N/A
`m.t.l.f.inference` | Type of inference to be used for this layer: "greedy" implements a left-to-right greedy prediction. "viterbi" implements a CRF layer. | "greedy"
`m.t.l.f.nonlinearity` | If defined, applies a nonlinear transformation before the softmax. Current values accepted are: "relu", "tanh". | 


As mentioned above, `Metal` comes preconfigured with GloVe embeddings, which are used in a _static_ way, i.e., they are not updated during training. This design choice was made so that this large resource can be shared between different tasks. These words embeddings are configured in a separate configuration file, called `glove.conf`, which by default looks like this:

```
glove {
  matrixResourceName = "/org/clulab/glove/glove.840B.300d.10f.txt"
  isResource = true
}
```
Note that because one of our design goals was to minimize deep learning resources, we pre-filtered GloVe embeddings to only contain words that occur more than 10 times in the Gigaword corpus. If you wish to change the type of word embeddings you use, adjust the `glove.matrixResourceName` to the file name with your embeddings, and set `glove.isResource` to `false`, to indicate that this is a file on disk not a resource in the class path. 


## 3. Training a classifier for the task

Once you have your configuration file, run this command to train a classifier:
```
sbt 'runMain org.clulab.dynet.Metal -conf <YOUR-CONFIG-FILE> -train <YOUR-MODEL-NAME>'
```

For example, we train our NER with this command:
```
sbt 'runMain org.clulab.dynet.Metal -conf org/clulab/mtl-en-ner.conf -train mtl-en-ner'
```

This command will output various statistics as training progresses, e.g., cumulative loss, performance after each epoch. For example, here is some sample output from training the above NER:

```
[dynet] random seed: 2522620396
[dynet] allocating memory: 512MB
[dynet] memory allocation done.
20:20:59.155 [run-main-0] DEBUG org.clulab.dynet.Utils - DyNet initialization complete.
20:20:59.232 [run-main-0] DEBUG org.clulab.dynet.TaskManager - Reading task 1 (En NER)...
20:20:59.549 [run-main-0] DEBUG org.clulab.dynet.TaskManager - ============ starting task 1 ============
20:20:59.549 [run-main-0] DEBUG org.clulab.dynet.TaskManager - Read 14987 training sentences for task 1, with shard size 1499.
20:20:59.549 [run-main-0] DEBUG org.clulab.dynet.TaskManager - Read 3466 development sentences for task 1.
20:20:59.549 [run-main-0] DEBUG org.clulab.dynet.TaskManager - Read 3684 testing sentences for task 1.
20:20:59.549 [run-main-0] DEBUG org.clulab.dynet.TaskManager - Using taskWeight = 1.0
20:20:59.550 [run-main-0] DEBUG org.clulab.dynet.TaskManager - Task type = basic.
20:20:59.550 [run-main-0] DEBUG org.clulab.dynet.TaskManager - ============ completed task 1 ============
20:20:59.550 [run-main-0] DEBUG org.clulab.dynet.TaskManager - Read 1 tasks from config file.
20:20:59.565 [run-main-0] INFO  org.clulab.dynet.Metal - Started epoch 0.
20:21:00.247 [run-main-0] INFO  org.clulab.utils.Sourcer$ - Sourcing resource file:/work/msurdeanu/sbttmp/sbt_aadf32eb/target/10bf1758/glove-840b-300d-10f-1.0.0.jar!/org/clulab/glove/glove.840B.300d.10f.txt
20:21:56.462 [run-main-0] DEBUG o.clulab.dynet.ConstEmbeddingsGlove - Completed loading word embeddings of dimension 300 for 778102 words.
20:21:56.573 [run-main-0] DEBUG org.clulab.dynet.Metal - Summary of layersPerTask(0):
20:21:56.574 [run-main-0] DEBUG org.clulab.dynet.Metal - initial = EmbeddingLayer(460) intermediate (1) = RnnLayer(460, 716)
20:21:56.574 [run-main-0] DEBUG org.clulab.dynet.Metal - Summary of layersPerTask(1):
20:21:56.574 [run-main-0] DEBUG org.clulab.dynet.Metal - final = ViterbiForwardLayer(716, 11)
20:22:22.333 [run-main-0] INFO  org.clulab.dynet.Metal - Cumulative loss: 0.5874771406676204
20:23:03.994 [run-main-0] INFO  org.clulab.dynet.Metal - Cumulative loss: 0.22642924679232757
20:23:37.110 [run-main-0] INFO  org.clulab.dynet.Metal - Cumulative loss: 0.1477900150116382
...
21:55:26.605 [run-main-0] DEBUG org.clulab.dynet.Metal - Started evaluation on the development dataset for task 1 (En NER)...
21:55:57.477 [run-main-0] INFO  org.clulab.dynet.Metal - Accuracy on 3466 development sentences for task 1 (En NER): 98.63
21:55:57.477 [run-main-0] INFO  org.clulab.dynet.Metal - Precision on 3466 development sentences for task 1 (En NER): 94.06
21:55:57.477 [run-main-0] INFO  org.clulab.dynet.Metal - Recall on 3466 development sentences for task 1 (En NER): 93.03
21:55:57.477 [run-main-0] INFO  org.clulab.dynet.Metal - Micro F1 on 3466 development sentences for task 1 (En NER): 93.54
21:55:57.477 [run-main-0] INFO  org.clulab.dynet.Metal -        P/R/F1 for label I-LOC (257): 94.44 / 85.99 / 90.02
21:55:57.477 [run-main-0] INFO  org.clulab.dynet.Metal -        P/R/F1 for label B-MISC (922): 92.15 / 89.15 / 90.62
21:55:57.477 [run-main-0] INFO  org.clulab.dynet.Metal -        P/R/F1 for label B-LOC (1837): 95.96 / 95.86 / 95.91
21:55:57.477 [run-main-0] INFO  org.clulab.dynet.Metal -        P/R/F1 for label I-PER (1307): 96.87 / 97.32 / 97.09
21:55:57.477 [run-main-0] INFO  org.clulab.dynet.Metal -        P/R/F1 for label I-MISC (346): 89.65 / 75.14 / 81.76
21:55:57.477 [run-main-0] INFO  org.clulab.dynet.Metal -        P/R/F1 for label B-PER (1842): 95.21 / 97.23 / 96.21
21:55:57.477 [run-main-0] INFO  org.clulab.dynet.Metal -        P/R/F1 for label I-ORG (751): 90.75 / 87.61 / 89.15
21:55:57.477 [run-main-0] INFO  org.clulab.dynet.Metal -        P/R/F1 for label * (8603): 94.06 / 93.03 / 93.54
21:55:57.477 [run-main-0] INFO  org.clulab.dynet.Metal -        P/R/F1 for label B-ORG (1341): 91.03 / 90.9 / 90.97
21:55:57.477 [run-main-0] INFO  org.clulab.dynet.Metal - Average accuracy across 1 tasks: 98.63
21:55:57.477 [run-main-0] INFO  org.clulab.dynet.Metal - Average P/R/F1 across 1 tasks: 94.06 / 93.03 / 93.54
21:55:57.477 [run-main-0] INFO  org.clulab.dynet.Metal - Best epoch so far is epoch 3 with an average F1 of 93.81, and average accuracy of 98.72.
...
```

`Metal` saves the model after each epoch. Note that a model in `Metal` consists of two files: one with the `.rnn` extension, which contains the DyNet parameters, and another with the `.x2i` extension, which contains other necessary meta data such as dictionaries of words and characters. Both files have the prefix provided in the command line. For example, the model files saved after epoch 3 by the previous command are called: `mtl-en-ner-epoch3.rnn` and `mtl-en-ner-epoch3.x2i`. 

## 4. Testing a trained model

Once training completes at least one epoch, you can start testing your trained model. For example, if you want to test the model after epoch 3, run the command:

```
sbt 'runMain org.clulab.dynet.Metal -conf org/clulab/mtl-en-ner.conf -test mtl-en-ner-epoch3'
```

The above command reports some internal evaluation measures such as accuracy and micro F1 scores for each label. 
It also saves the classifier's output on the test data as `task1.test.output`. Note: the scores reported by `Metal` are not the official CoNLL scores. If you want to run the official CoNLL scorer on the output file, run:
```
./scripts/conlleval < task1.test.output
```

`Metal` also includes a simple shell, so you can test your classifiers on arbitrary input texts. For example, to start the shell using the model saved after epoch3, run:

```
sbt 'runMain org.clulab.dynet.Metal -shell mtl-en-ner-epoch3'
```
This allows you to type text and see the model's output for it. For example:
```
(shell)>>> John Doe visited France .
Input words: John, Doe, visited, France, .
Labels for task #0: B-PER, I-PER, O, B-LOC, O
```
Important note: this shell only includes a white-space tokenizer, so, please separate the words in your text with white spaces, as in the example above.
