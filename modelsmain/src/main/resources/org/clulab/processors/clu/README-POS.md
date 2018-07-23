## POS Tagging Models

### `pos-memm-l1-o2-wsj.dat`

Trained on the Penn Treebank alone. Accuracy on the test partition of WSJ is: `Accuracy = 97.1`. 
NB: this model performs poorly on Genia, at approximately 87% accuracy.

### `pos-memm-l1-o2-wsjgenia.dat`

Trained on a dataset consisting of the Penn Treebank concatenated with Genia. 
Accuracy on the test partition of WSJ is: `Accuracy = 96.99`.
Accuracy on the test partition of Genia is: `Accuracy = 98.1`.

### `pos-bimemm-l1-o2-bi10-wsjgenia.dat`

BiMEMM trained on a dataset consisting of the Penn Treebank concatenated with Genia.
NB: the second pass used only 95% of the data due to OOM errors.
Accuracy on the test partition of WSJ is: `Accuracy = 97.09`.
Accuracy on the test partition of Genia is: `Accuracy = 98.04`.

### `pt-pos-bimemm-ud.dat`

Portuguese-language model BiMEMM trained on the concatenation of the Bosque 
(Floresta Sintá(c)tica) treebank and the Google Universal Dependency Treebank, 
available at [universaldependencies.org](https://universaldependencies.org). 
The test partition also includes the Parallel Universal Dependencies (PUD) treebank.
NB: the POS tags are universal tags (e.g., `NOUN` rather than `NN`). 
Accuracy on the combined test partition is: `Accuracy = 93.37`.

### `es-pos-bimemm-ud.dat`

Spanish-language model BiMEMM trained on the concatenation of the AnCora corpus 
and the Google Universal Dependency Treebank, available at 
[universaldependencies.org](https://universaldependencies.org). 
The test partition also includes the Parallel Universal Dependencies (PUD) treebank.
NB: the POS tags are universal tags (e.g., `NOUN` rather than `NN`). 
Accuracy on the combined test partition is: `Accuracy = `.
