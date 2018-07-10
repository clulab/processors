POS Tagging Models
==================

`pos-memm-l1-o2-wsj.dat`
-------------

Trained on the Penn Treebank alone. Accuracy on the test partition of WSJ is: `Accuracy = 97.1`. Note that this model performs poorly on Genia, at approximately 87% accuracy.

`pos-memm-l1-o2-wsjgenia.dat`
------------------

Trained an dataset consisting of the Penn Treebank concatenated with Genia. 
Accuracy on the test partition of WSJ is: `Accuracy = 96.99`.
Accuracy on the test partition of Genia is: `Accuracy = 98.1`.

`pos-bimemm-l1-o2-bi10-wsjgenia.dat`
-----------------------------------

Trained a BiMEMM an dataset consisting of the Penn Treebank concatenated with Genia.
Note: the second pass used only 95% of the data due to OOM errors.
Accuracy on the test partition of WSJ is: `Accuracy = 97.09`.
Accuracy on the test partition of Genia is: `Accuracy = 98.04`.

