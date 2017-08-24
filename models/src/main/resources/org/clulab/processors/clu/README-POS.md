POS Tagging Models
==================

`pos-wsj.dat`
-------------

Trained on the Penn Treebank alone. Accuracy on the test partition of WSJ is: `Accuracy = 96.49646643109541 (54617/56600)`. Note that this model performs poorly on Genia, at approximately 85% accuracy.

`pos-wsjgenia.dat`
------------------

Trained an dataset consisting of two copies of the Penn Treebank plus one copy of Genia. 
Accuracy on the test partition of WSJ is: `Accuracy = 95.99181426857668 (54412/56684)`.
Accuracy on the test partition of Genia is: `Accuracy = 95.78832178231713 (34138/35639)`.

