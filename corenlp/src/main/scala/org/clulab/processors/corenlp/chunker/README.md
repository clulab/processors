Get the dataset from:
http://www.cnts.ua.ac.be/conll2000/chunking/

To train the chunker:
$ sbt 'runMain org.clulab.processors.corenlp.chunker.TrainChunker train.txt.gz test.txt.gz'

the model will be written uncompressed to the current working directory and you need to move it and compress it manually to the path where ShallowNLPProcessor expects it:
models/src/main/resources/org/clulab/processors/corenlp/chunker/chunker.crf.gz
