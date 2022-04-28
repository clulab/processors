import argparse
from pytorch.metal import Metal
from pyhocon import ConfigFactory
from pytorch.taskManager import TaskManager
import pytorch.eisner as eisner
from pytorch.seqScorer import *
from sequences.rowReaders import MetalRowReader
from sequences.columnReader import ColumnReader
from pytorch.constEmbeddingsGlove import ConstEmbeddingsGlove
from pytorch.utils import *

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('--heads', type=str, help='Filename of the head model.')
    parser.add_argument('--labels', type=str, help='Filename of the label model.')
    parser.add_argument('--test', type=str, help='Filename of the test set.')
    args = parser.parse_args()

    config = ConfigFactory.parse_file(f'../resources/org/clulab/mtl-en-depsh.conf')
    taskManager = TaskManager(config, 0)
    heads = Metal.load(args.heads)
    heads_mtl = Metal(taskManager, heads)
    labels = Metal.load(args.labels)
    labels_mtl = Metal(taskManager, labels)

    sentences = ColumnReader.readColumns(args.test)
    print(f"Read {len(sentences)} sentences.")

    reader = MetalRowReader()
    scoreCountsByLabel = ScoreCountsByLabel()

    for sent in sentences:
        annotatedSentences = reader.toAnnotatedSentences(sent)
        for asent in annotatedSentences[:1]:
            sentence = asent[0]
            goldLabels = [lbl.label for lbl in asent[1]]
            modHeadPairs = getModHeadPairs(asent[1])

            constEmbeddings = ConstEmbeddingsGlove.get_ConstLookupParams()

            preds = eisner.ensembleParser(heads_mtl, labels_mtl, asent, constEmbeddings, 5, 0.6, True)
            predLabels = [p[0] for p in preds]

            sc = SeqScorer.f1(goldLabels, preds)
            scoreCountsByLabel.incAll(sc)

    print (f"Accuracy on {len(sentences)} {name} sentences: {scoreCountsByLabel.accuracy()}")
    print (f"Precision on {len(sentences)} {name} sentences: {scoreCountsByLabel.precision()}")
    print (f"Recall on {len(sentences)} {name} sentences: {scoreCountsByLabel.recall()}")
    print (f"Micro F1 on {len(sentences)} {name} sentences: {scoreCountsByLabel.f1()}")
    for label in scoreCountsByLabel.labels():
        print (f"\tP/R/F1 for label {label} ({scoreCountsByLabel.map[label].gold}): {scoreCountsByLabel.precision(label)} / {scoreCountsByLabel.recall(label)} / {scoreCountsByLabel.f1(label)}")




