from pytorch.labels import DualLabel, PrimalLabel
from pytorch.utils import *
import random

class AnnotatedSentence:

    def __init__(self, words, posTags = None, neTags = None):
        self.words = words
        self.posTags = posTags
        self.neTags = neTags
        self.size = len(words)
        self.indicies = range(self.size)

class RowReader(object):

    def __init__(self):
        raise NotImplementedError

    def toAnnotatedSentences(self, rows, insertNegatives = 0):
        raise NotImplementedError

class MetalRowReader(RowReader):

    def __init__(self):
        self.WORD_POSITION = 0
        self.POS_TAG_POSITION = 1
        self.NE_LABEL_POSITION = 2
        self.LABEL_START_OFFSET = 3

    def toAnnotatedSentences(self, rows, insertNegatives = 0):
        if (rows[0].length == 2):
            return self.parseSimple(rows)
        elif (rows[0].length == 4):
            return self.parseSimpleExtended(rows)
        elif (rows[0].length >= 5):
            return self.parseFull(rows)
        else:
            raise RuntimeError("ERROR: the Metal format expects 2, 4, or 5+ columns!")

    # Parser for the simple format: word, label 
    def parseSimple(self, rows):
        assert(rows[0].length == 2)
        words = list()
        labels = list()

        for row in rows:
            words += [row.get(self.WORD_POSITION)]
            labels += [PrimalLabel(row.get(self.WORD_POSITION + 1))]

        return [(AnnotatedSentence(words), labels)]

    # Parser for the simple extended format: word, POS tag, NE label, label
    def parseSimpleExtended(self, rows):
        assert(rows[0].length == 4)
        words = list()
        posTags = list()
        neLabels = list()
        labels = list()

        for row in rows:
            words += [row.get(self.WORD_POSITION)]
            posTags += [row.get(self.POS_TAG_POSITION)]
            neLabels += [row.get(self.NE_LABEL_POSITION)]
            labels += [PrimalLabel(row.get(self.LABEL_START_OFFSET))]

        return [(AnnotatedSentence(words, posTags, neLabels), labels)]

    # Parser for the full format: word, POS tag, NE label, (label head)+ 
    def parseFull(self, rows, insertNegatives):
        assert(rows[0].length >= 5)
        numSent = (rows[0].length - 3) / 2
        assert(numSent >= 1)
        assert(numSent==int(numSent))
        numSent = int(numSent)



        words = list()
        posTags = list()
        neLabels = list()
        headPositions = [list() for i in range(numSent)]
        labels = [list() for i in range(numSent)]

        for row in rows:
            words += [row.get(self.WORD_POSITION)]
            posTags += [row.get(self.POS_TAG_POSITION)]
            neLabels += [row.get(self.NE_LABEL_POSITION)]

            for j in range(numSent):
                labels[j] += [row.get(self.LABEL_START_OFFSET + (j * 2))]
                try:
                    headPositions[j] += [int(row.get(self.LABEL_START_OFFSET + (j * 2) + 1))]
                except ValueError:
                    headPositions[j] += [-1]

        sentences = list()
        for i in range(numSent):
            annotatedSent = AnnotatedSentence(words, posTags, neLabels)
            labelsForThisSentence = labels[i]
            headsForThisSentence = headPositions[i]
            sentLabels = list()
            for j in range(labelsForThisSentence):
                sentLabels += [DualLabel(j, headsForThisSentence[j], labelsForThisSentence[j])]
                if(insertNegatives > 0):
                    negHeads = mkRandoms(range(-1, annotatedSent.size), Set(headsForThisSentence[j]), insertNegatives)
                    for negHead in negHeads:
                        sentLabels += [DualLabel(j, negHead, STOP_TAG)]

            sentences += [(annotatedSent, sentLabels)]
        return sentences

    def mkRandoms(rg, exclude, howMany):
        numbers = random.shuffle(list(rg))
        randoms = set()
        for n in numbers:
            if len(randoms) >= howMany:
                break
            if n not in exclude:
                randoms.add(n)
        return randoms
