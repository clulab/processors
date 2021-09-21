
class AnnotatedSentence:

    def __init__(self, words, posTags = None, neTags = None, headPositions = None):
        self.words = words
        self.posTags = posTags
        self.neTags = neTags
        self.headPositions = headPositions
        self.size = len(words)
        self.indicies = range(self.size)

class RowReader(object):

    def __init__(self):
        raise NotImplementedError

    def toAnnotatedSentences(self, rows):
        raise NotImplementedError

class MetalRowReader(RowReader):

    def __init__(self):
        self.WORD_POSITION = 0
        self.POS_TAG_POSITION = 1
        self.NE_LABEL_POSITION = 2
        self.LABEL_START_OFFSET = 3

    def toAnnotatedSentences(self, rows):
        if (len(rows.head) == 2):
            self.parseSimple(rows)
        elif (len(rows.head) == 4):
            self.parseSimpleExtended(rows)
        elif (len(rows.head) >= 5):
            self.parseFull(rows)
        else:
            raise RuntimeError("ERROR: the Metal format expects 2, 4, or 5+ columns!")

    # Parser for the simple format: word, label 
    def parseSimple(rows):
        assert(len(rows.head) == 2)
        words = list()
        labels = list()

        for row in rows:
            words += [row.get(self.WORD_POSITION)]
            labels += [row.get(self.WORD_POSITION + 1)]

        return AnnotatedSentence(words), labels

    # Parser for the simple extended format: word, POS tag, NE label, label
    def parseSimpleExtended(rows):
        assert(len(rows.head) == 4)
        words = list()
        posTags = list()
        neLabels = list()
        labels = list()

        for row in rows:
            words += [row.get(self.WORD_POSITION)]
            posTags += [row.get(self.POS_TAG_POSITION)]
            neLabels += [row.get(self.NE_LABEL_POSITION)]
            labels += [row.get(self.LABEL_START_OFFSET)]

        return AnnotatedSentence(words), posTags, neLabels, labels

    # Parser for the full format: word, POS tag, NE label, (label head)+ 
    def parseFull(rows):
        assert(len(rows.head) >= 5)
        numSent = (len(rows.head) - 3) / 2
        assert(numSent >= 1)

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
                labels[j]+= [row.get(self.LABEL_START_OFFSET + (j * 2))]
                try:
                    headPositions[j] += [int(row.get(self.LABEL_START_OFFSET + (j * 2) + 1))]
                except:
                    raise RuntimeError # not sure about this part

        sentences = list()
        for i in range(numSent):
            annotatedSent = AnnotatedSentence(words, posTags, neLabels, headPositions[i])
            sentLabels = labels[i]
            sentences += [(annotatedSent, sentLabels)]

        return sentences
