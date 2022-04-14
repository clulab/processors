from pytorch.labels import ModifierHeadPair
from pytorch.utils import *

'''
  Stores one dependency for the Eisner algorithm 
  Indexes for head and mod start at 1 for the first word in the sentence; 0 is reserved for root
'''
class Dependency:
    def __init__(mod, head, score, rank, label = ""):
        self.mod = mod
        self.head = head
        self.score = score
        self.rank = rank
        self.label = label

class Eisner(object):

    # Converts the top K predictions from an unlabeled parserinto a matrix of Dependency (rows are mods; columns are heads)
    def toDependencyTable(scores, topK):
        length = len(scores) + 1
        dependencies = [[None for _ in range(length)] for _ in range(length)]

        for i, scs in enumerate(scores):
            mod = i + 1 # offsets start at 1 in the Dependency class
            sortedPreds = sorted(scs, key=lambda kv: kv[1], reverse=True)
            sortedLabels, sortedProbs = zip(*sortedPreds)
            headCount = 0

            for j, st_lbl in enumerate(sortedLabels):
                if headCount >= topK:
                    break
                try:
                    relHead = int(st_lbl)
                    score = sortedProbs[j]
                    head = 0 if relHead == 0 else mod + relHead
                    if (head >= 0 and head < length):
                        dependencies[mod][head] = Dependency(mod, head, score, j)
                        headCount += 1 
                except ValueError: # zheng: I am guessing the "NumberFormatException" from scala occurs when "sortedLabels(j).toInt" does not work
                    continue # Ok to skip these, since the parser may predict <START> and <STOP> labels
        return dependencies

    def ensembleParser(mtlHeads, mtlLabels, sentence, constEmbeddings, topK, lmd, generateRelativeHeads):
        # construct the dependency table using just the head prediction scores
        scores = mtlHeads.predictWithScores(0, sentence, constEmbeddings)
        startingDependencies = toDependencyTable(scores, topK) # currently the score of a dependency is just the head score

        # add label scores to all dependencies
        if mtlLabels:
            # prepare the (modifier, head) pairs for which we will get label scores
            modHeadPairs = []
            for i in range(len(startingDependencies)):
                for j in range(len(startingDependencies[i])):
                    dep = startingDependencies[i][j]
                    if dep is not None:
                        modHeadPairs.append(ModifierHeadPair(dep.mod - 1, dep.head - 1))
            # generate label probabilities using the label classifier
            # zheng: We have serious issue for the modHeadPairs here, it is not in the python implementation.
            labelScores = mtlLabels.predictWithScores(0, sentence, Some(modHeadPairs), constEmbeddings) # these are probs
            labelTopScores = [max((l, s) for l, s in enumerate(labelScores) if l != STOP_TAG, key=lambda kv: kv[1])]  # keep just the top score for each label that is not STOP
            assert len(labelTopScores) == len(modHeadPairs)

            for i, topLabelAndScore in enumerate(labelTopScores):
                modHeadPair = modHeadPairs[i]
                mod = modHeadPair.modifier
                head = modHeadPair.head

                startingDependencies[mod + 1][head + 1].score = lmd * startingDependencies[mod + 1][head + 1].score + (1 - lmd) * topLabelAndScore[1]
                startingDependencies[mod + 1][head + 1].label = topLabelAndScore[0]

        # the actual Eisner parsing algorithm
        top = parse(startingDependencies)

        # convert back to relative (or absolute) heads
        return generateOutput(top, scores, startingDependencies, generateRelativeHeads)

















