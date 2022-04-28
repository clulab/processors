from pytorch.labels import ModifierHeadPair
from pytorch.utils import *
import math

DEBUG = False
def p(s):
    if DEBUG:
        print(s, end='')
def pl(s = ""):
    if DEBUG:
        print(s)
# types of spans in the chart
HEAD_LEFT = 0
HEAD_RIGHT = 1

'''
  Stores one dependency for the Eisner algorithm 
  Indexes for head and mod start at 1 for the first word in the sentence; 0 is reserved for root
'''
class Dependency:
    def __init__(self, mod, head, score, rank, label = ""):
        self.mod = mod
        self.head = head
        self.score = score
        self.rank = rank
        self.label = label

    def __str__(self):
        return f"mod:{self.mod}, head:{self.head}, score:{self.score}, rank:{self.rank}, label:{self.label},"

class Span:
    def __init__(self, dependencies = [], head = -1, score = 0.0):
        self.dependencies = dependencies
        self.head = head
        self.score = score

    def __str__(self):
        sb = ""
        for dep in self.dependencies:
            sb += f"\t{dep}\n"
        return sb

    def contains(self, mod, head):
        for dep in self.dependencies:
            if dep.mod == mod and dep.head == head:
                return True
        return False

    def isEmpty(self):
        return len(self.dependencies) == 0

    @classmethod
    def apply(cls, left, right, dep, head):
        # product of probabilities, in log space
        if(dep is not None and dep.score != 0):
            score = left.score + right.score + float(math.log(dep.score))
        else:
            score = left.score + right.score

        # aggregate all dependencies for this span
        deps = []
        allNodes = set()
        modNodes = set()

        if(dep is not None):
            cls.addDep(dep, deps, allNodes, modNodes)
        for dep in left.dependencies:
            cls.addDep(dep, deps, allNodes, modNodes)
        for dep in right.dependencies:
            cls.addDep(dep, deps, allNodes, modNodes)

        return cls(deps, head, score)

    @staticmethod
    def addDep(dep, deps, allNodes, modNodes):
        deps.append(dep)
        allNodes.add(dep.head)
        allNodes.add(dep.mod)
        modNodes.add(dep.mod)

class Chart:
    def __init__(self, dimension):
        self.dimension = dimension
        self.chart = self.mkChart()
    
    def mkChart(self):
        c = [[None for _ in range(self.dimension)] for _ in range(self.dimension)]

        for i in range(len(c)):
            for j in range(len(c[i])):
                c[i][j] = [None, None]
            c[i][i][HEAD_LEFT] = Span()
            c[i][i][HEAD_RIGHT] = Span()

        return c

    def get(self, start, end, spanType):
        return self.chart[start][end][spanType]

    def set(self, start, end, spanType, span):
        if self.chart[start][end][spanType] is None:
            self.chart[start][end][spanType] = span
        elif self.chart[start][end][spanType].score < span.score:
            self.chart[start][end][spanType] = span
            return (2, span.score)
        else:
            return (0, span.score)

    def __str__(self):
        sb = ""
        for mod in range(0, self.dimension):
            for head in range(0, self.dimension):
                for spanType in range(0, 2):
                    spanTypeAsString = "left" if(spanType == HEAD_LEFT) else "right"
                    span = self.chart[mod][head][spanType]
                    if span:
                        sb += f"[{mod} -- {head}] (head {spanTypeAsString})\n"
                        sb += str(span)
        return sb


def parse(startingDependencies):
    length = len(startingDependencies)
    chart = Chart(length)

    for spanLen in range(2, length):
        for start in range(0, length - spanLen):
            end = start + spanLen - 1 # inclusive
            pl(f"Span: [{start}, {end}]")
            for split in range(start, end):
                ll = chart.get(start, split, HEAD_LEFT)
                rr = chart.get(split + 1, end, HEAD_RIGHT)
                if ll is not None and rr is not None:
                    d = startingDependencies[start][end]
                    if d is not None:
                        chart.set(start, end, HEAD_RIGHT, Span.apply(ll, rr, d, rr.head))
                    d = startingDependencies[start][split + 1]
                    if d is not None:
                        chart.set(start, end, HEAD_RIGHT, Span.apply(ll, rr, d, rr.head))
                    d = startingDependencies[end][start]
                    if d is not None:
                        chart.set(start, end, HEAD_LEFT, Span.apply(ll, rr, d, ll.head))
                    d = startingDependencies[end][split]
                    if d is not None:
                        chart.set(start, end, HEAD_LEFT, Span.apply(ll, rr, d, ll.head))
                lr = chart.get(start, split, HEAD_RIGHT)
                if lr is not None and rr is not None:
                    d = startingDependencies[split][split + 1]
                    if d is not None:
                        chart.set(start, end, HEAD_RIGHT, Span.apply(lr, rr, d, rr.head))
                    d = startingDependencies[split][end]
                    if d is not None:
                        chart.set(start, end, HEAD_RIGHT, Span.apply(lr, rr, d, rr.head))
                rl = chart.get(split + 1, end, HEAD_LEFT)
                if ll is not None and rl is not None:
                    d = startingDependencies[split + 1][split]
                    if d is not None:
                        chart.set(start, end, HEAD_LEFT, Span.apply(ll, rl, d, ll.head))
                    d = startingDependencies[split + 1][start]
                    if d is not None:
                        chart.set(start, end, HEAD_LEFT, Span.apply(ll, rl, d, ll.head))
                '''
                 merge [start, split] and [split, end] in both directions
                '''
                leftRightComplete = chart.get(start, split, HEAD_RIGHT)
                leftLeftIncomplete = chart.get(start, split, HEAD_LEFT)
                rightRightIncomplete = chart.get(split, end, HEAD_RIGHT)
                rightLeftComplete = chart.get(split, end, HEAD_LEFT)

                # merge [start(h), split] and [split(h), end]
                if(leftLeftIncomplete is not None and rightLeftComplete is not None):
                    chart.set(start, end, HEAD_LEFT, Span.apply(leftLeftIncomplete, rightLeftComplete, None, leftLeftIncomplete.head))
                # merge [start, split(h)] and [split, end(h)]
                if(leftRightComplete is not None and rightRightIncomplete is not None):
                    chart.set(start, end, HEAD_RIGHT, Span.apply(leftRightComplete, rightRightIncomplete, None, rightRightIncomplete.head))
    print(chart)
    print(length - 1, HEAD_LEFT)
    top = chart.get(0, length - 1, HEAD_LEFT)
    return top

def generateOutput(top, scores, dependencies, generateRelativeHeads):
    heads = [None for _ in range(len(scores))]
    if top:
        # Eisner correctly produced a full tree
        for dep in top.dependencies:
            head = -1
            if(generateRelativeHeads):
                head = 0 if (dep.head == 0) else dep.head - dep.mod
            else:
                head = dep.head - 1
            label = dep.label
            heads[dep.mod - 1] = (head, label)
    else:
        # Eisner failed to produce a complete tree; revert to the greedy inference
        for i in range(len(scores)):
            relativeHead = int(max([(l,s) for l,s in scores[i] if l!=STOP_TAG], key=lambda kv: kv[1])[0])
            depMod = i + 1
            depHead = 0 if (relativeHead == 0) else depMod + relativeHead
            print (depMod, relativeHead, depHead, len(scores))
            label = dependencies[depMod][depHead].label
            '''
             if(generateRelativeHeads): we are storing *relative* head positions here
             else: we are storing absolute heads, starting at offset 0
            '''
            head = relativeHead if(generateRelativeHeads) else depHead - 1
            heads[i] = (head, label)
    return heads

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

def printDependencyTable(deps):
    for i in range(len(deps)):
        p(f"{i}:")
        for j in range(len(deps[i])):
            dep = deps[i][j]
            p("\t")
            if(dep is not None):
                p(dep.score)
            else:
                p("-")
        pl()

def ensembleParser(mtlHeads, mtlLabels, sentence, constEmbeddings, topK, lmd, generateRelativeHeads):
    # construct the dependency table using just the head prediction scores
    scores = mtlHeads.predictWithScores(0, sentence, None, constEmbeddings)
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
        labelScores = mtlLabels.predictWithScores(0, sentence, modHeadPairs, constEmbeddings) # these are probs
        labelTopScores = [max([(l, s) for l,s in scores if l != STOP_TAG], key=lambda kv: kv[1]) for scores in labelScores]# keep just the top score for each label that is not STOP
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

















