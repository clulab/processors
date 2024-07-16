from dataclasses import dataclass
from collections import defaultdict

OUTSIDE_LABEL = "O"

@dataclass
class ScoreCounts:
    correct: int = 0
    gold: int = 0
    predicted: int = 0

class SeqScorer:

    @staticmethod
    def f1(golds, preds):
        scoreCountsByLabel = ScoreCountsByLabel()

        for e1, e2 in zip(preds, golds):
            scoreCountsByLabel.total += 1
            if e1 == e2:
                scoreCountsByLabel.correct += 1
            if e2 != OUTSIDE_LABEL:
                scoreCountsByLabel.incGold()
                scoreCountsByLabel.incGold(e2)
            if e1 != OUTSIDE_LABEL:
                scoreCountsByLabel.incPredicted()
                scoreCountsByLabel.incPredicted(e1)
                if e1 == e2:
                    scoreCountsByLabel.incCorrect()
                    scoreCountsByLabel.incCorrect(e1)
        return scoreCountsByLabel

class ScoreCountsByLabel:

    def __init__(self):
        self.map = defaultdict(ScoreCounts)
        self.total = 0
        self.correct = 0

    def labels(self):
        return self.map.keys()

    def incGold(self, label="*", value=1):
        counts = self.map[label]
        counts.gold += value

    def incPredicted(self, label="*", value=1):
        counts = self.map[label]
        counts.predicted += value

    def incCorrect(self, label="*", value=1):
        counts = self.map[label]
        counts.correct += value

    def incAll(self, counts):
        self.correct += counts.correct
        self.total += counts.total

        for label in counts.labels():
            c = counts.map[label]
            self.incGold(label, c.gold)
            self.incPredicted(label, c.predicted)
            self.incCorrect(label, c.correct)

    def precision(self, label="*", decimals=2):
        c = self.map[label].correct
        p = self.map[label].predicted

        prec = c/p if p!=0 else 0

        return round(prec*100, decimals) if decimals>0 else prec

    def recall(self, label="*", decimals=2):
        c = self.map[label].correct
        g = self.map[label].gold

        reca = c/g if g!=0 else 0

        return round(reca*100, decimals) if decimals>0 else reca

    def f1(self, label="*", decimals=2):
        p = self.precision(label, decimals=-1)
        r = self.recall(label, decimals=-1)

        f1 = 2.0 * p * r / (p + r) if (p!=0 and r!=0) else 0

        return round(f1*100, decimals) if decimals>0 else f1

    def accuracy(self, decimals=2):
        a = self.correct / self.total

        return round(a*100, decimals) if decimals>0 else a






