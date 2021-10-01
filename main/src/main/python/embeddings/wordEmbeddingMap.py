import numpy as np
import math

class WordEmbeddingMap:
    def __init__(self, config):
        self.emb_dict, self.dim = load(config)

    def isOutOfVocabulary(self, word):
        return word not in self.emb_dict

def load(config):
    emb_dict = dict()
    for line in open(config.get_string("glove.matrixResourceName")):
        if not len(line.split()) == 2:
            if "\t" in line:
                delimiter = "\t"
            else:
                delimiter = " "
            word, *rest = line.rstrip().split(delimiter)
            x = np.array(list(map(float, rest)))
            vector = (x /np.linalg.norm(x))
            embedding_size = vector.shape[0]
            emb_dict[word] = vector
    base = math.sqrt(6/embedding_size)
    emb_dict["<UNK>"] = np.random.uniform(-base,base,(embedding_size))
    return emb_dict, embedding_size