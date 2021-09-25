import numpy as np

class WordEmbeddingMap:
    def __init__(self, config):
        self.emb_dict = self.load(config)
        self.dim = self.emb_dict.shape[-1]

    def load(self):
        emb_matrix = None
        emb_dict = dict()
        for line in open(config.get_string("glove.matrixResourceName")):
            if not len(line.split()) == 2:
                if "\t" in line:
                    delimiter = "\t"
                else:
                    delimiter = " "
                line_split = line.rstrip().split(delimiter)
                # extract word and vector
                word = line_split[0]
                x = np.array([float(i) for i in line_split[1:]])
                vector = (x /np.linalg.norm(x))
                embedding_size = vector.shape[0]
                emb_dict[word] = vector
        base = math.sqrt(6/embedding_size)
        emb_dict["<UNK>"] = np.random.uniform(-base,base,(embedding_size))
        return emb_dict

    def isOutOfVocabulary(self, word):
        return word not in self.emb_dict