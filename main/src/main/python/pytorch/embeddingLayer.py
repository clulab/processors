from pytorch.initialLayer import InitialLayer
import random
from pytorch.utils import *
import torch.nn as nn
import torch
from pytorch.constEmbeddingsGlove import ConstEmbeddingsGlove

DEFAULT_DROPOUT_PROB: float = DEFAULT_DROPOUT_PROBABILITY
DEFAULT_LEARNED_WORD_EMBEDDING_SIZE: int = 128
DEFAULT_CHAR_EMBEDDING_SIZE: int = 32
DEFAULT_CHAR_RNN_STATE_SIZE: int = 16
DEFAULT_POS_TAG_EMBEDDING_SIZE: int = -1 # no POS tag embeddings by default
DEFAULT_NE_TAG_EMBEDDING_SIZE: int = -1 # no NE tag embeddings by default
DEFAULT_DISTANCE_EMBEDDING_SIZE: int = -1 # no distance embeddings by default
DEFAULT_POSITION_EMBEDDING_SIZE: int = -1 # no position embeddings by default
DEFAULT_DISTANCE_WINDOW_SIZE: int = -1
DEFAULT_USE_IS_PREDICATE: int = -1
random.seed(RANDOM_SEED)

class EmbeddingLayer(InitialLayer):
    def __init__(self, w2i, # word to index
                 w2f, # word to frequency
                 c2i, # character to index
                 tag2i, # POS tag to index
                 ne2i, # NE tag to index
                 learnedWordEmbeddingSize, # size of the learned word embedding
                 charEmbeddingSize, # size of the character embedding
                 charRnnStateSize, # size of each one of the char-level RNNs
                 posTagEmbeddingSize, # size of the POS tag embedding
                 neTagEmbeddingSize, # size of the NE tag embedding
                 distanceEmbeddingSize,
                 distanceWindowSize, # window considered for distance values (relative to predicate)
                 positionEmbeddingSize,
                 useIsPredicate, # if true, add a Boolean bit to indicate if current word is the predicate
                 wordLookupParameters,
                 charLookupParameters,
                 charRnnBuilder, # RNNs for the character representation
                 posTagLookupParameters,
                 neTagLookupParameters,
                 distanceLookupParameters,
                 positionLookupParameters,
                 dropoutProb):
        super().__init__()
        self.w2i = w2i
        self.w2f = w2f
        self.c2i = c2i
        self.tag2i = tag2i
        self.ne2i = ne2i
        self.learnedWordEmbeddingSize = learnedWordEmbeddingSize
        self.charEmbeddingSize = charEmbeddingSize
        self.charRnnStateSize = charRnnStateSize
        self.posTagEmbeddingSize = posTagEmbeddingSize
        self.neTagEmbeddingSize = neTagEmbeddingSize
        self.distanceEmbeddingSize = distanceEmbeddingSize
        self.distanceWindowSize = distanceWindowSize
        self.positionEmbeddingSize = positionEmbeddingSize
        self.useIsPredicate = useIsPredicate
        self.wordLookupParameters = wordLookupParameters
        self.charLookupParameters = charLookupParameters
        self.charRnnBuilder = mkBuilder(*charRnnBuilder)
        self.posTagLookupParameters = posTagLookupParameters
        self.neTagLookupParameters = neTagLookupParameters
        self.distanceLookupParameters = distanceLookupParameters
        self.positionLookupParameters = positionLookupParameters
        self.dropoutProb = dropoutProb

        posTagDim = posTagEmbeddingSize if posTagLookupParameters else 0
        neTagDim = neTagEmbeddingSize if neTagLookupParameters else 0
        distanceDim = distanceEmbeddingSize if distanceLookupParameters else 0
        predicateDim = 1 if distanceLookupParameters and useIsPredicate else 0
        positionDim = positionEmbeddingSize if positionLookupParameters else 0
        self.outDim = ConstEmbeddingsGlove.dim + learnedWordEmbeddingSize + charRnnStateSize * 2 + posTagDim + neTagDim + distanceDim + positionDim + predicateDim
        
    
    def forward(self, sentence, constEmbeddings, doDropout):

        words = sentence.words
        tags = sentence.posTags
        nes = sentence.neTags
        headPositions = sentence.headPositions

        # const word embeddings such as GloVe
        constEmbeddingsExpressions = self.mkConstEmbeddings(words, constEmbeddings)
        assert(constEmbeddingsExpressions.size(0) == len(words))
        if(tags): assert(len(tags) == len(words))
        if(nes): assert(len(nes) == len(words))
        if(headPositions): assert(len(headPositions) == len(words))

        # build the word embeddings one by one
        embeddings = self.mkEmbeddings(words, constEmbeddingsExpressions, doDropout, tags, nes, headPositions)

        return embeddings

    def mkConstEmbeddings(self, words, constEmbeddings):
        idxs = torch.LongTensor([constEmbeddings.w2i[word] if word in constEmbeddings.w2i else 0 for word in words])
        embeddings = constEmbeddings.emb(idxs)
        return embeddings

    def mkEmbeddings(self, words, constEmbeddings, doDropout, tags=None, nes=None, headPositions=None):
        #
        # Learned word embeddings
        # These are initialized randomly, and updated during backprop
        #
        ids = []
        wordPositions = []
        for i, word in enumerate(words):
            wordPositions.append(i)
            id = self.w2i.get(word, 0) # 0 reserved for UNK in the vocab
            # sample uniformly with prob 0.5 from singletons; move all other singletons to UNK
            if(doDropout and id > 0 and self.w2f[word] == 1 and random.random() < 0.5): id = 0
            ids.append(id) 
        learnedWordEmbeddings = self.wordLookupParameters(torch.LongTensor(ids))
        #
        # biLSTM over character embeddings
        #
        charEmbedding = torch.stack([mkCharacterEmbedding(word, self.c2i, self.charLookupParameters, self.charRnnBuilder) for word in words])
        #
        # POS tag embedding
        #
        if tags and self.posTagLookupParameters:
            posTagEmbed = self.posTagLookupParameters(torch.LongTensor([self.tag2i.get(tag, 0) for tag in tags]))
        else:
            posTagEmbed = None
        #
        # NE tag embedding
        #
        if nes and self.neTagLookupParameters:
            neTagEmbed = self.neTagLookupParameters(torch.LongTensor([self.ne2i.get(ne, 0) for ne in nes]))
        else:
            neTagEmbed = None
        #
        # 1 if this word is the predicate
        #
        if headPositions and self.useIsPredicate:
            predEmbed = torch.FloatTensor([1 if i==predicatePosition else 0 for i, predicatePosition in enumerate(headPositions)]).unsqueeze(1)
        else:
            predEmbed = None

        #
        # Distance embedding, relative to the distance to the predicate
        # We cut the distance down to values inside the window [-distanceWindowSize, +distanceWindowSize]
        #
        if headPositions and self.distanceLookupParameters:
            dists = [max(i-predicatePosition+self.distanceWindowSize+1, 0) if i-predicatePosition <= self.distanceWindowSize else 2 * self.distanceWindowSize + 2 for i, predicatePosition in enumerate(headPositions)]
            distanceEmbedding = self.distanceLookupParameters(torch.LongTensor(dists))
        else:
            distanceEmbedding = None

        #
        # Embedding that captures the absolute position of the token in the sentence
        #
        if self.positionLookupParameters:
            values = [i if i<100 else 100 for i, word in enumerate(words)]
            positionEmbedding = self.positionLookupParameters(torch.LongTensor(values))
        else:
            positionEmbedding = None

        # The final word embedding is a concatenation of all these
        embedParts = [constEmbeddings, learnedWordEmbeddings, charEmbedding, posTagEmbed, neTagEmbed, distanceEmbedding, positionEmbedding, predEmbed]
        embedParts = [ep for ep in embedParts if ep is not None]
        embed = torch.cat(embedParts, dim=1)
        return embed

    def saveX2i(self):
        x2i = dict()
        x2i['w2i'] = self.w2i
        x2i['w2f'] = self.w2f
        x2i['c2i'] = self.c2i
        if self.tag2i:
            x2i['hasTag2i'] = 1
            x2i['tag2i'] = self.tag2i
        else:
            x2i['hasTag2i'] = 0
        if self.ne2i:
            x2i['hasNe2i'] = 1
            x2i['ne2i'] = self.ne2i
        else:
            x2i['hasNe2i'] = 0
        x2i['learnedWordEmbeddingSize'] = self.learnedWordEmbeddingSize
        x2i['charEmbeddingSize']        = self.charEmbeddingSize
        x2i['charRnnStateSize']         = self.charRnnStateSize
        x2i['posTagEmbeddingSize']      = self.posTagEmbeddingSize
        x2i['neTagEmbeddingSize']       = self.neTagEmbeddingSize
        x2i['distanceEmbeddingSize']    = self.distanceEmbeddingSize
        x2i['distanceWindowSize']       = self.distanceWindowSize
        x2i['useIsPredicate']           = 1 if self.useIsPredicate else 0
        x2i['positionEmbeddingSize']    = self.positionEmbeddingSize
        x2i['dropoutProb']              = self.dropoutProb

        return x2i

    def __str__(self):
        return f"EmbeddingLayer({self.outDim})"

    @classmethod
    def load(cls, x2i):
        w2i = x2i['w2i']
        w2f = x2i['w2f']
        c2i = x2i['c2i']
        tag2i = x2i['tag2i'] if x2i['hasTag2i'] == 1 else None
        ne2i = x2i['ne2i'] if x2i['hasNe2i'] == 1 else None

        learnedWordEmbeddingSize = x2i.get('learnedWordEmbeddingSize', DEFAULT_LEARNED_WORD_EMBEDDING_SIZE)
        charEmbeddingSize        = x2i.get('charEmbeddingSize', DEFAULT_CHAR_EMBEDDING_SIZE)
        charRnnStateSize         = x2i.get('charRnnStateSize', DEFAULT_CHAR_RNN_STATE_SIZE)
        posTagEmbeddingSize      = x2i.get('posTagEmbeddingSize', DEFAULT_POS_TAG_EMBEDDING_SIZE)
        neTagEmbeddingSize       = x2i.get('neTagEmbeddingSize', DEFAULT_NE_TAG_EMBEDDING_SIZE)
        distanceEmbeddingSize    = x2i.get('distanceEmbeddingSize', DEFAULT_DISTANCE_EMBEDDING_SIZE)
        distanceWindowSize       = x2i.get('distanceWindowSize', DEFAULT_DISTANCE_WINDOW_SIZE)
        useIsPredicate           = x2i.get('useIsPredicate', DEFAULT_USE_IS_PREDICATE) == 1
        positionEmbeddingSize    = x2i.get('positionEmbeddingSize', DEFAULT_POSITION_EMBEDDING_SIZE)
        dropoutProb              = x2i.get('dropoutProb', DEFAULT_DROPOUT_PROB)

        #  make the loadable parameters
        wordLookupParameters = nn.Embedding(len(w2i), learnedWordEmbeddingSize)
        charLookupParameters = nn.Embedding(len(c2i), charEmbeddingSize)
        
        charRnnBuilder = (charEmbeddingSize, charRnnStateSize, 1, True, dropoutProb)

        posTagLookupParameters   = nn.Embedding(len(tag2i), posTagEmbeddingSize) if x2i['hasTag2i'] == 1 else None
        neTagLookupParameters    = nn.Embedding(len(ne2i), neTagEmbeddingSize) if x2i['hasNe2i'] == 1 else None
        distanceLookupParameters = nn.Embedding(distanceWindowSize * 2 + 3, distanceEmbeddingSize) if distanceEmbeddingSize > 0 else None
        positionLookupParameters = nn.Embedding(101, positionEmbeddingSize) if positionEmbeddingSize > 0 else None

        return cls(w2i, w2f, c2i, tag2i, ne2i,
                  learnedWordEmbeddingSize,
                  charEmbeddingSize,
                  charRnnStateSize,
                  posTagEmbeddingSize,
                  neTagEmbeddingSize,
                  distanceEmbeddingSize,
                  distanceWindowSize,
                  positionEmbeddingSize,
                  useIsPredicate,
                  wordLookupParameters,
                  charLookupParameters,
                  charRnnBuilder,
                  posTagLookupParameters,
                  neTagLookupParameters,
                  distanceLookupParameters,
                  positionLookupParameters,
                  dropoutProb)

    @classmethod
    def initialize(cls, config, paramPrefix, wordCounter):

        if(not config.contains(paramPrefix)):
            return None

        learnedWordEmbeddingSize = config.get_int(paramPrefix + ".learnedWordEmbeddingSize",DEFAULT_LEARNED_WORD_EMBEDDING_SIZE)
        charEmbeddingSize        = config.get_int(paramPrefix + ".charEmbeddingSize",DEFAULT_CHAR_EMBEDDING_SIZE)
        charRnnStateSize         = config.get_int(paramPrefix + ".charRnnStateSize",DEFAULT_CHAR_RNN_STATE_SIZE)
        posTagEmbeddingSize      = config.get_int(paramPrefix + ".posTagEmbeddingSize",DEFAULT_POS_TAG_EMBEDDING_SIZE)
        neTagEmbeddingSize       = config.get_int(paramPrefix + ".neTagEmbeddingSize",DEFAULT_NE_TAG_EMBEDDING_SIZE)
        distanceEmbeddingSize    = config.get_int(paramPrefix + ".distanceEmbeddingSize",DEFAULT_DISTANCE_EMBEDDING_SIZE)
        distanceWindowSize       = config.get_int(paramPrefix + ".distanceWindowSize",DEFAULT_DISTANCE_WINDOW_SIZE)
        useIsPredicate           = config.get_bool(paramPrefix + ".useIsPredicate",DEFAULT_USE_IS_PREDICATE == 1)
        positionEmbeddingSize    = config.get_int(paramPrefix + ".positionEmbeddingSize",DEFAULT_POSITION_EMBEDDING_SIZE)
        dropoutProb              = config.get_float(paramPrefix + ".dropoutProb",DEFAULT_DROPOUT_PROB)

        wordList = [UNK_WORD] + sorted(wordCounter.keys())
        w2i = {w:i for i, w in enumerate(wordList)}

        wordLookupParameters = nn.Embedding(len(w2i), learnedWordEmbeddingSize)
        nn.init.xavier_uniform_(wordLookupParameters.weight)

        c2iFilename = config.get_string(paramPrefix + ".c2i", "org/clulab/c2i-en.txt")
        c2i = readChar2Ids(c2iFilename)

        charLookupParameters = nn.Embedding(len(c2i), charEmbeddingSize)
        nn.init.xavier_uniform_(charLookupParameters.weight)
        charRnnBuilder = (charEmbeddingSize, charRnnStateSize, 1, True, dropoutProb)

        if(posTagEmbeddingSize > 0):
            tag2i = readString2Ids(config.get_string(paramPrefix + ".tag2i", "../resources/org/clulab/tag2i-en.txt"))
            posTagLookupParameters = nn.Embedding(len(tag2i), posTagEmbeddingSize)
            nn.init.xavier_uniform_(posTagLookupParameters.weight)
        else:
            tag2i = None
            posTagLookupParameters = None

        if(neTagEmbeddingSize > 0):
            ne2i = readString2Ids(config.get_string(paramPrefix + ".ne2i", "../resources/org/clulab/ne2i-en.txt"))
            neTagLookupParameters = nn.Embedding(len(ne2i), neTagEmbeddingSize)
        else:
            ne2i = None
            neTagLookupParameters = None

        if distanceEmbeddingSize > 0:
          distanceLookupParameters = nn.Embedding(distanceWindowSize * 2 + 3, distanceEmbeddingSize)
          nn.init.xavier_uniform_(distanceLookupParameters.weight)
        else:
          distanceLookupParameters = None

        if positionEmbeddingSize > 0:
          positionLookupParameters = nn.Embedding(101, positionEmbeddingSize)
          nn.init.xavier_uniform_(positionLookupParameters.weight)
        else:
          positionLookupParameters = None

        return cls(w2i, wordCounter, c2i, tag2i, ne2i,
                  learnedWordEmbeddingSize,
                  charEmbeddingSize,
                  charRnnStateSize,
                  posTagEmbeddingSize,
                  neTagEmbeddingSize,
                  distanceEmbeddingSize,
                  distanceWindowSize,
                  positionEmbeddingSize,
                  useIsPredicate,
                  wordLookupParameters,
                  charLookupParameters,
                  charRnnBuilder,
                  posTagLookupParameters,
                  neTagLookupParameters,
                  distanceLookupParameters,
                  positionLookupParameters,
                  dropoutProb)

def mkBuilder(inputSize, rnnStateSize, numLayers, bi, dropoutProb):
    return nn.LSTM(inputSize, rnnStateSize, numLayers, bidirectional=bi, dropout=dropoutProb)
    



















