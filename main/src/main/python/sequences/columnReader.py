#-----------------------------------------------------------
#  Reads the CoNLL-like column format
#-----------------------------------------------------------
class ColumnReader:

  def readColumns(source):
    if type(source) is str:
      source = open(source)
    sentence = list()
    sentences = list()
    for line in source:
      l = line.strip()
      if (l is ""):
        # end of sentence
        if (sentence):
          sentences += [sentence]
          sentence = list()
      else:
        # within the same sentence
        bits = l.split("\t")
        if (len(bits) < 2):
          raise RuntimeError(f"ERROR: invalid line {l}!")
        sentence += [Row(bits)]

    if (sentence):
      sentences += [sentence]

    source.close()
    return sentences

# -----------------------------------------------------------
# Stores training data for sequence modeling
# Mandatory columns: 0 - word, 1 - label
# Optional columns: 2 - POS tag, 3+ SRL arguments
# @param tokens
# -----------------------------------------------------------

class Row:

  def __init__(self, tokens): 
    self.tokens = tokens
    self.length = len(tokens)

  def get(self, idx):
    if(idx >= self.length):
      raise RuntimeError(f"ERROR: trying to read field #{idx}, which does not exist in this row: {tokens}!")
    return self.tokens[idx]
