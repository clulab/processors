class Label:
  def __init__(self, label):
        self.label = label

class PrimalLabel(Label):
    def __init__(self, label):
        self.label = label

'''
Label information for a dual task that classifies pairs of words (modifier and head)
Note: offsets for modifier and head start at 0. "root" heads have index -1
'''
class DualLabel(Label):
    def __init__(self, modifier, head, label):
        self.modifier = modifier
        self.head = head
        self.label = label
        self.modifierHeadPair = (modifier, head)

'''
Indexes for pairs of words (modifier and head)
Note: offsets for modifier and head start at 0. "root" heads have index -1
'''
class ModifierHeadPair:
    def __init__(self, modifier, head):
        self.modifier = modifier
        self.head = head