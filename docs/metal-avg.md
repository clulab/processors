---
title: Temporal Averaging
parent: Metal
has_children: false
nav_order: 2
---

# Temporal Averaging

`Metal` includes functionality to average the parameters of multiple epochs. This is similar to [temporal ensembling](https://arxiv.org/pdf/1610.02242.pdf) but not exactly the same: temporal ensembles are proper ensembles that use multiple base models. Our temporal averaging approach averages the parameters of multiple epochs to produce a _single_ model, which has the same runtime overhead as any individual model. We observed empirically that this trick yields an improvement in the performance of 1 - 2 F1 points on all the tasks we experimented with. To generate such an average, use the `ModelAveraging` class to average the epochs with the highest performance in dev. For example, if you want to average epochs 2, 3, and 5 for the above NER classifier, run this command:

```
sbt 'runMain org.clulab.dynet.ModelAveraging mtl-en-ner-epoch2 mtl-en-ner-epoch3 mtl-en-ner-epoch5 mtl-en-ner-avg-e2e3e5'
```

The output model (`mtl-en-ner-avg-e2e3e5`) can then be used as a regular model.


