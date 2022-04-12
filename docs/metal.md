---
title: Metal
has_children: true
nav_order: 5
---

# Metal, a Multi-task Learning Framework for Sequence Modeling

`Metal` is a multi-task learning framework inspired by [Collobert and Weston's famous paper](https://dl.acm.org/doi/pdf/10.1145/1390156.1390177), but adapted for sequence modeling. It comes with its own simple domain-specific language (DSL) that allows you to ramp up sequence models for various NLP tasks (e.g., part-of-speech tagging, named entity recognition, semantic role labeling), without writing any actual code. `Metal` supports the expected deep learning components for sequence modeling, e.g., biLSTMs, CRF layers, character-level encoders, etc. 

`Metal` is implemented on top of [`DyNet`](https://dynet.readthedocs.io/), using our own Scala packaging in the [`fatdynet`](https://github.com/clulab/fatdynet) project. 

