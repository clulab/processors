---
title: Odin
has_children: false
nav_order: 4
---

# Odin (Open Domain INformer)

This page describes our event extraction (EE) framework, called Odin (from Open Domain INformer). 

Rule-base information extraction (IE) has long enjoyed wide adoption throughout industry, though it has remained largely ignored in academia, in favor of machine learning (ML) methods ([Chiticariu et al., 2013](http://www.aclweb.org/anthology/D13-1079)). However, rule-based systems have several advantages over pure ML systems, including: (a) the rules are interpretable and thus suitable for rapid development and/or domain transfer; and (b) humans and machines can contribute to the same model. Why then have such systems failed to hold the attention of the academic community? One argument raised by Chiticariu et al. is that, despite notable previous efforts (Appelt and Onyshkevych, 1998; Levy and Andrew, 2006; Hunter et al., 2008; Cunningham et al., 2011; Chang and Manning, 2014), there is not a standard language for this task, or a “standard way to express rules”, which raises the entry cost for new rule-based systems.

Odin aims to address these issues with a new language and framework. We follow the simplicity principles promoted by other natural language processing toolkits, such as Stanford’s CoreNLP, which aim to “avoid over-design”, “do one thing well”, and have a user “up and running in ten minutes or less” ([Manning et al., 2014](http://nlp.stanford.edu/pubs/StanfordCoreNlp2014.pdf)). In particular, our approach is:

**Simple**: Taking advantage of a syntactic dependency representation ([de Marneffe and Manning, 2008](http://nlp.stanford.edu/pubs/dependencies-coling08.pdf)), our EE language has a simple, declarative syntax for _n_-ary events, which captures event predicates (or triggers) with lexical and morphological constraints, and event arguments with (generally) simple syntactic patterns and semantic constraints.

**Powerful**: Despite its simplicity, our EE framework can capture complex constructs when necessary, such as: (a) recursive events, i.e., events that take other events as arguments; (b) complex regular expressions for the argument syntactic patterns: we have extended a standard regular expression language to allow matching patterns over directed graphs, e.g., new `<` and `>` operators capture specific traversal direction; and (c) optional arguments and multiple arguments with the same name.

**Robust**: To recover from unavoidable syntactic errors, syntactic patterns can be can be freely mixed with surface, token-based patterns, using a language inspired by the [Allen Institute of Artificial Intelligence’s Tagger](https://github.com/allenai/taggers). These patterns match against information extracted by any of our `Processors`, namely a token’s part of speech, unnormalized or lemmatized form, named entity label, and the immediate incoming and outgoing edges in the syntactic graph. 

**Fast**: Our EE runtime is fast because our rules use trigger patterns, which generally use shallow lexico-morphological constraints, as starting points. Only when event triggers are detected, the matching of the more complex syntactic patterns for arguments is attempted. This guarantees quick executions. For example, in the real-world domain discussed below, Odin processes more than 100 sentences/second on average (after the initial text processing pipeline) on a laptop with an i7 CPU and 16GB of RAM.

# The Odin Rule Language and API

Please read the [Odin Manual](http://arxiv.org/abs/1509.07513) for a thorough description of Odin's rule language and API. For a quick introduction, please see our [LREC paper that summarizes the Odin rule language](http://surdeanu.info/mihai/papers/lrec2016-odin.pdf).

We also have an [interactive Odin tutorial](https://github.com/clu-ling/odin-tutorial).

# A Toy Domain

Please see this project for an example of a toy (yet complete!) domain:
 [https://github.com/clulab/odin-examples](https://github.com/clulab/odin-examples). We recommend starting from this project when building a new Odin domain.

# A Real-world Domain

For a complex, real-world domain built using Odin, please see our [`reach` project](https://github.com/clulab/reach).

# Odin Authors

[Marco Valenzuela](https://github.com/marcovzla), [Gustave Hahn-Powell](https://github.com/myedibleenso), and [Mihai Surdeanu](http://surdeanu.info/mihai/)
