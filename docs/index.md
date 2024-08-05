---
title: Overview
has_children: false
nav_order: 1
---

# What is it?

[processors](https://github.com/clulab/processors) is the main public code repository of the [Computational Language Understanding (CLU) Lab](http://clulab.org) at [University of Arizona](http://www.arizona.edu). 

This repository contains:

+ A suite of natural language processors in the `org.clulab.processors` package. See the Processors section for details.
+ A rule-based event extraction (EE) framework called Odin (Open Domain INformer) in the `org.clulab.odin` package. See the Odin section for more details.
+ A machine learning (ML) package (`org.clulab.learning`), which includes implementations for common ML algorithms (e.g., Perceptron, Logistic Regression, Support Vector Machines, Random Forests) for both classification and ranking.

# Authors

[Mihai Surdeanu](http://surdeanu.info/mihai/), [Marco Valenzuela](https://github.com/marcovzla), [Gustave Hahn-Powell](https://github.com/myedibleenso), Peter Jansen, [Daniel Fried](http://www.cs.arizona.edu/~dfried/), Dane Bell, [Keith Alcock](http://www.keithalcock.com), and Tom Hicks.

# License

Our code is licensed under Apache License Version 2.0. 

# Citations

If you use Odin, our event extraction framework, please cite this paper:

Marco A. Valenzuela-Escarcega, Gustave Hahn-Powell, Thomas Hicks, and Mihai Surdeanu. A Domain-independent Rule-based Framework for Event Extraction. In *Proceedings of the 53rd Annual Meeting of the Association for Computational Linguistics and the 7th International Joint Conference on Natural Language Processing of the Asian Federation of Natural Language Processing: Software Demonstrations (ACL-IJCNLP)*, 2015. [[pdf]](http://surdeanu.info/mihai/papers/acl2015.pdf) [[bib]](http://surdeanu.info/mihai/papers/acl2015.bib)

If you use the syntactic parser in `BalaurProcessor` please cite the original algorithm:

Amini, Afra, Tianyu Liu, and Ryan Cotterell. Hexatagging: Projective Dependency Parsing as Tagging. In *Proceedings of the 61st Annual Meeting of the Association for Computational Linguistics Volume 2: Short Papers*, 2023. [[pdf]](https://aclanthology.org/2023.acl-short.124.pdf)

If you use anything else in this package, please link to this page.


