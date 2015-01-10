How to regenerate the discourse models
======================================

Note: this is of limited utility to regular users; but it might be useful for developers interested in modifying the discourse parser. 

The regeneration process works in two major steps.
First, preprocess (i.e., syntactically parse) the RST corpora:

    sbt 'run-main edu.arizona.sista.discourse.rstparser.CacheReader /data/nlp/corpora/RST_cached_preprocessing/rst_test'
    sbt 'run-main edu.arizona.sista.discourse.rstparser.CacheReader /data/nlp/corpora/RST_cached_preprocessing/rst_train'

The two commands above run full syntactic analysis on the two directories and serialize the outputs as `rst_test.rcache` and `rst_train.rcache`. Note: by default, `CacheReader` uses `CoreNLPProcessor` for this job. To generate the discourse model that uses only dependency syntax, please change the `PROCESSOR` variable in `CacheReader` to use `FastNLPProcessor` before running these tasks.

Second, after the two jobs finish, run the actual training + testing process:

    sbt 'run-main edu.arizona.sista.discourse.rstparser.RSTParser -train /data/nlp/corpora/RST_cached_preprocessing/rst_train -test /data/nlp/corpora/RST_cached_preprocessing/rst_test -model model.const.rst.gz'

This will generate the model file, `model.const.rst.gz` in the current directory.
If the cache used `FastNLPProcessor`, the above command will generate the model that uses only dependency information. I suggest naming it something different, e.g., `model.dep.rst.gz`

