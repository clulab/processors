How to regenerate the discourse models
======================================

Note: this is of limited utility to regular users; but it might be useful for developers interested in modifying the discourse parser. 

To regenerate the constituent-syntax model (slower but better), run this command:

        sbt 'run-main edu.arizona.sista.discourse.rstparser.RSTParser -train /data/nlp/corpora/RST_cached_preprocessing/rst_train -test /data/nlp/corpora/RST_cached_preprocessing/rst_test -model model.const.rst.gz'

This will generate the model file, `model.const.rst.gz`, in the current directory.
To regenerate the dependency-syntax model (faster but slightly worse), run this command:

        sbt 'run-main edu.arizona.sista.discourse.rstparser.RSTParser -train /data/nlp/corpora/RST_cached_preprocessing/rst_train -test /data/nlp/corpora/RST_cached_preprocessing/rst_test -model model.dep.rst.gz -dep'

Similarly, this command generates  the model that uses only dependency information: `model.dep.rst.gz`

