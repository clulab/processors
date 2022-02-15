#
+ **8.4.8** - We now extract and normalize approximate dates
+ **8.4.8** - We now extract percentages as part of numeric entities 
+ **8.4.8** - Improved tokenization and recognition of dates
+ **8.4.7** - Improved recognition of relative dates
+ **8.4.6** - Made the parsing of numeric entities more robust to broken texts (from PDF conversion)
+ **8.4.6** - Bug fix in the generation of enhanced semantic roles, again on broken texts
+ **8.4.5** - Odin can now operate over hybrid dependencies that combine semantic roles and syntax.
+ **8.4.5** - Added support for custom NERs to CluProcessor.
+ **8.4.4** - Added support for number, date, and measurement recognition to CluProcessor.
+ **8.4.4** - Removed CluCoreProcessor, as CluProcessor now natively supports numeric entities.
+ **8.4.3** - Move calculation of roots to DirectedGraph
+ **8.4.3** - Fixed POS tagging errors such as "due" in "due to" not being a IN. 
+ **8.4.2** - Improve markdown code, fix serialization stability issue 
+ **8.4.1** - Fix markdown syntax and duplicate labels, recover mention text from raw strings
+ **8.4.0** - Odin RuleReader can now generate markdown summaries of the grammars, grouped by rule or by extraction type.
+ **8.4.0** - Removed the discourse parser. Too much fat, and it is rarely used.
+ **8.4.0** - Faster CluProcessor: the syntactic parser now uses a multi-task learning model.
+ **8.3.6** - Moved the odin subproject in main. This way, we can add processors in main that rely on odin.
+ **8.3.5** - In CluProcessor manually close out the lookupParameters and collection to recover C++ memory sooner
+ **8.3.5** - Supply glove in a serialized format, kryo, for faster loading
+ **8.3.4** - FastNLPProcessorWithSemanticRoles now has enhanced SRL dependencies.
+ **8.3.3** - Handle stopwords in processors, port TagSet and FileUtils from eidos
+ **8.3.3** - Allow KBs to be provided by more generic Standard- and OverrideKbSources.
+ **8.3.3** - Use per sentence lookup parameters for embeddings
+ **8.3.3** - Better handling of enhanced dependencies, mimicking CoreNLP better.
+ **8.3.2** - Fixed a crash in ConstEmbeddingsGlove. The current implementation seems to not crash, but it is not efficient.
+ **8.3.1** - Fix a serious error in the LexiconNER which causes labels to be scrambled.  Please don't use 8.3.0.
+ **8.3.1** - Revert the precedence rules for ties encountered in override KBs to match past behavior.
+ **8.3.0** - Use a WordEmbeddingMapPool to enable sharing of glove files.  Get yours there.
+ **8.3.0** - Support headerless glove files straight from Stanford (at a small price for buffering), with or without an out-of-vocabularly vector.
+ **8.3.0** - Rename WordEmbeddingMap to SanitizedWordEmbeddingMap and deprecate it.
+ **8.3.0** - Replace it with a trait and two implementations (Explicit or Compact) with pipeline to updated fatdynet.
+ **8.3.0** - Remove word sanitization responsibility from trait, but provide DefaultWordSanitizer.
+ **8.3.0** - Supply InputStreamer to aid in reading materials from resources and files, possibly after serialization.
+ **8.3.0** - Modernize build with separate sbt files, improved scoping, updated plugins, and most recent versions of Scala 11 and 12.
+ **8.3.0** - Update Jenkins file for memory, Scala version, cross testing.
+ **8.3.0** - CluCoreProcessor now supports a custom LexiconNER constructed on the fly 
+ **8.3.0** - Allow LexiconNER collection to use KBs with differing case sensitivity
+ **8.2.6** - Bug fix: handle cycles in dependency trees produced by CoreNLPProcessor.
+ **8.2.5** - Added CluCoreProcessor, which uses all our tools + Stanford's numeric entity recognizer. 
+ **8.2.5** - Update to FatDynet 0.3.0 in serial mode. This fixes memory leaks and crashed in the DyNet Scala wrapper. 
+ **8.2.4** - Bug fix: the statement below holds even when the dependency graph is deserialized via the DocumentSerializer.
+ **8.2.4** - Bug fix: we now guarantee that the Stanford dependency graph produced by CoreNLP has the same number of nodes as the words in the sentence.
+ **8.2.3** - Do not allow loops in enhanced semantic roles.
+ **8.2.2** - Bug fix: we now guarantee that the SRL graph has the same number of nodes as the words in the sentence.
+ **8.2.1** - Bug fix in LexiconNER: we were ignoring case information before.
+ **8.2.1** - Improvements to the generation of enhanced semantic roles.
+ **8.2.0** - Added simple lexicon-based sentiment analyzer, using Bing Liu's lexicon.
+ **8.2.0** - Added support for enhanced semantic roles to CluProcessor.
+ **8.2.0** - Added support for basic and enhanced universal dependencies in CluProcessor.
+ **8.2.0** - Metal now uses a single file format across all tasks. 
+ **8.2.0** - Bug fix in Metal dropout during training. 
+ **8.1.3** - Added SRL functionality to FastNLPProcessorWithSemanticRoles.
+ **8.1.2** - Better SRL model: about twice as fast with no drop in F1 score.
+ **8.1.1** - Bug fix in Metal that makes it thread safe.
+ **8.1.0** - CluProcessor now uses the new Metal framework on top of DyNet, and includes components for POS tagging, NER, syntactic chunking, and semantic role labeling.
+ **8.0.3** - Allow vars to be formatted as a yaml list in Odin.
+ **8.0.3** - Tokenization bug fixes.
+ **8.0.3** - Better management of artifactory repos.
+ **8.0.3** - Improved emoticon grammars with help from Sandeep Suntwal.
+ **8.0.2** - Add functionality for weighted composite embedding vectors.
+ **8.0.2** - Using processors-models 0.0.5, which has better handling of unknown words in LstmCrfMtl models.
+ **8.0.2** - Have lexer keep UTF-16 surrogate pairs together.
+ **8.0.1** - No change
+ **8.0.0** - BioNLPProcessor moved to the Reach/processors subproject.
+ **8.0.0** - Added RnnLM, a language model that combines the one of Lample et al. (2016) with some of the pre-training components of ULMFit (Howard and Ruder, 2018). This is now used in LstmCrfMtl.
+ **8.0.0** - Added Document.setDCT to provide document creation times to Stanford's SUTime.
+ **8.0.0** - Made sure we can cross-compile with scala 2.11.
+ **8.0.0** - Improved tokenization of whitespaces and control characters.
+ **8.0.0** - Updated liblinear to 2.30, libsvm to 3.23, and fatdynet to 0.2.4.
+ **8.0.0** - Updated CoreNLP to v3.9.2.
+ **8.0.0** - Better tokenization of emoticons. Better tokenization of (missing) white spaces around periods.
+ **8.0.0** - Bug fix: sometimes CoreNLP's enhanced syntactic dependencies contain duplicated ones.
+ **8.0.0** - Complete redesign of CluProcessor. POS tagging, NER, and syntactic chunking are now implemented using a multi-task framework implemented using the Scala wrapper in DyNet. Dependency syntax and support for multiple languages coming soon. All CoreNLP processors (CoreNLP, FastNLP, BioNLP) are fully backwards compatible with v7. 
+ **8.0.0** - Include small refactorings that allow clients (e.g., Eidos) to more easily override tokenization. Streamline TokenizerStepContractions.
+ **8.0.0** - Added Porter stemmer.  
+ **7.5.3** - Add DocumentAttachment to Document
+ **7.5.3** - Fixed CoreNLP parser crash on weird sentences with non-alpha characters only (Reach issue #634).
+ **7.5.3** - Added support for lemmatization and improved Chunking, Parsing and POS models for Portuguese.
+ **7.5.3** - Switched to Jenkins for unit testing.
+ **7.5.3** - Added a LSTM-CRF sequence tagger.
+ **7.5.2** - Assemble with LF line endings rather than CRLF to match resource files in duplicated jars, especially with reach.  Also enable finding triggers through named mentions.
+ **7.5.1** - Fixed ambivalence hash
+ **7.5.0** - Fixed Odin bug related to lazy ranged quantifiers
+ **7.5.0** - Cache sentence hashes "ambivalently", support OpenIE annotator and serialization of OpenIE relations, buffer file access, and improve performance of LexiconNER
+ **7.4.4** - Improvements to English tokenization rules pertaining to scientific text from plant science.
+ **7.4.3** - Better handling of parentheses throughout all processors. We keep as they are, e.g., "(", and we convert them to the normalized form, e.g., "-LRB-", just for the constituent parser.
+ **7.4.3** - Added PT lemmatizer.
+ **7.4.3** - We now penalize the creation of disconnected trees in the ensemble model for dependency parsing. 
+ **7.4.2** - Odin now accepts dependency labels with ":" in the name (for UD).
+ **7.4.2** - Added chunker model and improved tokenization for PortugueseCluProcessor.
+ **7.4.2** - Reorganized Serializer to load/save to file, input/output stream, or byte array. Also takes an optional ClassLoader to use for deserialization.
+ **7.4.2** - Added SpanishCluProcessor and PortugueseCluProcessor, which currently include tokenization, POS tagging, and parsing of universal dependencies.
+ **7.4.1** - Expose Sentence.norms in Odin rules.
+ **7.4.0** - Added a bidirectional POS model that performs 0.1% better on WSJ.
+ **7.4.0** - Significant improvements to tokenization code. Both CLU and CoreNLP processors now use the same tokenizer. This tokenizer preserves references to the original text.
+ **7.3.1** - Lazily calculate the hash code of mentions for better performance.
+ **7.3.0** - "nmod" collapsing in CluProcessor now uses "_" as connector instead of ":" to align with CoreNLP.
+ **7.3.0** - "nmod" collapsing now follows multi-word expressions (MWE) to generate multi-word prepositions.
+ **7.2.2** - This is just a copy of 7.2.1, whose release on maven was broken
+ **7.2.1** - Cross-compile for scala 2.11 and 2.12
+ **7.2.1** - Prevented NER from changing state for the ShallowNLPProcessor and FastNLPProcessor
+ **7.2.1** - Removed dependency to "edu.washington.cs.knowitall.nlptools"  %  "nlptools-stem-morpha_2.10", which was introducing dependency conflicts.
+ **7.2.0** - Added a BiMEMM named entity recognizer based on the CoNLL dataset to CluProcessor.
+ **7.1.1** - Multiple bug fixes to CluProcessor.
+ **7.1.1** - Added TextToCoNLLU, which converts raw text into the CoNLL-U format.
+ **7.1.0** - CluProcessor now contains a MEMM model for syntactic chunking.
+ **7.1.0** - CluProcessor now produces both universal and Stanford dependencies, both basic and enhanced.
+ **7.0.2** - The ensemble parsing model in CluProcessor is now guaranteed to produce cycle-free outputs.
+ **7.0.1** - Added feature to Odin to import variables from a file. Works similarly to taxonomies.
+ **7.0.0** - Updated library to use Scala 2.12 by default. Add ability to remove lexicon members.
+ **7.0.0** - Retrained our two discourse parsers (constituent- and dependency-syntax based) to work with universal dependencies.
+ **7.0.0** - Switched to CoreNLP 3.8.0 and universal dependencies.
+ **6.3.0** - Branched released version 6.3.0 as base for future Reach development (branch name 'reach').
+ **6.3.0** - Renamed modification trait to Attachment.
+ **6.2.0** - Added ability to add Modifications to mentions.
+ **6.2.0** - Added openie package, which contains EntityFinder. 
+ **6.2.0** - Added wrappers for the regression functions in `liblinear` and `libsvm`. 
+ **6.2.0** - Added functionality to use a serialized model to LexiconNER. 
+ **6.1.4** - Added org.clulab.sequences.LexiconNER to match known entity names efficiently.
+ **6.1.4** - Allow for `odin` variable use in import paths.
+ **6.1.3** - Update core server parameters. Increment Akka version to 2.5.4.
+ **6.1.3** - Added BioCluProcessor, which has all the functionality of BioNLPProcessor, with the exception of the bio NER (to be added later).
+ **6.1.3** - Separated the `models` jar in two jars: `modelsmain`, which contains data relevant for `processors-main`, and `modelscorenlp`, which contains data relevant for `processors-corenlp`.
+ **6.1.3** - Bug fix: fixed model loading for the maltparser models, which were not loading properly from the processors-models jar.
+ **6.1.3** - `CluProcessor` can now be configured through Config.
+ **6.1.2** - Update to use Bioresources 1.1.24 which includes the Harvard BioEntities updates of 8/22/2017.
+ **6.1.2** - Bug fix: made Counter.keySet efficient (again). This severely affected the performance of the discourse parser, which was extremely slow.
+ **6.1.2** - Replaced the CRF model based on Mallet with an in-house BiMEMM model. Performance is 97% on WSJ and 98% on Genia.
+ **6.1.0/1** - Added the word-by-word voting ensemble model to the CluProcessor parser. Performance (with gold POS tags) is: LAS on WSJ: 89.2%, LAS on Genia: 87.4%.
+ **6.1.0/1** - Added collapsed Stanford dependencies to CluProcessor. See `org.clulab.processors.clu.syntax.EnhancedDependencies` for details.
+ **6.1.0/1** - Bug fix: the parsing model in CluProcessor was broken (maltparser was trained incorrectly).
+ **6.0.7** - Implement Akka-based processors server, with default configuration for BioNLP.
+ **6.0.7** - Added basic Stanford dependencies to CluProcessor. These are implemented using maltparser trained on WSJ + Genia. Note that currently the CluProcessor POS tagger is trained just on WSJ.
+ **6.0.6** - Rename Odin subproject library artifact to processors-odin, for consistency.
+ **6.0.6** - Added CluProcessor, which contains only in-house tools under the Apache license. Currently contains English tokenization, lemmatization, and POS tagging.
+ **6.0.6** - Added `graph` field to Odin rules, which denotes the graph (ex. stanford collapsed dependencies) used within the pattern (`incoming`, `outgoing`, etc.).
+ **6.0.6** - Added ranged quantifiers to Odin graph pattern arguments.
+ **6.0.5** - Added charset parameter to ExtractorEngine methods.
+ **6.0.5** - The FastBioNLPProcessor is available in the ProcessorsShell.
+ **6.0.4** - Use consistent SBT version across projects. Fix lib clash: allow ai.lum.common to dicate typesafe.config version.
+ **6.0.3** - Added FastBioNLPProcessor. Update for Bioresources 1.1.22 containing updated MITRE Phase3 override KB.
+ **6.0.3** - The rule NER is now a singleton object, so it can be shared between different processor objects.
+ **6.0.3** - Remove use of Phase3 override file to phase3 branch only.
+ **6.0.2** - Update to use Bioresources 1.1.21 containing updated MITRE PhaseIII override KB.
+ **6.0.2** - Enhance NER to load multiple override files. Update to use Bioresources 1.1.20 containing MITRE PhaseIII override KB.
+ **6.0.2** -  file I/O fix related to `json` deserialization.
+ **6.0.2** - Fixed odin bug related to` ^` in lookbehinds. Update to use Bioresources 1.1.19 containing Bioentities project KBs.
+ **6.0.1** - Added support to odin for redefining variables.
+ **6.0.1** - Added support to odin for matching mention arguments.
+ **6.0.1** - Added support to odin for cross-sentence patterns.
+ **6.0.1** - Fixed odin bug when matching overlapping entities with same end at the beginning of a pattern.
+ **6.0.1** - Improved validation of named entities in RuleNER.
+ **6.0.1** - Added automated lexical variations to the BioNLPProcessor RuleNER.
+ **6.0.1** - Bug fix in multi-world entity matching in the RuleNER.
+ **6.0.0** - Reorganization into 3 subprojects with different licenses: main, corenlp, models.
+ **6.0.0** - Optimizations to `json` serialization/deserialization.
+ **5.9.6** - Serialize/deserialize `Document`, `Mention`, etc. to/from `json`.
+ **5.9.5** - Bug fix release: do not tag XML tags such as XREF... as named entities.
+ **5.9.4** - Update to use bioresources 1.1.15.
+ **5.9.3** - Improved support for multi-token triggers in dependency patterns in Odin.
+ **5.9.3** - Improved tokenization of n-ary complexes for BioNLPProcessor. Reverts some of CoreNLP's aggressive tokenization.
+ **5.9.2** - Add numeric comparisons and distributional similarity comparisons via word embeddings to Odin (word2vec).
+ **5.9.2** - Ensure amino acid abbreviations parse correctly. Tokenize sites that appear as prefixes; e.g. "Tyr-phosphorylated". Use Bioresources 1.1.13.
+ **5.9.1** - Bug fixes for KBLoader and X/Y heterodimer bugs. Update some BioNLP* tests.
+ **5.9.1** - Added override capability to the rule NER in BioNLPProcessor.
+ **5.9.1** - Improved tokenization of protein families and other edge cases.
+ **5.9.1** - Bug fix in the RF classifier: handle features that appear only with 0 values during training.
+ **5.9.1** - Updated to use bioresources 1.1.10.
+ **5.9.0** - Changed top package from edu.arizona.sista to org.clulab.
+ **5.8.6** - Global tokenization for complexes in BioNLPProcessor. Added support for sentiment analysis in CoreNLPSentimentAnalyzer.
+ **5.8.5** - Updated to bioresources 1.1.9. Improved the handling of stop-list entities in the bio NER.
+ **5.8.4** - Updated to bioresources 1.1.6. Add and load TissueType KB. Synch the KBLoader with NER configuration in Bioresources.
+ **5.8.3** - Updated to bioresources 1.1.4. The BioNLPProcessor POS tagger now correctly tags verbs for biochemical interactions as verbs.
+ **5.8.2** - Implemented unrestricted lookbehind for Odin's surface patterns. Updated to bioresources 1.1.1. Added support for unbalanced data to LibLinearClassifier. Incremental improvements to the SRL system.
+ **5.8.1** - Improved Odin's variables compatibility with YAML. Switched BioNLPProcessor to using bioresources rather than the local resources. Bug fix: RuleNER was not producing the longest possible match all the time.
+ **5.8.0** - Added in-house random forest implementation. Removed Weka dependency and its RF classifier. Removed RF reranking classifier. Bug fix in Odin: allow `$` and `\` in variable values. Restricted Odin variable names to valid Java identifiers. Added object Serializer. We no longer intern Strings by default in BioNLPProcessor.
+ **5.7.3** - Added support for context entities: species, organs, cell lines, cell types.
+ **5.7.2** - Keep matched syntactic paths in event/relation mentions. Added support for trigger matching from the state.
+ **5.7.1** - Made Odin event mentions friendlier for event coreference.
+ **5.7.0** - Added chunker for shallow syntax based on the CoNLL-2000 shared task. Improved Odin's Java API. Several bugfixes.
+ **5.6.0** - Changed organization to org.clulab. Changed to 3-number versions. Changed resource loader behavior for the Odin "import" command.
+ **5.5** - Added support for rule templates, importing grammar files, and taxonomies in Odin.
+ **5.4** - The Sentence class now stores all available dependency types generated by a processor, e.g., both basic and collapsed Stanford dependencies. Multiple improvements to the Odin language.
+ **5.3** - Many incremental improvements to ODIN and `BioNLPProcessor`, including better tokenization of biomedical text, and a revamped bio NER, which now combines an in-house CRF with a rule-based system, with rules derived from multiple knowledge bases such as Uniprot.
+ **5.2** - Version 2 of ODIN, including a cleaner (more declarative) rule language, which minimizes the need for custom actions. See `edu.arizona.sista.odin.domains.toydomain` for an example of a toy domain, and `edu.arizona.sista.odin.domains.bigmechanism.dryrun2015` for a complete biomedical domain.
+ **5.1** - Improved tokenization for the bio domain. Replaced the BANNER NER for the bio domain with our own implementation. Installing BANNER is thus no longer necessary.
+ **5.0** - changed to Java 8, Scala 2.11, and CoreNLP 3.5.1. First public release of ODIN (domain-independent event extraction) framework, in the `edu.arizona.sista.odin` package. First release of ODIN's DARPA biomedical grammar. `FastNLPProcessor` now supports both the Malt and the new Stanford NN dependency parser (the Stanford parser is now the default setting).
+ **4.0** - added `BioNLPProcessor`. Install our fork of the [BANNER named entity recognizer](https://github.com/sistanlp/banner) before!
+ **3.3** - bug fix: make sure DocumentSerializer.load() works when multiple documents are serialized into the same file.
+ **3.2** - Added a discourse parser to `FastNLPProcessor`. This performs marginally worse than the one in `CoreNLPProcessor`, but it is much faster for end-to-end processing, due to the shift-reduce syntactic parser.
+ **3.1** - Minimal functionality added to the learning package. Changed to CoreNLP 3.3.1.
+ **3.0** - Added a RST discourse parser to `CoreNLPProcessor`. Added the `edu.arizona.sista.learning` package. Utils classes are now under `edu.arizona.sista.utils` rather than `edu.arizona.sista.processors.utils`.
+ **2.2** - Various bug fixes. Added support for basic dependencies to `CoreNLPProcessor`.
+ **2.1** - Bug fix in FastNLPProcessor: better root detection algorithm, robust to malt inconsistencies.
+ **2.0** - We now support two processors: `CoreNLPProcessor` and `FastNLPProcessor`. Changed the package name from `e.a.s.processor` to `e.a.s.processors`. Added Java usage example to README. Updated to CoreNLP 3.3.0. Added better unit tests to check for thread safetiness.
+ **1.5** - Bug fixing. Made the string interning process (see `Processor.in`) local to each thread to avoid concurrency issues in multi-threaded programs. Added new unit tests. Added minor functionality to Lexicon.
+ **1.4** - Code cleanup. Added some minor new functionality such as finding base NPs in the Trees class.
+ **1.3** - Reverted back to the `1.x` version numbers, since we will add other software here not just CoreNLP. Added correct mvn dependencies for the CoreNLP jars. Removed the `install*.sh` scripts, which are no longer needed.
+ **3.2.0** - Updated to Scala 2.10.1 and CoreNLP 3.2.0. Changed versioning system to be identical to CoreNLP's, so it's clear which CoreNLP version is used.
+ **1.0** - Initial release
