#!/bin/bash

#
# This installs the CoreNLP jar in the local maven repository.
# This is only needed if maven can't find it in the central repository! As of 10/8/13, this was needed for 3.2.0.
# Before running this scripts, download CoreNLP and copy the stanford-corenlp-X.Y.Z.jar in this directory.
#
# If you use Windows and cannot run this script as is, this essentially copies the jar file to:
# ~/.m2/repository/edu/stanford/nlp/stanford-corenlp/3.2.0/stanford-corenlp-3.2.0.jar 
#

CORENLP_VER=3.2.0

mvn install:install-file -DartifactId=stanford-corenlp -DgroupId=edu.stanford.nlp -Dversion=$CORENLP_VER -Dpackaging=jar -Dfile=./stanford-corenlp-$CORENLP_VER.jar
