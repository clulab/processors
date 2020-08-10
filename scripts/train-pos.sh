#!/bin/bash

#
# Script to train a POS tagger, syntactic chunker, and SRL predicates
#

MODEL=$1

sbt 'runMain org.clulab.dynet.Metal -train '"$MODEL"' -conf org/clulab/mtl-en-pos-chunk-srlp.conf'

