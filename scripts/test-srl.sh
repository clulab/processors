#!/bin/bash

#
# Script to test a SRL model for the recognition of arguments (assuming gold predicates)
#

MODEL=$1

sbt 'runMain org.clulab.dynet.Metal -test '"$MODEL"' -conf org/clulab/mtl-en-srla.conf'

