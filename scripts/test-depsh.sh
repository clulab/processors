#!/bin/bash

#
# Script to test a syntactic dependency parser model, for unlabeled dependencies only
#

if [ "$#" -ne 1 ]; then
    echo "You must provide the model name in the command line!"
    exit 1
fi

MODELH=$1

sbt 'runMain org.clulab.dynet.Metal -test '"$MODELH"' -conf org/clulab/mtl-en-depsh.conf'

