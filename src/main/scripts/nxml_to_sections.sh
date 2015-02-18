#!/bin/sh

#
# Extracts abstract and body from a paper in nxml format
# Based on process_paper.sh but calls just nxml2txt and split_abstract_body.py
#

FILENAME="$1"
OUT_DIR="$2"

# what is my directory?
export MYDIR=$(dirname $0)
export ORIGDIR=`pwd`
echo "Changing working dir to $MYDIR from $ORIGDIR"
# we have to be in the directory where this script is; we will revert at the end
cd $MYDIR

# make output directory if needed
[ ! -d $OUT_DIR ] && mkdir -p $OUT_DIR

# command paths
ABSTRACT_BODY="split_abstract_body.py"
# document name for input paper
docname=$(basename $FILENAME .nxml)

# files to be created
txt_file="${OUT_DIR}/${docname}.txt"
so_file="${OUT_DIR}/${docname}.so"
abstract_file="${OUT_DIR}/${docname}.abstract.txt"
body_file="${OUT_DIR}/${docname}.body.txt"
merged_file="${OUT_DIR}/${docname}.merged.txt"

# extract text
nxml2txt $FILENAME $txt_file $so_file

# split abstract and body
#$ABSTRACT_BODY $txt_file $so_file $abstract_file $body_file
$ABSTRACT_BODY $txt_file $so_file $merged_file $merged_file

# no need to keep the .so file anymore
rm -f $so_file

# change directory back to where we were originally
cd $ORIGDIR
