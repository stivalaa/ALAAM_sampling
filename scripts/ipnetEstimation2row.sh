#!/bin/sh
#
# File:    ipnetEstimation2row.sh
# Author:  Alex Stivala
# Created: September 2015
#
# Read last estimation result from an IPNet estimation output file and
# convert to single row with effect, estimate, stderr, t-ratio
#
# Usage: ipnetEstimation2row.sh   estimation_output_file.txt
#
# E.g.:
#   ipnetEstimation2row.sh  estimation-100000.txt
#
# Output is to stdout
#
# Uses various GNU utils options on echo, etc.

if [ $# -ne 1 ]; then
    echo "usage: $0 estimation_file.txt" >&2
    exit 1
fi

estimationresults=$1

tmpfile=`mktemp`

# get line number of start of last Estimation result
lineno=`grep -n '^NOTE' $estimationresults | cut -d: -f1 | tail -1`

# write last estimation results to tmpfile (start at line no, end on blank line),
awk -vlineno=$lineno 'NR > lineno' $estimationresults | sed -n '0,/^$/p'  | head -n-1  >  $tmpfile

# remove spaces and - in effect name and output row as
# Effect  Estimate  std. errror  convergence statistic  signif.
awk -F'\t' '{printf("%s  %.4f  %.4f  %.4f  %s\n",gensub("[ -]","_","g",$1),$2,$3,$4,($5 == "*" ? "TRUE" : "FALSE"))}' $tmpfile

rm $tmpfile
