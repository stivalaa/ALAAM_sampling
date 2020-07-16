#!/bin/sh
#
# File:    collect_estimation_results.sh
# Author:  Alex Stivala
# Created: September 2015
#
# collect all the estimation results files in cwd and write summary table
# to stdout with header line for use with R read.table
#
# Usage: collect_estimation_results.sh percent
#              percent is percentage of nodes removed for percentRemoved field
#
#
DIR=$(cd $(dirname "$0"); pwd)

PATH=${PATH}:${DIR}

if [ $# -ne 1 ]; then
    echo "Usage: $0 percentRemoved " >&2
    exit 1
fi

percentRemoved=$1

echo "percentRemoved sampleId Effect Estimate StdErr tRatio Significant"
for i in estimation-*.txt 
do 
  sample=`basename $i .txt | cut -d- -f2`
  ipnetEstimation2row.sh $i | while read line
  do
    echo $percentRemoved $sample $line
  done
done

