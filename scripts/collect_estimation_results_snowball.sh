#!/bin/sh
#
# File:    collect_estimation_results_snowball.sh
# Author:  Alex Stivala
# Created: September 2015
#
# collect all the estimation results files in cwd and write summary table
# to stdout with header line for use with R read.table
#
# Usage: collect_estimation_results_snowball.sh num_waves num_seeds
#
#
DIR=$(cd $(dirname "$0"); pwd)

PATH=${PATH}:${DIR}

if [ $# -ne 2 ]; then
    echo "Usage: $0 numWaves numSeeds " >&2
    exit 1
fi

numWaves=$1
numSeeds=$2

echo "numWaves numSeeds numNodes sampleId Effect Estimate StdErr tRatio Significant"
for i in estimation-*.txt 
do 
  sample=`basename $i .txt | cut -d- -f2`
  numNodes=`fgrep "* Number of actors = " ${i} | cut -d= -f2`
  if [ -z $numNodes ]; then  # if missing get value from counting matrix rows
      sampleid=`basename $i .txt | cut -d- -f2`
      matrixfilename=n500_kstar_simulate12750000_matrix_waves${numWaves}_seeds${numSeeds}_num${sampleid}.txt
      numNodes=`wc -l ${matrixfilename} | awk '{print $1}'`
  fi
  ipnetEstimation2row.sh $i | while read line
  do
    echo $numWaves $numSeeds $numNodes $sample $line
  done
done

