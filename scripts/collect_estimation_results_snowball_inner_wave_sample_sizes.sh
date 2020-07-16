#!/bin/sh
#
# File:    collect_estimation_results_snowball_inner_wave_sample_sizes.sh
# Author:  Alex Stivala
# Created: November 2015
#
# get number of nodes in snowball sample in waves from the zone files
# in cwd and write summary table
# to stdout with header line for use with R read.table
#
# Usage: collect_estimation_results_snowball_inner_wave_sample_sizes.sh max_links num_waves num_seeds
#
#
DIR=$(cd $(dirname "$0"); pwd)

PATH=${PATH}:${DIR}

if [ $# -ne 3 ]; then
    echo "Usage: $0 maxLinks numWaves numSeeds " >&2
    exit 1
fi

maxLinks=$1
numWaves=$2
numSeeds=$3

echo "maxLinks numWaves numSeeds numNodes numNodesInnerWaves sampleId"
for zonefile in snowball_zonefile*_waves*_seeds*_num*.txt 
do 
  sample=`echo $zonefile | sed 's/.*_num\([0-9]*\).*/\1/'`
  maxzone=`cat $zonefile | tr ' ' '\n' | sort -n | uniq | tail -1`
  if [ $maxzone != $numWaves ]; then
      echo "ERROR: $zonefile maxzone = $maxzone but numWaves = $numWaves" >&2
      continue
  fi
  numNodesInnerWaves=0
  zone=0
  while [ $zone -lt $numWaves ]; do
      zonenodecount=`cat $zonefile | grep -o $zone | wc -l`
      numNodesInnerWaves=`expr $numNodesInnerWaves + $zonenodecount`
      zone=`expr $zone + 1`
  done
  outernodecount=`cat $zonefile | grep -o $numWaves | wc -l`
  totalnodecount=`expr $outernodecount + $numNodesInnerWaves`
  if [ $totalnodecount != `cat $zonefile | wc -w` ]; then
      echo "ERROR: $zonefile totalnodecount = $totalnodecount wrong" >&2
      continue
  fi
  echo $maxLinks $numWaves $numSeeds $totalnodecount $numNodesInnerWaves $sample
done


