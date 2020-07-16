#!/bin/sh
#
# genrandomattributes.sh
#
# Generate random binary attributes 
#
# Usage:
#    genrandomattributes N Ntrue
#
#      N is the number of attributes to generate
#      Ntrue is the number of true attributes 
#
# Output is to stdout in PNet format for attributes; first line is header
# 
# ADS 25Sept2013
#
# Uses GNU utils seq, shuf etc.
#
if [ $# -ne 2 ]; then
  echo "Usage: $0 N Ntrue" >&2
  exit 1
fi
N=$1
Ntrue=$2
if [ $Ntrue -gt $N ]; then
  echo "$0: Ntrue must be <= N" >&2
  exit 1
fi

#echo "binaryAttribute"
for i in `seq $N`
do
  if [ $i -le $Ntrue ]; then
    echo 1
  else
    echo 0
  fi
done | shuf 


