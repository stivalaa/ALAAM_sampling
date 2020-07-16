#!/usr/bin/Rscript
#
# File:    snowballSampleFull.R
# Author:  Alex Stivala
# Created: September 2015
#
# Read network and attributes and set of outcome files (ALAAM simulation model) 
# and take snowball sample, writing out the new newtork
# and attributes and outcomes files.
#
# Usage: Rscript snowballSampleFull.R num_waves num_seeds networkFilename binAttrFileName contAttrFileName outcomeFilePattern
#
#    num_waves              - number of snowball waves            
#    num_seeds             - number of snowball seeds
#    networkFileName       - network matrix file name  (no header)
#    binAttrFileName       - binary attribute file name ( no header)
#    contAttrFileName      - continuous attribute file name (no header)
#    outcomeFilePattern    - filename pattern for outcome files (Pajek headers)
#
# Output files are in cwd, basename of input files with _removedX_seedsY_numZ
# appended before suffix,  where X is waves e.g. _waves2
# and Y is number of seeds e.g. _seeds3 
# and Z is sample number (from attribute outcome sample) e.g. 910000
#
# also zonefile and snowball_zonefile with suffixies above
# zonefile is a fake zone file (all 1) for use with IPNet
# snowball_zonefile shows the snowball sample zone of each node from
# snowball_sample() procedure
#
#  Eg. Rscript snowballSampleFull.R 2 3 ../simulated_n500_bin_cont/n500_kstar_simulate12750000_matrix.txt ../simulated_n500_bin_cont/binaryAttribute_50_50_n500.txt ../simulated_n500_bin_cont/continuousAttributes_n500.txt '../simulated_n500_bin_cont/sample-n500_bin_cont*.clu'
# 

library(igraph)

# read in R source file from directory where this script is located
#http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
source_local <- function(fname){
  argv <- commandArgs(trailingOnly = FALSE)
  base_dir <- dirname(substring(argv[grep("--file=", argv)], 8))
  source(paste(base_dir, fname, sep=.Platform$file.sep))
}

source_local('readFiles.R')
source_local('snowballSample.R')

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 6) {
 cat('Usage: Rscript snowballSampleFull.R num_waves num_seeds networkFilename binAttrFileName contAttrFileName outcomeFilePattern\n')
 quit(save="no")
}
num_waves <- as.integer(args[1])
num_seeds <- as.integer(args[2])
networkFilename <-args[3]
binAttrFilename <- args[4]
contAttrFilename <- args[5]
outcomeFilePattern <- args[6]


suffix <- paste('_waves', num_waves, '_seeds', num_seeds, sep='')
networkBasename <- sub("(.+)[.].+", "\\1", basename(networkFilename))
binAttrBasename <- sub("(.+)[.].+", "\\1", basename(binAttrFilename))
contAttrBasename <- sub("(.+)[.].+", "\\1", basename(contAttrFilename))

g <- read_graph_matrix_file(networkFilename, directed=FALSE)
V(g)$binAttr <- read_attr_file(binAttrFilename)
V(g)$contAttr <- read_attr_file(contAttrFilename)
g_orig <- g
for (outcomeFilename in Sys.glob(outcomeFilePattern)) {
  V(g)$outcome <- read_outcome_file(outcomeFilename)
  seed_nodes <- sample.int(vcount(g), num_seeds, replace=FALSE)
  g <- snowball_sample(g, num_waves, seed_nodes)
  outcomeBasename <- sub("(.+)[.].+", "\\1", basename(outcomeFilename))
#  samplenum <- sub(".+cont([0-9]+)[.].+", "\\1", basename(outcomeFilename))
  samplenum <- sub(".+[a-z]([0-9]+)[.].+", "\\1", basename(outcomeFilename))
  samplesuffix <- paste('_num', samplenum, sep='')
  outcomeOutputFilename <- paste(outcomeBasename, suffix, '.clu', sep='')
  networkOutputFilename <- paste(networkBasename, suffix, samplesuffix, '.txt', sep='')
  binAttrOutputFilename <- paste(binAttrBasename, suffix, samplesuffix, '.txt', sep='')
  contAttrOutputFilename <- paste(contAttrBasename, suffix, samplesuffix, '.txt', sep='')
  fakezoneOutputFilename <- paste('zonefile', suffix, samplesuffix, '.txt', sep='')
  zoneOutputFilename <- paste('snowball_zonefile', suffix, samplesuffix, '.txt', sep='')
  write_graph_file(networkOutputFilename, g, write_header=FALSE)
  write_attr_file(binAttrOutputFilename, V(g)$binAttr)
  write_attr_file(contAttrOutputFilename, V(g)$contAttr)
  write_outcome_file(outcomeOutputFilename, V(g)$outcome)

  # fake zonefile (all 1) for estimation in IPNet with no conditional estimation
  cat(rep(1, vcount(g)), file=fakezoneOutputFilename)
  cat('\n', file=fakezoneOutputFilename, append=TRUE)

  # snowball sample node zones
  cat (V(g)$zone, file=zoneOutputFilename)
  cat('\n', file=zoneOutputFilename, append=TRUE)

  g <- g_orig
}

