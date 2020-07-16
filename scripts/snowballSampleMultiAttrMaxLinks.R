#!/usr/bin/Rscript
#
# File:    snowballSampleMultiAttrMaxLinks.R
#
# Author:  Alex Stivala
# Created: September 2016
#
# Read network and attributes and set of outcome files (ALAAM simulation model) 
# and take snowball sample, following only up to specified maximum number
# of links at each node (rather than all links) writing out the new newtork
# and attributes and outcomes files. This version handles multiple attributes
# (of binary and categroical type), for use with Project 90 data for example.
#
# Usage: Rscript snowballSampleMultiAttrMaxLinks.R num_waves num_seeds max_links networkFilename binAttrFileName catAttrFileName outcomeFilePattern
#
#    num_waves             - number of snowball waves            
#    num_seeds             - number of snowball seeds
#    max_links             - max number of edges to follow from a node
#    networkFileName       - network matrix file name  (no header)
#    binAttrFileName       - binary attribute file name ( no header)
#    catAttrFileName       - categorical attribute file name (no header)
#    outcomeFilePattern    - filename pattern for outcome files (Pajek headers)
#
# Output files are in cwd, basename of input files with _maxlinksX_wavesY_seedsZ_numN
# appended before suffix,  where 
#  X is max number of edges to follow e.g. _maxlinks5
# and Y is waves e.g. _waves2
# and Z is number of seeds e.g. _seeds3 
# and N is sample number (from attribute outcome sample) e.g. 910000
#
# also zonefile and snowball_zonefile with suffixies above
# zonefile is a fake zone file (all 1) for use with IPNet
# snowball_zonefile shows the snowball sample zone of each node from
# snowball_sample() procedure
#
#  Example: Rscript snowballSampleMultiAttrFull.R  2 10 3 ${HOME}/alaam/simulated_Project90realAttributes/project90_giantcomponent_adjmatrix.txt /home/stivalaa/alaam/simulated_Project90realAttributes/project90_giantcomponent_binattr.txt  /home/stivalaa/alaam/simulated_Project90realAttributes/project90_giantcomponent_catattr.txt  ${HOME}/alaam/simulated_Project90realAttributes/sample-project90\*.clu
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
if (length(args) != 7) {
 cat('Usage: Rscript snowballSampleMultiAttrMaxLinks.R num_waves num_seeds max_links networkFilename binAttrFileName catAttrFileName outcomeFilePattern\n')
 quit(save="no")
}
num_waves <- as.integer(args[1])
num_seeds <- as.integer(args[2])
max_links <- as.integer(args[3])
networkFilename <-args[4]
binAttrFilename <- args[5]
catAttrFilename <- args[6]
outcomeFilePattern <- args[7]


suffix <- paste('_maxlinks', max_links, '_waves', num_waves, '_seeds', num_seeds, sep='')
networkBasename <- sub("(.+)[.].+", "\\1", basename(networkFilename))
binAttrBasename <- sub("(.+)[.].+", "\\1", basename(binAttrFilename))
catAttrBasename <- sub("(.+)[.].+", "\\1", basename(catAttrFilename))

g <- read_graph_matrix_file(networkFilename, directed=FALSE)
binAttrs <- read_attr_file_multi(binAttrFilename)
catAttrs <- read_attr_file_multi(catAttrFilename)
for (aname in names(binAttrs)) {
  g <- set.vertex.attribute(g, paste("bin", aname, sep="_"), 
                            V(g)[order(V(g)$name)], binAttrs[ ,aname])
}
for (aname in names(catAttrs)) {
  g <- set.vertex.attribute(g, paste("cat", aname, sep="_"), 
                            V(g)[order(V(g)$name)], catAttrs[ ,aname])
}
binAttrNames <- paste("bin", names(binAttrs), sep="_")
catAttrNames <- paste("cat", names(catAttrs), sep="_")
g_orig <- g
for (outcomeFilename in Sys.glob(outcomeFilePattern)) {
  V(g)$outcome <- read_outcome_file(outcomeFilename)
  seed_nodes <- sample.int(vcount(g), num_seeds, replace=FALSE)
  g <- snowball_sample(g, num_waves, seed_nodes, max_links)
  outcomeBasename <- sub("(.+)[.].+", "\\1", basename(outcomeFilename))
  samplenum <- sub(".+[a-z]([0-9]+)[.].+", "\\1", basename(outcomeFilename))
  samplesuffix <- paste('_num', samplenum, sep='')
  outcomeOutputFilename <- paste(outcomeBasename, suffix, '.clu', sep='')
  networkOutputFilename <- paste(networkBasename, suffix, samplesuffix, '.txt', sep='')
  binAttrOutputFilename <- paste(binAttrBasename, suffix, samplesuffix, '.txt', sep='')
  catAttrOutputFilename <- paste(catAttrBasename, suffix, samplesuffix, '.txt', sep='')
  fakezoneOutputFilename <- paste('zonefile', suffix, samplesuffix, '.txt', sep='')
  zoneOutputFilename <- paste('snowball_zonefile', suffix, samplesuffix, '.txt', sep='')
  write_graph_file(networkOutputFilename, g, write_header=FALSE)
  binAttrs <- data.frame("id" = V(g)[order(V(g)$name)]$name)
  catAttrs <- data.frame("id" = V(g)[order(V(g)$name)]$name)
  for (aname in binAttrNames) {
    binAttrs[,aname] <- get.vertex.attribute(g, aname, V(g)[order(V(g)$name)])
  }
  for (aname in catAttrNames) {
    catAttrs[,aname] <- get.vertex.attribute(g, aname, V(g)[order(V(g)$name)])
  }
  # remove node names from data frame of attributes so don't appear in output
  binAttrs$id <- NULL
  catAttrs$id <- NULL
  write_attr_file_multi(binAttrOutputFilename, binAttrs)
  write_attr_file_multi(catAttrOutputFilename, catAttrs)
  write_outcome_file(outcomeOutputFilename, V(g)$outcome)

  # fake zonefile (all 1) for estimation in IPNet with no conditional estimation
  cat(rep(1, vcount(g)), file=fakezoneOutputFilename)
  cat('\n', file=fakezoneOutputFilename, append=TRUE)

  # snowball sample node zones
  cat (V(g)$zone, file=zoneOutputFilename)
  cat('\n', file=zoneOutputFilename, append=TRUE)

  g <- g_orig
}

