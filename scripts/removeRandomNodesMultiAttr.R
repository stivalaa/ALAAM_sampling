#3544!/usr/bin/Rscript
#
# File:    removeRandomNodesMultiAttr.R
# Author:  Alex Stivala
# Created: September 2015
#
# Read network and attributes and set of outcome files (ALAAM simulation model) 
# and randomly remove given fraction of nodes, writing out the new newtork
# and attributes and outcomes files. This version handles multiple attributes
# (of binary and categorical types), for use with Project 90 data for example.
#
# Usage: Rscript removeRandomNodesMultiAttr.R percentage networkFilename binAttrFileName catAttrFileName outcomeFilePattern
#
#    percentage            - percentage of nodes to remove
#    networkFileName       - network matrix file name  (no header)
#    binAttrFileName       - binary attribute file name ( no header)
#    catAttrFileName      - catinuous attribute file name (no header)
#    outcomeFilePattern    - filename pattern for outcome files (Pajek headers)
#
# Output files are in cwd, basename of input files with _removedX_numY
# appended before suffix,  where X is percentRemoved e.g. _removed10
# and Y is sample number (from attribute outcome sample) e.g. 910000
# and also zonefile.txt (a 1 for each node in output network)
#
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

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 5) {
 cat('Usage: Rscript removeRandomNodesMultiAttr.R percentage networkFilename binAttrFileName catAttrFileName outcomeFilePattern\n')
 quit(save="no")
}
percentRemoved <- as.integer(args[1])
networkFilename <-args[2]
binAttrFilename <- args[3]
catAttrFilename <- args[4]
outcomeFilePattern <- args[5]

stopifnot(percentRemoved >= 0 && percentRemoved < 100)

suffix <- paste('_removed', percentRemoved, sep='')
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
keepCount = as.integer(((100 - percentRemoved)/100)*vcount(g))
cat('keeping ', keepCount, ' of ', vcount(g), ' nodes\n')
g_orig <- g
for (outcomeFilename in Sys.glob(outcomeFilePattern)) {
  V(g)$outcome <- read_outcome_file(outcomeFilename)
  keepNodeNums <- sample.int(vcount(g), keepCount, replace=FALSE)
  g <- induced.subgraph(g, V(g)[keepNodeNums])
  outcomeBasename <- sub("(.+)[.].+", "\\1", basename(outcomeFilename))
  samplenum <- sub(".+[a-z]([0-9]+)[.].+", "\\1", basename(outcomeFilename))
  samplesuffix <- paste('_num', samplenum, sep='')
  outcomeOutputFilename <- paste(outcomeBasename, suffix, '.clu', sep='')
  networkOutputFilename <- paste(networkBasename, suffix, samplesuffix, '.txt', sep='')
  binAttrOutputFilename <- paste(binAttrBasename, suffix, samplesuffix, '.txt', sep='')
  catAttrOutputFilename <- paste(catAttrBasename, suffix, samplesuffix, '.txt', sep='')
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

  g <- g_orig
}

cat(rep(1, keepCount), file='zonefile.txt')
cat('\n', file='zonefile.txt', append=TRUE)
