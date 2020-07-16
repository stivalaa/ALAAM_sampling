#!/usr/bin/Rscript
#
# File:    convertAddHealthToAdjmatrix.R
# Author:  Alex Stivala
# Created: August 2016
#
# $Id: convertAddHealthToAdjmatrix.R 623 2016-09-13 06:33:57Z stivalaa $
#
# Load the Add Health network data and convert it
# to adjacency matrix format for use in IPNet etc.
#
#
# Usage: RScript convertAddHealthToAdjmatrix.R datadir
#
#  datadir is the directory containing the Add Health network data
#
#  Output is written to ./addhealth_adjmatrix.txt
#  (WARNING overwritten if it exists)
#

library(igraph)

# read in R source file from directory where this script is located
#http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
source_local <- function(fname){
  argv <- commandArgs(trailingOnly = FALSE)
  base_dir <- dirname(substring(argv[grep("--file=", argv)], 8))
  source(paste(base_dir, fname, sep=.Platform$file.sep))
}

source_local('readAddHealthData.R')
source_local('readFiles.R')
source_local('snowballSample.R')

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 1) {
 cat('Usage: Rscript convertAddHealthToAdjmatrix.R datadir\n')
 quit(save="no")
}

datadir <-args[1]

g <- read_addhealth_data(datadir)
print(g)
write_graph_file("./addhealth_adjmatrix.txt", g, write_header=FALSE)

