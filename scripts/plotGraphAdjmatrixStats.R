#!/usr/bin/Rscript
#
# File:    plotGraphAdjmatrixStats.R
# Author:  Alex Stivala
# Created: August 2016
#
# Plot degree distribution etc. of a graph from adjacency matrix
#
# Usage: Rscript plotGraphAdjmatrixStats.R <adjmatrix>
#
# Writes output to adjmatrix_stats.eps in cwd (WARNING: overwrites)
#

library(igraph)

# read in R source file from directory where this script is located
#http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
source_local <- function(fname){
  argv <- commandArgs(trailingOnly = FALSE)
  base_dir <- dirname(substring(argv[grep("--file=", argv)], 8))
  source(paste(base_dir, fname, sep=.Platform$file.sep))
}

source_local('plotGraphStatistics.R')
source_local('readFiles.R')

args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 1) {
  cat("Usage: Rscript plotGraphAdjmatrixStats.R <adjmatrix>\n")
  quit(save="no")
}
adjmatrix_filename <- args[1]
networkBasename <- sub("(.+)[.].+", "\\1", basename(adjmatrix_filename))
outfile <- paste(networkBasename, "_stats.eps", sep="")

g <- read_graph_matrix_file(adjmatrix_filename, directed=FALSE)
print(summary(g))
plot_graph_statistics(g, outfile)

