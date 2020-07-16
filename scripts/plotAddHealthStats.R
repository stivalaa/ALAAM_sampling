#!/usr/bin/Rscript
#
# File:    plotAddHealthStats.R
# Author:  Alex Stivala
# Created: August 2016
#
# Plot degree distribution etc. of Add Health network
#
# Usage: Rscript plotAddHealthStats.R <data_dirctory>
#
# <data_directory>  is directory containing the Add Health network data
# Writes output to addhelath_stats.eps (WARNING: overwrites)
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
source_local('readAddHealthData.R')
source_local('snowballSample.R')

args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 1) {
  cat("Usage: Rscript plotAddHealthStats.R <data_dirctory>\n")
  quit(save="no")
}
datadir <- args[1]

outfile <- "addhealth_stats.eps"

g <- read_addhealth_data(datadir)
print(summary(g))
plot_graph_statistics(g, outfile)

