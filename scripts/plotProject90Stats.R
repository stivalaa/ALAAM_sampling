#!/usr/bin/Rscript
#
# File:    plotProject90Stats.R
# Author:  Alex Stivala
# Created: August 2016
#
# Plot degree distribution etc. of Project 90 network giant component
#
# Usage: Rscript plotProject90Stats.R <data_dirctory>
#
# <data_directory>  is directory containing the hospital data
# Writes output to project90_giantcomponent_stats.eps (WARNING: overwrites)
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
source_local('readProject90Data.R')
source_local('snowballSample.R')

args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 1) {
  cat("Usage: Rscript plotProject90Stats.R <data_dirctory>\n")
  quit(save="no")
}
datadir <- args[1]

outfile <- "project90_giantcomponent_stats.eps"

g <- read_project90_data(datadir)
g <- giant.component(g)
print(summary(g))
plot_graph_statistics(g, outfile)

