#!/usr/bin/Rscript
#
# File:    plotGraphAdjmatrixPowerLaw.R
# Author:  Alex Stivala
# Created: August 2016
#
# Plot power law and log-normal fit to network as adjacnecy matrix
# using the poweRlaw package (Gillespie 2015 J. Stat. Soft)
#
# Usage: Rscript ploGraphAdjmatrixPowerlaw.R <adjmatrix>
#
# Writes output to <basname adjmatrix>_powerlaw.eps (WARNING: overwrites)
# and p-values etc. to stdout
#


# read in R source file from directory where this script is located
#http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
source_local <- function(fname){
  argv <- commandArgs(trailingOnly = FALSE)
  base_dir <- dirname(substring(argv[grep("--file=", argv)], 8))
  source(paste(base_dir, fname, sep=.Platform$file.sep))
}

source_local('plotPowerLaw.R')
source_local('readFiles.R')

args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 1) {
  cat("Usage: plotGraphAdjmatrixPowerLaw <adjmatrix>\n")
  quit(save="no")
}

adjmatrix_filename <- args[1]
networkBasename <- sub("(.+)[.].+", "\\1", basename(adjmatrix_filename))
outfile <- paste(networkBasename, "_powerlaw.eps", sep="")

g <- read_graph_matrix_file(adjmatrix_filename, directed=FALSE)
summary(g)

postscript(outfile, onefile=FALSE,
           paper="special", horizontal=FALSE, width=9, height=6)
par(mfrow=c(1,2))
plot_power_law(g, networkBasename, TRUE)
dev.off()


