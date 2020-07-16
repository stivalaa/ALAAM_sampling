#!/usr/bin/Rscript
#
# File:    plotProject90PowerLaw.R
# Author:  Alex Stivala
# Created: August 2016
#
# Plot power law and log-normal fit to Project 90 giant 
# component degree distribution,
# using the poweRlaw package (Gillespie 2015 J. Stat. Soft)
#
# Usage: Rscript plotProjec90PowerLaw.R <data_dirctory>
#
# <data_directory>  is directory containing the Project 90 data
# Writes output to project90_giantcomponent_powelawer.eps (WARNING: overwrites)
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
source_local('readProject90Data.R')
source_local('snowballSample.R')

args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 1) {
  cat("Usage: plotHostpitalPowerLaw <data_dirctory>\n")
  quit(save="no")
}
datadir <- args[1]

g <- read_project90_data(datadir)
g <- giant.component(g)
summary(g)
outfile <- "project90_giantcomponent_powerlaw.eps"
postscript(outfile, onefile=FALSE,
           paper="special", horizontal=FALSE, width=9, height=6)
par(mfrow=c(1,2))
plot_power_law(g, 'Project 90 giant component', TRUE)
dev.off()


