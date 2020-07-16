#!/usr/bin/Rscript
#
# File:    plotNumConverged.R
# Author:  Alex Stivala
# Created: September 2015
#
#
# Read multiple IPNet estimation error summary statistics results (generated
# by plotIPNetResults.R) concatenated together from stdin and plot
# number of ALAAM estimations which converged.
#
# Usage: Rscript plotNumConverged.R  outfilename.eps
#
# This versino plots different sampling types (full snowball, maxlinks limited
# snowball with various maxlinks values) on teh same plot.
#
# Output file is PostScript file outfilename.eps as specified on command line
#
#  
# WARNING: output file is overwritten if it exists.
#

library(ggplot2)
library(grid)
library(gridExtra)
library(reshape)
library(doBy)


# read in R source file from directory where this script is located
#http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
source_local <- function(fname){
  argv <- commandArgs(trailingOnly = FALSE)
  base_dir <- dirname(substring(argv[grep("--file=", argv)], 8))
  source(paste(base_dir, fname, sep=.Platform$file.sep))
}

source_local('effectNames.R')



args <- commandArgs(trailingOnly=TRUE)

output_filename <- args[1]

label_function <- get_label_function(output_filename)

D <- read.table(file("stdin"), header=TRUE, stringsAsFactors=FALSE)

D <- D[ , names(D) %in% c('numSeeds','numWaves','maxLinks','Effect','numConverged')]


maxNumSeeds <- max(D$numSeeds)    
D$numWaves <- as.factor(D$numWaves)
D$numSeeds <- as.factor(D$numSeeds)
D$maxLinks <- as.factor(D$maxLinks)
D$Effect <- as.factor(D$Effect)


D <- melt(D, id=c('numSeeds','maxLinks','numWaves','Effect'))
D <- summaryBy(value ~ numSeeds + maxLinks + numWaves +          variable,
               data = D, FUN = c(mean,sd))
D <- D[which(D$variable == 'numConverged'), ]
p <- ggplot(D, aes(x = numSeeds, y = value.mean,
                   colour = maxLinks,
                   shape = maxLinks))
p <- p + facet_grid(. ~ numWaves, labeller = label_function,
                    scales="free")
p <- p + xlab('Number of seeds')
p <- p + geom_point()

p <- p + scale_x_discrete(breaks=seq(0, maxNumSeeds, 10))

p <- p + theme_bw()
p <- p + theme(panel.background = element_blank(),
               ## panel.grid.major = element_blank(),
               ## panel.grid.minor = element_blank(),
               plot.background = element_blank(),
               strip.background = element_blank(),
               legend.text = element_text(size = 10, colour = "black"),
               legend.key = element_rect(fill = "white", colour = "white"),
               panel.border = element_rect(color = 'black')
               )
p <- p + ylab('number of converged estimations')
#p <- p + theme(axis.ticks.x = element_blank())
p <- p + scale_colour_brewer("m", palette = "Dark2")
#p <- p + scale_linetype("m")
p <- p + scale_shape("m")


postscript(output_filename, onefile=FALSE,
           paper="special", horizontal=FALSE, width=9, height=6)
print(p)
dev.off()

