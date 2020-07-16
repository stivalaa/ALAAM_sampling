#!/usr/bin/Rscript
#
# File:    plotBiasCombined.R
# Author:  Alex Stivala
# Created: September 2015
#
# $Id: plotBiasCombined.R 648 2016-09-27 07:40:15Z stivalaa $
#
# Read multiple IPNet estimation error summary statistics results (generated
# by plotIPNetResults.R) concatenated together from stdin and plot
# Bias
#
# Usage: Rscript plotBiasCombined.R baseline_Stats_table.txt  outfilename.eps
#
# baseline_Stats_table.txt contains the stats (false negative rate etc.)
# for the baseline case i.e. full network, used to plot horizontal line
# with shaded error region for the best possible case (baseline).
# (actually can't get shaded error region to work, using dotted lines instead)
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


# read in R source file from directory where this script is located
#http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
source_local <- function(fname){
  argv <- commandArgs(trailingOnly = FALSE)
  base_dir <- dirname(substring(argv[grep("--file=", argv)], 8))
  source(paste(base_dir, fname, sep=.Platform$file.sep))
}

source_local('effectNames.R')



args <- commandArgs(trailingOnly=TRUE)

baseline_filename <- args[1]
output_filename <- args[2]

effects <- get_effects(output_filename)
effect_names <- get_effect_names(output_filename)
label_function <- get_label_function(output_filename)

baseline <- read.table(baseline_filename, header=TRUE, stringsAsFactors=FALSE)
D <- read.table(file("stdin"), header=TRUE, stringsAsFactors=FALSE)

baseline$percentRemoved <- NULL
D <- merge(D, baseline, by="Effect", suffixes=c("",".baseline"))

D <- D[which(D$numSeeds <= 20), ] #XXX
D <- D[which(D$numSeeds > 1), ] # error bar on Bias too large for 1 seed

maxNumSeeds <- max(D$numSeeds)    
D$numWaves <- as.factor(D$numWaves)
D$numSeeds <- as.factor(D$numSeeds)

p <- ggplot(D, aes(x = numSeeds, y = Bias,
                   colour = as.factor(maxLinks),
#                   linetype = as.factor(maxLinks),
                   shape = as.factor(maxLinks)))
p <- p + facet_grid(Effect ~ numWaves, labeller = label_function,
                    scales="free_y")
p <- p + xlab('Number of seeds')
p <- p + geom_point()
p <- p + geom_errorbar(aes(ymax = Bias_upper, ymin = Bias_lower))    

p <- p + geom_hline(aes(yintercept = Bias.baseline),colour="gray55")
p <- p + geom_hline(aes(yintercept = Bias_lower.baseline), linetype="dashed",colour="gray55")
p <- p + geom_hline(aes(yintercept = Bias_upper.baseline), linetype="dashed",colour="gray55")

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
p <- p + ylab('Bias')
#p <- p + theme(axis.ticks.x = element_blank())
p <- p + scale_colour_brewer("m", palette = "Dark2")
#p <- p + scale_linetype("m")
p <- p + scale_shape("m")
p <- p + scale_x_discrete(breaks=seq(0, maxNumSeeds, 2))    


postscript(output_filename, onefile=FALSE,
           paper="special", horizontal=FALSE, width=9, height=6)
print(p)
dev.off()

