#!/usr/bin/Rscript
#
# File:    plotStderr.R
# Author:  Alex Stivala
# Created: September 2015
#
# $Id: plotStderr.R 648 2016-09-27 07:40:15Z stivalaa $
#
# Read multiple IPNet estimation results (from collect_estimation_results.sh)
# concatenated together from stdin and plot
# estimated standard error.
#
# Usage: Rscript plotStderr.R baseline_table.txt  outfilename.eps
#
# baseline_table.txt contains the results
# for the baseline case i.e. full network.
#
#
# Output file is PostScript file outfilename.eps as specified on command line
##
#  
# WARNING: output file is overwritten if it exists.
#
# Example:
# cat estimation_addhealth_simulated_activity0_removed??.txt | awk '!a[$$0]++' | Rscript $(SCRIPTDIR)/plotStderr.R estimation_addhealth_simulated_activity0_baseline.txt estimation_addhealth_simulated_activity0_stderr.eps
# 

library(ggplot2)
library(grid)
library(gridExtra)
library(reshape)
library(doBy)

zSigma <- 1.96 # number of sd for 95% CI

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
baseline$percentRemoved <- NULL
D <- read.table(file("stdin"), header=TRUE, stringsAsFactors=FALSE)

D <- merge(D, baseline, by="Effect", suffixes=c("",".baseline"))

D <- melt(D, id = c('percentRemoved','Effect'))
D <- summaryBy(value ~ percentRemoved + Effect + variable, data=D, FUN=c(mean, sd))

D <- D[which(D$variable == 'StdErr'), ]



# rename effects for plot labels
for (i in 1:length(effects)) {
    D[which(D$Effect == effects[i]),"Effect"] <- effect_names[i]
}

# order factors according to the effect_names vector to order in facet
D$Effect <- factor(D$Effect, levels = effect_names)


    # FIXME dodgy use of output filename to get original network size
    if (length(grep('project90', output_filename)) > 0) {
        D$numNodes <- (1-(D$percentRemoved/100)) * 4430 #  only works for N=4430
    } else if (length(grep('addhealth_', output_filename)) > 0) {
        D$numNodes <- (1-(D$percentRemoved/100)) * 2539 #  only works for N=2539
    } else if (length(grep('n1000_', output_filename)) > 0) {
        D$numNodes <- (1-(D$percentRemoved/100)) * 1000 #  only works for N=1000
    } else {
        D$numNodes <- (1-(D$percentRemoved/100)) * 500 #  only works for N=500
    }

    p <- ggplot(D, aes(x = numNodes, y = value.mean))
    p <- p + xlab('Number of nodes in sample')

    p <- p + geom_point()
    p <- p + geom_errorbar(aes(ymin=value.mean - zSigma*value.sd, ymax=value.mean + zSigma*value.sd))
    p <- p + theme_bw()
    p <- p + theme(panel.background = element_blank(),
                   ## panel.grid.major = element_blank(),
                   ## panel.grid.minor = element_blank(),
                   plot.background = element_blank(),
                   strip.background = element_blank(),
                   legend.text = 	element_text(size = 10, colour = "black"),
                   legend.key =  	element_rect(fill = "white", colour = "white"),
                   panel.border = element_rect(color = 'black')
                   )
    
    p <- p + ylab('Estimated standard error')
#    p <- p + theme(axis.ticks.x = element_blank())
    p <- p + facet_wrap(~ Effect, scales="free_y")

    postscript(output_filename, onefile=FALSE,
               paper="special", horizontal=FALSE, width=9, height=6)
    print( p)
    dev.off()

