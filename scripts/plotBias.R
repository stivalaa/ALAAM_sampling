#!/usr/bin/Rscript
#
# File:    plotBias.R
# Author:  Alex Stivala
# Created: September 2015
#
# $Id: plotBias.R 648 2016-09-27 07:40:15Z stivalaa $
#
# Read multiple IPNet estimation error summary statistics results (generated
# by plotIPNetResults.R) concatenated together from stdin and plot
# Bias
#
# Usage: Rscript plotBias.R baseline_Stats_table.txt  outfilename.eps
#
# baseline_Stats_table.txt contains the stats (false negative rate etc.)
# for the baseline case i.e. full network, used to plot horizontal line
# with shaded error region for the best possible case (baseline).
# (actually can't get shaded error region to work, using dotted lines instead)
#
#
# Handles percent nodes removed and snowball sampling differently:
# for the former, single plot, for the latter, different output postscript
# for each number of waves separately due to different nubmers of seeds
# used and mangitudes of errors.
#
# Output file is PostScript file outfilename.eps as specified on command line
# (and separate for each num waves for snowball sampling).
#
#  
# WARNING: output file is overwritten if it exists.
#
# Example: cat estimation_n500_bin_cont_*_error_statistics.txt | Rscript plotBias.R  estimation_n500_bin_cont_rmse.eps
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
baseline$percentRemoved <- NULL
D <- read.table(file("stdin"), header=TRUE, stringsAsFactors=FALSE)

D <- merge(D, baseline, by="Effect", suffixes=c("",".baseline"))

# rename effects for plot labels
for (i in 1:length(effects)) {
    D[which(D$Effect == effects[i]),"Effect"] <- effect_names[i]
}

# order factors according to the effect_names vector to order in facet
D$Effect <- factor(D$Effect, levels = effect_names)

if ("percentRemoved" %in% colnames(D)) {

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
    
    p <- ggplot(D, aes(x = numNodes, y = Bias))
    p <- p + xlab('Number of nodes in sample')

    p <- p + geom_point()
    p <- p + geom_errorbar(aes(ymax = Bias_upper, ymin = Bias_lower))
    ## p <- p + geom_hline(aes(yintercept = Bias.baseline))
    ## p <- p + geom_hline(aes(yintercept = Bias_lower.baseline), linetype="dashed")
    ## p <- p + geom_hline(aes(yintercept = Bias_upper.baseline), linetype="dashed")
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
    
    p <- p + ylab('Bias')
#    p <- p + theme(axis.ticks.x = element_blank())
    p <- p + facet_wrap(~ Effect, scales="free_y")

    postscript(output_filename, onefile=FALSE,
               paper="special", horizontal=FALSE, width=9, height=6)
    print( p)
    dev.off()
} else if ("numWaves" %in% colnames(D)) {
    origD <- D
    for (wave in unique(D$numWaves)) {
        print(wave)#XXX
        D <- origD[which(origD$numWaves == wave),]
        if (wave == 1) {
            D <- D[which(D$numSeeds > 60), ] # XXX 1 to 20 on combined graph
        } else {
            D <- D[which(D$numSeeds >= 10), ] # XXX error bar on Bias too large for small samples
        }
        maxNumSeeds <- max(D$numSeeds)    
        D$numSeeds <- as.factor(D$numSeeds)
        
        p <- ggplot(D, aes(x = numSeeds, y = Bias,
                           colour = as.factor(maxLinks),
#                           linetype = as.factor(maxLinks),
                           shape = as.factor(maxLinks)
                           ))
        p <- p + xlab('Number of seeds')
        p <- p + scale_x_discrete(breaks=seq(0, maxNumSeeds, maxNumSeeds/10))
        p <- p + scale_colour_brewer("m", palette = "Dark2")
#        p <- p + scale_linetype("m")
        p <- p + scale_shape("m")
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
                       legend.text = 	element_text(size = 10, colour = "black"),
                       legend.key =  	element_rect(fill = "white", colour = "white"),
                       panel.border = element_rect(color = 'black')
                       )
        
        p <- p + ylab('Bias')
#        p <- p + theme(axis.ticks.x = element_blank())
        p <- p + facet_wrap(~ Effect, scales="free_y")
        
        wave_output_filename <- sub('[.]eps',
                                    paste('-waves', wave, '.eps', sep=''),
                                    output_filename)
        postscript(wave_output_filename, onefile=FALSE,
               paper="special", horizontal=FALSE, width=9, height=6)
        print( p)
        dev.off()
    }
}
