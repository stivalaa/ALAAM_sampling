#!/usr/bin/Rscript
#
# File:    plotFPRwithCIsnowball.R
# Author:  Alex Stivala
# Created: September 2015
#
# $Id: plotFPRwithCIsnowball.R 648 2016-09-27 07:40:15Z stivalaa $
#
# Read multiple IPNet estimation error summary statistics results (generated
# by plotIPNetResults.R) concatenated together from stdin and plot
# false positive rate with confidence interval error bar.
#
# Usage: Rscript plotFPRwithCIsnowball.R  baseline_Stats_Table.txt outfilename.eps
#
# baseline_Stats_table.txt contains the stats (false positive rate etc.)
# for the baseline case i.e. full network, used to plot horizontal line
# with shaded error region for the best possible case (baseline).
# (actually can't get shaded error region to work, using dotted lines instead)
#
# Output file is PostScript file outfilename.eps as specified on command line
#
# This script now used to plot specifically for waves = 1 as theat has more max seeds (100 not 20) 
#  
# WARNING: output file is overwritten if it exists.
#
# Example: cat estimation_n500_bin_cont2_snowball_full_error_statistics.txt | Rscript plotFPRwithCIsnowball.R estimation_n500_bin_cont_baseline_error_statistics.txt   estimation_n500_bin_cont2_snoball_full_false_positive_rate.eps
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


zSigma <- 1.96 # number of standard deviations for 95% confidence interval


# label function for ggplot2 labeller
#
label_function <- function(variable, value) {
    sapply(value, FUN = function(xval) 
        if (variable == 'numWaves') {
            paste('Number of waves:', format(xval))
        }
        else {
            bquote(.(variable) == .(xval))
        }
    )
}


args <- commandArgs(trailingOnly=TRUE)
baseline_filename <- args[1]
output_filename <- args[2]

effects <- get_effects(output_filename)
effect_names <- get_effect_names(output_filename)

baseline <- read.table(baseline_filename, header=TRUE, stringsAsFactors=FALSE)
D <- read.table(file("stdin"), header=TRUE, stringsAsFactors=FALSE)

D <- D[which(D$numWaves == 1),]  # only waves = 1

D$numWaves <- factor(D$numWaves)
maxNumSeeds <- max(D$numSeeds)
D$numSeeds <- factor(D$numSeeds)

plotlist <- list()
for (i in 1:length(effects)) {
    effect <- effects[i]
    effect_name <- effect_names[i]
    ebaseline <- baseline[which(baseline$Effect == effect),]
    De <- D[which(D$Effect == effect),]
    De <- De[which(De$ZeroEffect == effect),] # get the false positive stats rows    

    print(De)#xxx

    if (length(De$Effect) == 0) {
        cat("skipping effect ", effect, " not present in data\n")
        # put an empty plot in list at position where this effect would
        # be, to make position of effect plot in grid same, easier to compare
        emptyplot <- ggplot(data.frame()) + geom_blank() +theme(panel.background=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), plot.background=element_blank(),strip.background=element_blank(),axis.ticks=element_blank(), axis.title=element_blank(), axis.text=element_blank()) +xlim(0,1)+ylim(0,1) +annotate("text",label=paste("no", effect_names[i], "parameter",sep=' '), x=0.5,y=0.5,colour="grey")
        plotlist <- c(plotlist, list(emptyplot))
        next
    }

    p <- ggplot(De, aes(x = numSeeds, y = FPRpercent,
                        colour = as.factor(maxLinks),
#                        linetype = as.factor(maxLinks),
                        shape = as.factor(maxLinks)))
    p <- p + geom_hline(yintercept = ebaseline$FPRpercent,colour="gray55")
    p <- p + geom_hline(yintercept = ebaseline$FPRpercentLower, linetype="dashed",colour="gray55")
    p <- p + geom_hline(yintercept = ebaseline$FPRpercentUpper, linetype="dashed",colour="gray55")
    p <- p + geom_point()
    p <- p + geom_errorbar(aes(ymax = FPRpercentUpper,
                               ymin = FPRpercentLower))
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
    p <- p + scale_x_discrete(breaks=seq(0, maxNumSeeds, 10))    
    p <- p + scale_y_continuous(lim=c(0,100))
    p <- p + ggtitle(effect_names[i])
    p <- p + ylab('Type I error rate %')
    p <- p + xlab('Number of seeds')
    p <- p + theme(axis.ticks.x = element_blank())
#    p <- p + facet_grid(. ~ numWaves, labeller = label_function)
    p <- p + scale_colour_brewer("m", palette = "Dark2")
#    p <- p + scale_linetype("m")
    p <- p + scale_shape("m")
    
    plotlist <- c(plotlist, list(p))
}

# add plot of snowball sample size
De <- D[which(D$Effect == effects[1]),] # just use first effect, all the same
p <- ggplot(De, aes(x = numSeeds, y = meanNumNodes,
                        colour = as.factor(maxLinks),
#                        linetype = as.factor(maxLinks),
                        shape = as.factor(maxLinks)))
p <- p + geom_point()
p <- p + geom_errorbar(aes(ymax = meanNumNodes + zSigma * sdNumNodes,
                           ymin = meanNumNodes - zSigma * sdNumNodes))
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
p <- p + ggtitle('Snowball sample size')
p <- p + scale_x_discrete(breaks=seq(0, maxNumSeeds, 10))    
#p <- p + scale_y_continuous(lim=c(0,500)) #XXX
p <- p + ylab('Nodes in sample')
p <- p + xlab('Number of seeds')
p <- p + theme(axis.ticks.x = element_blank())
#p <- p + facet_grid(. ~ numWaves, labeller = label_function)
p <- p + scale_colour_brewer("m", palette = "Dark2")
#p <- p + scale_linetype("m")
p <- p + scale_shape("m")


plotlist <- c(plotlist, list(p))


postscript(output_filename, onefile=FALSE,
           paper="special", horizontal=FALSE, width=9, height=12)
do.call(grid.arrange, plotlist)
dev.off()

