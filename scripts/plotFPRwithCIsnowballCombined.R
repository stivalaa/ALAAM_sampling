#!/usr/bin/Rscript
#
# File:    plotFPRwithCIsnowballCombined.R
# Author:  Alex Stivala
# Created: September 2015
#
# $Id: plotFPRwithCIsnowballCombined.R 648 2016-09-27 07:40:15Z stivalaa $
#
# Read multiple IPNet estimation error summary statistics results (generated
# by plotIPNetResults.R) concatenated together from stdin and plot
# false positive rate with confidence interval error bar.
#
# Usage: Rscript plotFPRwithCIsnowballCombined.R  baseline_Stats_Table.txt outfilename.eps
#
# baseline_Stats_table.txt contains the stats (false positive rate etc.)
# for the baseline case i.e. full network, used to plot horizontal line
# with shaded error region for the best possible case (baseline).
# (actually can't get shaded error region to work, using dotted lines instead)
#
#
# This versino plots different sampling types (full snowball, maxlinks limited
# snowball with various maxlinks values) on teh same plot.
#
# If the filenames contains 'colour', then done in colour else grayscale.
#
#
# If filename contains 'landscape' then postscript arranged sideways
# for slides not paper
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


zSigma <- 1.96 # number of standard deviations for 95% confidence interval


args <- commandArgs(trailingOnly=TRUE)
baseline_filename <- args[1]
output_filename <- args[2]



effects <- get_effects(output_filename)
effects <- effects[2:length(effects)]
effect_names <- get_effect_names(output_filename)
effect_names <- effect_names[2:length(effects)]
label_function <- get_label_function(output_filename)


use_colour <- FALSE
if (length(grep("colour", output_filename, fixed=TRUE)) > 0) {
    use_colour <- TRUE
}

landscape <- FALSE
if (length(grep("landscape", output_filename, fixed=TRUE)) > 0) {
    landscape <- TRUE
}

baseline <- read.table(baseline_filename, header=TRUE, stringsAsFactors=FALSE)
D <- read.table(file("stdin"), header=TRUE, stringsAsFactors=FALSE)

baseline$percentRemoved <- NULL
D <- merge(D, baseline, by="Effect", suffixes=c("",".baseline"))

D <- D[which(D$numSeeds <= 20), ] #XXX

D$numWaves <- factor(D$numWaves)
maxNumSeeds <- max(D$numSeeds)
D$numSeeds <- factor(D$numSeeds)

D <- D[which(D$Effect %in% effects), ]
D$Effect <- factor(D$Effect, levels = c(effects))
    

p <- ggplot(D, aes(x = numSeeds, y = FPRpercent,
                    colour = as.factor(maxLinks),
#                    linetype = as.factor(maxLinks),
                    shape = as.factor(maxLinks)))
p <- p + geom_hline(aes(yintercept = FPRpercent.baseline),colour="gray55")
p <- p + geom_hline(aes(yintercept = FPRpercentLower.baseline), linetype="dashed",colour="gray55")
p <- p + geom_hline(aes(yintercept = FPRpercentUpper.baseline), linetype="dashed",colour="gray55")

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

p <- p + scale_x_discrete(breaks=seq(0, maxNumSeeds, 2))    
p <- p + scale_y_continuous(lim=c(0,100))
p <- p + ylab('Type I error rate %')
p <- p + xlab('Number of seeds')
#p <- p + theme(axis.ticks.x = element_blank())
if (use_colour) {
    p <- p + scale_colour_brewer("m", palette = "Dark2")
} else {
    p <- p + scale_colour_grey("m" )
}
#p <- p + scale_linetype("m")
p <- p + scale_shape("m")
if (landscape) {
    p <- p + facet_grid(numWaves ~ Effect, labeller = label_function)
} else {
    p <- p + facet_grid(Effect ~ numWaves, labeller = label_function)
}


## # add plot of snowball sample size
## De <- D[which(D$Effect == effects[1]),] # just use first effect, all the same
## p <- ggplot(De, aes(x = numSeeds, y = meanNumNodes,
##                     colour = as.factor(maxLinks),
##                     linetype = as.factor(maxLinks),
##                     shape = as.factor(maxLinks)))
## p <- p + geom_point()
## p <- p + geom_errorbar(aes(ymax = meanNumNodes + zSigma * sdNumNodes,
##                            ymin = meanNumNodes - zSigma * sdNumNodes))
## p <- p + theme_bw()
## p <- p + theme(panel.background = element_blank(),
##                panel.grid.major = element_blank(),
##                panel.grid.minor = element_blank(),
##                plot.background = element_blank(),
##                strip.background = element_blank(),
##                legend.text = 	element_text(size = 10, colour = "black"),
##                legend.key =  	element_rect(fill = "white", colour = "white"),
##                panel.border = element_rect(color = 'black')
##                )
## p <- p + ggtitle('Snowball sample size')
## p <- p + scale_x_discrete(breaks=seq(0, maxNumSeeds, 2))    
## p <- p + scale_y_continuous(lim=c(0,500)) #XXX
## p <- p + ylab('Nodes in sample')
## p <- p + xlab('Number of seeds')
## p <- p + theme(axis.ticks.x = element_blank())
## p <- p + scale_colour_brewer("k", palette = "Dark2")
## p <- p + scale_linetype("k")
## p <- p + scale_shape("k")
    
## p <- p + facet_grid(. ~ numWaves, labeller = label_function)

if (landscape) {
    postscript(output_filename, onefile=FALSE,
               paper="special", horizontal=FALSE, width=9, height=6)
} else {
    postscript(output_filename, onefile=FALSE,
               paper="special", horizontal=FALSE, width=9, height=12)
}
print(p)
dev.off()

