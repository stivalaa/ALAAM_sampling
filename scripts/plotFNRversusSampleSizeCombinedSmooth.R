#!/usr/bin/Rscript
#
# File:    plotFNRversusSampleSizeCombinedSmooth.R
# Author:  Alex Stivala
# Created: September 2015
#
# $Id: plotFNRversusSampleSizeCombinedSmooth.R 648 2016-09-27 07:40:15Z stivalaa $
#
# Read multiple IPNet estimation error summary statistics results (generated
# by plotIPNetResults.R) concatenated together from stdin, as well as
# random node removal (including baseline ie all nodes) from file, and plot
# false negative rate with confidence interval error bar.
#
# Usage: Rscript plotFNRversusSampleSizeCombinedSmooth.R  removednoderesults.txt outfilename.eps
#
# removednodersults.txt contains the stats (false negative rate etc.)
# for random node removal (inc. baseline).
# Snowball results are read from stdin.
#
# This version plots different sampling types (full snowball, maxlinks limited
# snowball with various maxlinks values) on teh same plot
# and does loess smoothing instead of showing data points.
#
# If the filenames contains 'colour', then done in colour else grayscale.
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
library(RColorBrewer)


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
noderemoval_filename <- args[1]
output_filename <- args[2]



effects <- get_effects(output_filename)
#effect_names <- get_effect_names(output_filename)
label_function <- get_label_function(output_filename)

use_colour <- FALSE
if (length(grep("colour", output_filename, fixed=TRUE)) > 0) {
    use_colour <- TRUE
}

landscape <- FALSE
if (length(grep("landscape", output_filename, fixed=TRUE)) > 0) {
    landscape <- TRUE
}


A <- read.table(noderemoval_filename, header=TRUE, stringsAsFactors=FALSE)
D <- read.table(file("stdin"), header=TRUE, stringsAsFactors=FALSE)

# FIXME dodgy use of output filename to get original network size
if (length(grep('project90', output_filename)) > 0) {
    A$numNodes <- (1-(A$percentRemoved/100)) * 4430 #  only works for N=4430
} else if (length(grep('addhealth_', output_filename)) > 0) {
    A$numNodes <- (1-(A$percentRemoved/100)) * 2539 #  only works for N=2539
} else if (length(grep('n1000_', output_filename)) > 0) {
    A$numNodes <- (1-(A$percentRemoved/100)) * 1000 #  only works for N=1000
} else {
    A$numNodes <- (1-(A$percentRemoved/100)) * 500 #  only works for N=500
}
A$maxLinks <- 'Simple\nrandom\nsampling'

baseline <- A[which(A$percentRemoved == 0),]
baseline$percentRemoved <- NULL

D <- merge(D, baseline, by="Effect", suffixes=c("",".baseline"))


D$numWaves <- factor(D$numWaves)
maxNumSeeds <- max(D$numSeeds)
D$numSeeds <- factor(D$numSeeds)


cat(unique(D$Effect),'\n')#XXX

# hack to try to make it work like it used to, apparently whatever
# version I am using now ignore the factor levels even when explicitly
# specified, and orders factes alphabetically, so put 'aaa' in front
# of the one I want first. 
# Why is R alway so dificult and aribtrary ?!
A$Effect[which(A$Effect == 'Attribute_Density')] <- 'aaaAttribute_Density'
D$Effect[which(D$Effect == 'Attribute_Density')] <- 'aaaAttribute_Density'
effects[which(effects == 'Attribute_Density')] <- 'aaaAttribute_Density'
D$Effect <- factor(D$Effect,  effects)


cat(effects,'\n')#XXX
#cat(D$Effect)#XXX

# this does not work either:
# try to make ggplot2 facet work like it did before in last version I was
# using instead of now apparently reordering it alphatbietically despite
# my explict factor levels (why is everything always so hard in R?!)
#https://github.com/tidyverse/ggplot2/issues/2257
#effect_factor <- function(x) {
#  factor(x, effects)
#}
#D$Effect <- effect_factor(D$Effect)


p <- ggplot(D, aes(x = meanNumNodes, y = FNRpercent,
                    colour = as.factor(maxLinks),
#                    linetype = as.factor(maxLinks),
#                    shape = as.factor(maxLinks)))
                   ))
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
p <- p + geom_hline(aes(yintercept = FNRpercent.baseline),colour="gray55")
p <- p + geom_hline(aes(yintercept = FNRpercentLower.baseline), linetype="dashed",colour="gray55")
p <- p + geom_hline(aes(yintercept = FNRpercentUpper.baseline), linetype="dashed",colour="gray55")
## p <- p + geom_point()
## p <- p + geom_errorbarh(aes(xmax = meanNumNodes + zSigma * sdNumNodes,
##                             xmin = meanNumNodes - zSigma * sdNumNodes))
## p <- p + geom_errorbar(aes(ymax = FNRpercentUpper,
##                            ymin = FNRpercentLower))
p <- p + geom_smooth(method="loess", se=FALSE)

p <- p + geom_point(data = A, aes(x = numNodes, y = FNRpercent))
p <- p + geom_errorbar(data = A, width = 1, 
                        aes(x = numNodes, y = FNRpercent,
                            ymax = FNRpercentUpper, ymin = FNRpercentLower))
## p <- p + geom_smooth(data = A, aes(x = numNodes, y = FNRpercent),
##                      method="loess", se=FALSE)

p <- p + scale_y_continuous(lim=c(0,100))
p <- p + ylab('Type II error rate %')
p <- p + xlab('Number of nodes in sample')
#p <- p + theme(axis.ticks.x = element_blank())
if (use_colour) {
    brew_colours <- brewer.pal(length(levels(as.factor(D$maxLinks))), "Dark2")
} else {
    brew_colours <- brewer.pal(length(levels(as.factor(D$maxLinks))), "Greys")
}
p <- p + scale_colour_manual("m", values = c(brew_colours, "black"))
#p <- p + scale_linetype("m")
#p <- p + scale_shape_manual("m", values=c(16,17,15,8)) # circle, triangle, square, asterisk
p <- p + guides(colour = guide_legend(override.aes = list(shape=list(32,32,32,16)))) # no shape for snowball loess lines, filled circle for SRS
#p <- p + facet_grid(Effect ~ numWaves)#XXX
p <- p + facet_grid(Effect ~ numWaves, labeller = label_function)


if (landscape) {
    postscript(output_filename, onefile=FALSE, horizontal=FALSE,
               paper="special",  width=8, height=6)
} else {
    postscript(output_filename, onefile=FALSE, horizontal=FALSE,
               paper="special",  width=9, height=12)    
}
print(p)
dev.off()

