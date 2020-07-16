#!/usr/bin/Rscript
#
# File:    plotFPRversusSampleSizeCombined.R
# Author:  Alex Stivala
# Created: September 2015
#
# $Id: plotFPRversusSampleSizeCombined.R 648 2016-09-27 07:40:15Z stivalaa $
#
# Read multiple IPNet estimation error summary statistics results (generated
# by plotIPNetResults.R) concatenated together from stdin, as well as
# random node removal (including baseline ie all nodes) from file, and plot
# false positive rate with confidence interval error bar.
#
# Usage: Rscript plotFPRversusSampleSizeCombined.R  removednoderesults.txt outfilename.eps
#
# removednodersults.txt contains the stats (false positive rate etc.)
# for random node removal (inc. baseline).
# Snowball results are read from stdin.
#
#
# If filename contains 'landscape' then postscript arranged sideways
# for slides not paper
#
# This version plots different sampling types (full snowball, maxlinks limited
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

#
# label function for ggplot2 labeller
#
label_function <- function(variable, value) {
    sapply(value, FUN = function(xval) 
        if (variable == 'numWaves') {
            paste('Number of waves:', format(xval))
        }
        else if (variable == 'Effect') {
            switch(format(xval),
                   'Attribute_Density' = 'Density',
                   'Activity' = 'Activity',
                   'Contagion' = 'Contagion',
                   'oOb_for_Attribute1' = 'Binary',
                   'oOc_Continuous_Attribute1' = 'Continuous'
                   )
        }
        else {
            bquote(.(variable) == .(xval))
        }
    )
}


args <- commandArgs(trailingOnly=TRUE)
noderemoval_filename <- args[1]
output_filename <- args[2]

effects <- get_effects(output_filename)
effects <- effects[2:length(effects)] # skip Attribute_Density

landscape <- FALSE
if (length(grep("landscape", output_filename, fixed=TRUE)) > 0) {
    landscape <- TRUE
}
A <- read.table(noderemoval_filename, header=TRUE, stringsAsFactors=FALSE)
D <- read.table(file("stdin"), header=TRUE, stringsAsFactors=FALSE)

A <- A[which(A$Effect %in% effects), ]
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

D <- D[which(D$Effect %in% effects), ]
D <- D[which(D$ZeroEffect == D$Effect),] # get the false positive stats rows    

D$Effect <- factor(D$Effect, levels = c(effects))
    

p <- ggplot(D, aes(x = meanNumNodes, y = FPRpercent,
                    colour = as.factor(maxLinks),
#                    linetype = as.factor(maxLinks),
                    shape = as.factor(maxLinks)))
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
p <- p + geom_hline(aes(yintercept = FPRpercent.baseline),colour="gray55")
p <- p + geom_hline(aes(yintercept = FPRpercentLower.baseline), linetype="dashed",colour="gray55")
p <- p + geom_hline(aes(yintercept = FPRpercentUpper.baseline), linetype="dashed",colour="gray55")
p <- p + geom_point()
p <- p + geom_errorbarh(aes(xmax = meanNumNodes + zSigma * sdNumNodes,
                            xmin = meanNumNodes - zSigma * sdNumNodes))
p <- p + geom_errorbar(aes(ymax = FPRpercentUpper,
                           ymin = FPRpercentLower))

p <- p + geom_point(data = A, aes(x = numNodes, y = FPRpercent))
p <- p + geom_errorbar(data = A, width = 1, 
                        aes(x = numNodes, y = FPRpercent,
                            ymax = FPRpercentUpper, ymin = FPRpercentLower))


p <- p + scale_y_continuous(lim=c(0,100))
p <- p + ylab('Type I error rate %')
p <- p + xlab('Number of nodes in sample')
#p <- p + theme(axis.ticks.x = element_blank())
brew_colours <- brewer.pal(length(levels(as.factor(D$maxLinks))), "Dark2")
p <- p + scale_colour_manual("m", values = c(brew_colours, "black"))
#p <- p + scale_linetype("m")
p <- p + scale_shape_manual("m", values=c(16,17,15,8)) # circle, triangle, square, asterisk
p <- p + facet_grid(Effect ~ numWaves, labeller = label_function)


if (landscape) {
    postscript(output_filename, onefile=FALSE,
               paper="special", horizontal=FALSE, width=9, height=6)
} else {
    postscript(output_filename, onefile=FALSE,
               paper="special", horizontal=FALSE, width=9, height=12)
   
}
print(p)
dev.off()

