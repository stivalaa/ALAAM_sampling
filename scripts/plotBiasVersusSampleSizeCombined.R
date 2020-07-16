#!/usr/bin/Rscript
#
# File:    plotBiasversusSampleSizeCombined.R
# Author:  Alex Stivala
# Created: September 2015
#
# $Id: plotBiasVersusSampleSizeCombined.R 706 2018-07-23 05:07:18Z stivalaa $
#
# Read multiple IPNet estimation error summary statistics results (generated
# by plotIPNetResults.R) concatenated together from stdin, as well as
# random node removal (including baseline ie all nodes) from file, and plot
# false negative rate with confidence interval error bar.
#
# Usage: Rscript plotBiasversusSampleSizeCombined.R  removednoderesults.txt outfilename.eps [minSampleSize]
#
# removednodersults.txt contains the stats (false negative rate etc.)
# for random node removal (inc. baseline).
# Snowball results are read from stdin.
#
# This version plots different sampling types (full snowball, maxlinks limited
# snowball with various maxlinks values) on the same plot, optionally
# with a minimum sample size to included specified (default 100 nodes).
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
if (length(args) > 2) {
  minSampleSize <- as.integer(args[3])
} else {
  minSampleSize <- 100
}


effects <- get_effects(output_filename)
effect_names <- get_effect_names(output_filename)
label_function <- get_label_function(output_filename)


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

# Exclude very small samples since huge bias then makes plots unreadable
A <- A[which(A$numNodes >= minSampleSize), ]
D <- D[which(D$meanNumNodes >= minSampleSize), ]

baseline <- A[which(A$percentRemoved == 0),]
baseline$percentRemoved <- NULL
D <- merge(D, baseline, by="Effect", suffixes=c("",".baseline"))



D$numWaves <- factor(D$numWaves)
maxNumSeeds <- max(D$numSeeds)
D$numSeeds <- factor(D$numSeeds)

D$Effect <- factor(D$Effect, levels = c(effects))
    

p <- ggplot(D, aes(x = meanNumNodes, y = Bias,
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
p <- p + geom_hline(aes(yintercept = Bias.baseline),colour="gray55")
p <- p + geom_hline(aes(yintercept = Bias_lower.baseline), linetype="dashed",colour="gray55")
p <- p + geom_hline(aes(yintercept = Bias_upper.baseline), linetype="dashed",colour="gray55")
p <- p + geom_point()
p <- p + geom_errorbarh(aes(xmax = meanNumNodes + zSigma * sdNumNodes,
                            xmin = meanNumNodes - zSigma * sdNumNodes))
p <- p + geom_errorbar(aes(ymax = Bias_upper,
                           ymin = Bias_lower))

p <- p + geom_point(data = A, aes(x = numNodes, y = Bias))
p <- p + geom_errorbar(data = A, width = 1, 
                        aes(x = numNodes, y = Bias,
                            ymax = Bias_upper, ymin = Bias_lower))


p <- p + ylab('Bias')
p <- p + xlab('Number of nodes in sample')
#p <- p + theme(axis.ticks.x = element_blank())
brew_colours <- brewer.pal(length(levels(as.factor(D$maxLinks))), "Dark2")
p <- p + scale_colour_manual("m", values = c(brew_colours, "black"))
#p <- p + scale_linetype("m")
p <- p + scale_shape_manual("m", values=c(16,17,15,8)) # circle, triangle, square, asterisk
p <- p + facet_grid(Effect ~ numWaves, labeller = label_function, 
                    scales = "free_y")



postscript(output_filename, onefile=FALSE,
           paper="special", horizontal=FALSE, width=9, height=12)
print(p)
dev.off()

