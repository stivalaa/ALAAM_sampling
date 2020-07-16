#!/usr/bin/Rscript
#
# File:    plotCombinedSnowballSampleSizes.R
# Author:  Alex Stivala
# Created: November 2015
#
# $Id: plotCombinedSnowballSampleSizes.R 572 2016-08-23 06:16:47Z stivalaa $
#
# Read multiple IPNet estimation error summary statistics results (generated
# by plotIPNetResults.R) concatenated together from stdin and plot
# snowball sample size with 95% CI error bar.
#
# Usage: Rscript plotCombinedSnowballSampleSizes.R  outfilename.eps
#
#
# This version plots different sampling types (full snowball, maxlinks limited
# snowball with various maxlinks values) on teh same plot.
# If the filenames contains 'colour', then done in colour else grayscale.
#
# Output file is PostScript file outfilename.eps as specified on command line
#
#  
# WARNING: output file is overwritten if it exists.
#


library(ggplot2)
library(grid)
library(gridExtra)


zSigma <- 1.96 # number of standard deviations for 95% confidence interval

#
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
output_filename <- args[1]

use_colour <- FALSE
if (length(grep("colour", output_filename, fixed=TRUE)) > 0) {
    use_colour <- TRUE
}

D <- read.table(file("stdin"), header=TRUE, stringsAsFactors=FALSE)

D <- D[which(D$numSeeds <= 20), ] #XXX

D$numWaves <- factor(D$numWaves)
maxNumSeeds <- max(D$numSeeds)
D$numSeeds <- factor(D$numSeeds)

D <- D[which(D$Effect == "Activity"), ] # only get one effect, size duplicated for each
    

p <- ggplot(D, aes(x = numSeeds, y = meanNumNodes,
                    colour = as.factor(maxLinks),
#                    linetype = as.factor(maxLinks),
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

p <- p + scale_x_discrete(breaks=seq(0, maxNumSeeds, 2))    
p <- p + ylab('Nodes in sample')
p <- p + xlab('Number of seeds')
#p <- p + theme(axis.ticks.x = element_blank())
if (use_colour) {
    p <- p + scale_colour_brewer("m", palette = "Dark2")
} else {
    p <- p + scale_colour_grey("m")
}
#p <- p + scale_linetype("m")
p <- p + scale_shape("m")
p <- p + facet_grid( . ~ numWaves, labeller = label_function)
#p <- p + facet_grid( numWaves ~ . , labeller = label_function)
#p <- p + facet_wrap(~ numWaves )




postscript(output_filename, onefile=FALSE,
           paper="special", horizontal=FALSE, width=9, height=3)
print(p)
dev.off()

