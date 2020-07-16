#!/usr/bin/Rscript
#
# File:    plotFPRwithCIsnowballSinglePanel.R
# Author:  Alex Stivala
# Created: September 2015
#
# $Id: plotFPRwithCIsnowballSinglePanel.R 648 2016-09-27 07:40:15Z stivalaa $
#
# Read multiple IPNet estimation error summary statistics results (generated
# by plotIPNetResults.R) concatenated together from stdin and plot
# false positive rate with confidence interval error bar.
#
# Usage: Rscript plotFPRwithCIsnowballSinglePanel.R  baseline_Stats_Table.txt outfilename.eps EffectName NumWaves
#
# baseline_Stats_table.txt contains the stats (false positive rate etc.)
# for the baseline case i.e. full network, used to plot horizontal line
# with shaded error region for the best possible case (baseline).
# (actually can't get shaded error region to work, using dotted lines instead)
#
# Output file is PostScript file outfilename.eps as specified on command line
# EffectName is name of effect to plot (e.g. Contagion)
# NumWaves is number of waves to plot  (e.g. 3)
#
# This script plots a single panel (facet) for a specified effect and
# numbe of waves.
#  
# WARNING: output file is overwritten if it exists.
#
# Example: cat estimation_n500_bin_cont2_snowball_full_error_statistics.txt | Rscript plotFPRwithCIsnowballSinglePanel.R estimation_n500_bin_cont_baseline_error_statistics.txt   estimation_n500_bin_cont2_snoball_full_false_positive_rate.eps Contagion 1
# 

library(ggplot2)
library(grid)
library(gridExtra)

zSigma <- 1.96 # number of standard deviations for 95% confidence interval

# read in R source file from directory where this script is located
#http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
source_local <- function(fname){
  argv <- commandArgs(trailingOnly = FALSE)
  base_dir <- dirname(substring(argv[grep("--file=", argv)], 8))
  source(paste(base_dir, fname, sep=.Platform$file.sep))
}

source_local('effectNames.R')


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
effect <- args[3]
numWaves <- args[4]


effects <- get_effects(output_filename)
effect_names <- get_effect_names(output_filename)


baseline <- read.table(baseline_filename, header=TRUE, stringsAsFactors=FALSE)
D <- read.table(file("stdin"), header=TRUE, stringsAsFactors=FALSE)

D <- D[which(D$Effect == effect),]
D <- D[which(D$numWaves == numWaves),]

D$numWaves <- factor(D$numWaves)
maxNumSeeds <- max(D$numSeeds)
D$numSeeds <- factor(D$numSeeds)




effect_name <- effect_names[which(effects == effect)]
ebaseline <- baseline[which(baseline$Effect == effect),]
De <- D
De <- De[which(De$ZeroEffect == effect),] # get the false positive stats rows    

print(De)#xxx


p <- ggplot(De, aes(x = numSeeds, y = FPRpercent,
                    colour = as.factor(maxLinks),
                    linetype = as.factor(maxLinks),
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
#p <- p + scale_x_discrete(breaks=seq(0, maxNumSeeds, 10))    
#p <- p + scale_y_continuous(lim=c(0,100))
p <- p + ggtitle(paste(effect_names[which(effects == effect)], '\n',
                       "Number of waves: ", numWaves, sep=''))
                 
p <- p + ylab('Type I error rate %')
p <- p + xlab('Number of seeds')
p <- p + theme(axis.ticks.x = element_blank())
#    p <- p + facet_grid(. ~ numWaves, labeller = label_function)
p <- p + scale_colour_brewer("m", palette = "Dark2")
p <- p + scale_linetype("m")
p <- p + scale_shape("m")






postscript(output_filename, onefile=FALSE,
           paper="special", horizontal=FALSE, width=9, height=6)
print( p )
dev.off()

