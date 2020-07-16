#!/usr/bin/Rscript
#
# File:    plotPNetSimulationDiagnostics.R
# Author:  Alex Stivala
# Created: February 2014
#
# Similarly to the SPSS script genreated by PNet simulation or GoF, plot
# scatterplot to show autocorrelation in samples and histograms of network
# statisics, for use on UNIX version instead of the SPSS script.
#
#
# Usage: Rscript plotPNetSimulationDiagnostics.R simulation_pnet_output_.txt
#
# e.g.: Rscript plotPNetSimulationDiagnostics.R simulation_netscience_gof.txt
#
# Output is postscrpt file basename.eps where basename is from the input
# file e.g. simulation_netsceince_gof-plots.eps
#
# NB uses pipe() to run UNIX commands, since only for UNIX anyway.
#
library(grid)
library(gridExtra)
library(ggplot2)
library(reshape)

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 1) {
    cat("Usage: Rscript plotPNetSimulationDiagnostics.R simulation_stats.txt\n")
    quit(save='no')
}
simstats_filename <- args[1]
basefilename <- sub("(.+)[.].+", "\\1", basename(simstats_filename))

numfields <- scan(pipe(paste("head -1", simstats_filename, "| awk '{print NF}'")))

simstats <- read.table(pipe(paste("cut -f1-",format(numfields)," ",simstats_filename,sep="")), header=TRUE)

statnames <- names(simstats)[names(simstats) != "id"]

simstats <- melt(simstats, id=c('id'))
plotlist <- list()
for (statname in statnames) {
    simstats_statname <- simstats[which(simstats$variable == statname),]

    p <- ggplot(simstats_statname, aes(x=id, y=value))
    p <- p + geom_point()
    p <- p + xlab('sample')
    p <- p + ylab(statname)
    plotlist <- c(plotlist, list(p))

    p <- ggplot(simstats_statname, aes(x=value))
    p <- p + geom_histogram()
    p <- p + xlab(statname)
    plotlist <- c(plotlist, list(p))
}

postscript(paste(basefilename, '-plots.eps', sep=''), onefile=FALSE,
           paper="special", horizontal=FALSE, width=9, height=6)
do.call(grid.arrange, plotlist)
dev.off()

