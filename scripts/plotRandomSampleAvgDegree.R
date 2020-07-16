#!/usr/bin/Rscript
#
# File:    plotRandomSampleAvgDegree.R
# Author:  Alex Stivala
# Created: August 2016
#
# $Id: plotRandomSampleAvgDegree.R 608 2016-08-31 01:25:29Z stivalaa $
#
# Do random sampling in a network (read as adjacency matrix, or from
# Nexus) do boxplots of mean and median degree, for varying values of number of
# seeds and waves
#
#
#
# Usage: Rscript plotRandomSampleAvgDegree.R {adjmatrixfile | nexus:nexusname | gml:gmlfilename}
#
#  adjmatrixfile is adjacency matrix for entwork to sample
#  nexus:nexusname instead reads network nexusname from the Nexus repository
#  gml:gmlfilename instaed reaads networks from GML file
#
# Output files: networkbasename-randomsamplemeandegreeboxplots.eps
#               networkbasename-randomsamplemediandegreeboxplots.eps
#               networkbasename-randomsamplemaxdegreeboxplots.eps


library(igraph)
library(ggplot2)
library(reshape) 
library(grid)
library(gridExtra)

# read in R source file from directory where this script is located
#http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
source_local <- function(fname){
  argv <- commandArgs(trailingOnly = FALSE)
  base_dir <- dirname(substring(argv[grep("--file=", argv)], 8))
  source(paste(base_dir, fname, sep=.Platform$file.sep))
}

source_local('readFiles.R')


#
# label function for ggplot2 labeller
#
label_function <- function(variable, value) {
    sapply(value, FUN = function(xval) 
        if (variable == 'num_waves') {
            paste('Number of waves:', format(xval))
        }
        else if (variable == 'max_links') {
            paste('Max. links followed:', format(xval))
        }
        else {
            bquote(.(variable) == .(xval))
        }
           
    )
}

# 
# main
#

num_samples <- 100


args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 1) {
  cat("Usage: Rscript snowballSampleSizes.R {graphmatrix_filename.txt |  nexus:nexusname | gml:gmlfilename}\n")
  quit(save="no")
}

if (substr(args[1], 1, 6) == 'nexus:') {
    # download network from Nexus
    network_name <- substr(args[1], 7, nchar(args[1]))
    cat('Reading Nexus network ', network_name, '\n')
    basefilename <- network_name
    g <- nexus.get(network_name)
} else if (substr(args[1], 1, 4) == 'gml:') {
    gmlfilename <- substr(args[1], 5, nchar(args[1]))
    cat('Reading GML file ', gmlfilename, '\n')
    basefilename <- sub("(.+)[.].+", "\\1", basename(gmlfilename))
    g <- simplify(read.graph(gmlfilename, format='gml'))
} else {
    # read adjacency matrix from file
    graphmatrix_filename <- args[1]
    basefilename <- sub("(.+)[.].+", "\\1", basename(graphmatrix_filename))

    g <- read_graph_matrix_file(graphmatrix_filename,directed=FALSE)
}

summary(g) #xxx

# label each node with 'name' attribute that is preserved by sampling
V(g)$name <- 1:vcount(g)


D <- NULL

cat("generating random samples...")
if (is.directed(g)) {
    cat("(digraph)")
}
cat("\n")

for (percentRemoved in seq(0, 90, 10)) {

    
    if (is.directed(g)) {
        stopifnot(FALSE) # directed not supported yet
    } else{
        keepCount = as.integer(((100 - percentRemoved)/100)*vcount(g))
        samples <- lapply(1:num_samples,
                          function(i) induced.subgraph(g,
                          V(g)[sample.int(vcount(g), keepCount, replace=FALSE)]))
    }
    
    # collect data on degree distribution
    Dn <- data.frame(num_nodes = rep(keepCount, num_samples),
                     max_degree = sapply(samples,
                         function(g) max(degree(g))),
                     mean_degree = sapply(samples,
                         function(g) mean(degree(g))),
                     median_degree = sapply(samples,
                         function(g) median(degree(g)))
                     )
    D <- rbind(D, Dn)

    ## print(percentRemoved)#XXX
    ## print(keepCount)#XXX
    ## print(Dn)#XXX
}



D <- melt(D, id = c('num_nodes'))
D$num_nodes <- factor(D$num_nodes)

#print(D)#XXX

De <- D[which(D$variable == 'mean_degree'),]
p <- ggplot(De, aes(x =  num_nodes ,  y = value))
p <- p + theme_bw()
p <- p + theme(panel.background = element_blank(),
               ## panel.grid.major = element_blank(),
               ## panel.grid.minor = element_blank(),
               plot.background = element_blank(),
               strip.background = element_blank(),
               panel.border = element_rect(color = 'black')
               )
p <- p + geom_boxplot()
p <- p + xlab('Number of nodes')
p <- p + ylab('Mean degree')
p <- p + geom_hline(aes(yintercept = mean(degree(g))),colour="gray55")
postscript(paste(basefilename, '-randomsamplemeandegreeboxplots.eps',sep=''),
           onefile=FALSE, paper='special',horizontal=FALSE,width=9,height=6)
print(p)
dev.off()

De <- D[which(D$variable == 'median_degree'),]
p <- ggplot(De, aes(x =  num_nodes ,  y = value))
p <- p + theme_bw()
p <- p + theme(panel.background = element_blank(),
               ## panel.grid.major = element_blank(),
               ## panel.grid.minor = element_blank(),
               plot.background = element_blank(),
               strip.background = element_blank(),
               panel.border = element_rect(color = 'black')
               )
p <- p + geom_boxplot()
p <- p + xlab('Number of nodes')
p <- p + ylab('Median degree')
p <- p + geom_hline(aes(yintercept = median(degree(g))), colour="gray55")
postscript(paste(basefilename, '-randomsamplemediandegreeboxplots.eps',sep=''),
           onefile=FALSE, paper='special',horizontal=FALSE,width=9,height=6)
print(p)
dev.off()

De <- D[which(D$variable == 'max_degree'),]
p <- ggplot(De, aes(x =  num_nodes ,  y = value))
p <- p + theme_bw()
p <- p + theme(panel.background = element_blank(),
               ## panel.grid.major = element_blank(),
               ## panel.grid.minor = element_blank(),
               plot.background = element_blank(),
               strip.background = element_blank(),
               panel.border = element_rect(color = 'black')
               )
p <- p + geom_boxplot()
p <- p + xlab('Number of nodes')
p <- p + ylab('Max. degree')
p <- p + geom_hline(aes(yintercept = max(degree(g))), colour="gray55")
postscript(paste(basefilename, '-randomsamplemaxdegreeboxplots.eps',sep=''),
           onefile=FALSE, paper='special',horizontal=FALSE,width=9,height=6)
print(p)
dev.off()

