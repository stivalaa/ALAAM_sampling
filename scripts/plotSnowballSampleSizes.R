#!/usr/bin/Rscript
#
# File:    plotSnowballSampleSizes.R
# Author:  Alex Stivala
# Created: February 2014
#
# Do snowball sampling in a network (read as adjacency matrix, or from
# Nexus) do boxplots of sample sizes, for varying values of number of
# seeds and waves
#
#
# Usage: Rscript plotSnowballSampleSizes.R {adjmatrixfile | nexus:nexusname | gml:gmlfilename}
#
#  adjmatrixfile is adjacency matrix for entwork to sample
#  nexus:nexusname instead reads network nexusname from the Nexus repository
#  gml:gmlfilename instaed reaads networks from GML file
#
# Output files: networkbasename-snowballsamplesizeboxplots.eps
#               networkbasename-snowballinnerwavessamplesizeboxplots.eps


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
source_local('snowballSample.R')



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


max_num_waves <- 3 
max_num_seeds <- 50
max_links_values <- c(Inf, 5, 4, 3)

D <- NULL

cat("generating snowball samples...")
if (is.directed(g)) {
    cat("(digraph)")
}
cat("\n")

for (max_links in max_links_values) {
  for (num_waves in seq(1, max_num_waves)) {
    for (num_seeds in seq(1, max_num_seeds)) {

        # get matrix where each row is random seed set for one sample
        seedsets <- matrix(sample.int(vcount(g), num_seeds * num_samples, replace=T),
                        nrow = num_samples);


        if (is.directed(g)) {
            stopifnot(FALSE) # directed not supported yet
            ## samples <- lapply(1:num_samples,
            ##                   function(i) snowball_sample_from_digraph(g, num_waves, seedsets[i,]))            
        } else{
            samples <- lapply(1:num_samples,
                              function(i) snowball_sample(g, num_waves,
                                                          seedsets[i,],
                                                          max_links))
            inner_waves_samples <- lapply(samples,
                   function(g) induced.subgraph(g, V(g)[V(g)$zone < num_waves]))
        }


        nodevectors <- sapply(samples, function(g) V(g)$name)

        nodenames_set <- unique(unlist(nodevectors))


        # collect data on number of nodes in each sample
        Dn <- data.frame(num_seeds = rep(num_seeds, num_samples),
                        num_waves = rep(num_waves, num_samples),
                        max_links = rep(max_links, num_samples),
                        num_nodes_in_sample = sapply(samples,
                                                    function(g) vcount(g)),
                        num_nodes_in_inner_waves = sapply(inner_waves_samples,
                            function(g) vcount(g))
                         )
        D <- rbind(D, Dn)

    }
  }
}

D$sample_percentage <-  100* D$num_nodes_in_sample / vcount(g)
D$inner_wave_percentage <- 100*D$num_nodes_in_inner_waves / vcount(g)

D <- melt(D, id = c('num_seeds', 'num_waves', 'max_links'))
D$num_seeds <- factor(D$num_seeds)
D$num_waves <- factor(D$num_waves)
D$max_links <- factor(D$max_links)

De <- D[which(D$variable == 'sample_percentage'),]
p <- ggplot(De, aes(x =  num_seeds ,  y = value))
p <- p + theme_bw()
p <- p + theme(panel.background = element_blank(),
               ## panel.grid.major = element_blank(),
               ## panel.grid.minor = element_blank(),
               plot.background = element_blank(),
               strip.background = element_blank(),
               panel.border = element_rect(color = 'black')
               )
p <- p + scale_x_discrete(breaks=seq(0, max_num_seeds, 5))
p <- p + geom_boxplot()
p <- p + xlab('Number of seeds')
p <- p + ylab('Nodes in sample %')
p <- p + facet_grid(max_links ~ num_waves, labeller = label_function)
postscript(paste(basefilename, '-snowballsamplesizeboxplots.eps',sep=''),
           onefile=FALSE, paper='special',horizontal=FALSE,width=9,height=6)
print(p)
dev.off()

De <- D[which(D$variable == 'inner_wave_percentage'),]
p <- ggplot(De, aes(x =  num_seeds ,  y = value))
p <- p + theme_bw()
p <- p + theme(panel.background = element_blank(),
               ## panel.grid.major = element_blank(),
               ## panel.grid.minor = element_blank(),
               plot.background = element_blank(),
               strip.background = element_blank(),
               panel.border = element_rect(color = 'black')
               )
p <- p + scale_x_discrete(breaks=seq(0, max_num_seeds, 5))
p <- p + geom_boxplot()
p <- p + xlab('Number of seeds')
p <- p + ylab('Nodes in sample inner waves %')
p <- p + facet_grid(max_links ~ num_waves, labeller = label_function)
postscript(paste(basefilename, '-snowballinnerwavessamplesizeboxplots.eps',sep=''),
           onefile=FALSE, paper='special',horizontal=FALSE,width=9,height=6)
print(p)
dev.off()

