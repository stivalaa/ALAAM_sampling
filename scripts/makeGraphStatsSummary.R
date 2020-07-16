#!/usr/bin/Rscript
#
# File:    makeGraphStatsSummary.R
# Author:  Alex Stivala
# Created: January 2015
#
# Read in all the simulated graph samples and produced descriptive tatistics
# (components, mean degree, density, global clustering coefficient)
#
#
# Usage:
# 
# Rscript makeGraphStatsSummary.R
#

library(igraph)

# read in R source file from directory where this script is located
#http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
source_local <- function(fname){
  argv <- commandArgs(trailingOnly = FALSE)
  base_dir <- dirname(substring(argv[grep("--file=", argv)], 8))
  source(paste(base_dir, fname, sep=.Platform$file.sep))
}

source_local('readFiles.R')


# 
# main
#

graphs <- c("../simulated_n500_bin_cont2/n500_kstar_simulate12750000_matrix.txt",
            "../simulated_n1000_bin_cont/n1000_kstar_simulate12750000_matrix.txt",
            "../simulated_Project90/project90_giantcomponent_adjmatrix.txt",
            "../simulated_AddHealth/addhealth_adjmatrix.txt",
            "../simulated_n1000_powerlaw/n1000_powerlaw_matrix.txt",
            "../simulated_n1000_powerlawtri/n1000_powerlawtri_matrix.txt")

# glob patterns of outcome (attribute) files must line up with above
attrglobs <- c("../simulated_n500_bin_cont2/sample-n500_bin_cont*.clu",
               "../simulated_n1000_bin_cont/sample-n1000_bin_cont*.clu",
               "../simulated_Project90/sample-project90_*.clu",
               "../simulated_AddHealth/sample-addhealth_*.clu",
            "../simulated_n1000_powerlaw/sample-n1000_powerlaw*.clu",
            "../simulated_n1000_powerlawtri/sample-n1000_powerlawtri*.clu")

# description (must line up with above)
descrs <- c("ERGM", "ERGM", "Project 90", "Add Health", "B-A", "B-A $m=4$")

cat('\\begin{tabular}{lrrrrrrrr}\n')
cat('\\hline\n')
cat('Network & N  &   Components &  Mean  & Max.      &    Density & Clustering & \\multicolumn{2}{c}{Positive outcome \\%}\\\\\n')
cat('        &     &             &   degree       &     degree     &            &  coefficient          & mean & s.d.             \\\\\n')
cat('\\hline\n')
for (i in 1:length(graphs)) {
    g <- read_graph_matrix_file(graphs[i], directed=FALSE)
    nodecount <- vcount(g)
    components <-  length(decompose.graph(g))
    meandegree <-  mean(degree(g))
    maxdegree <- max(degree(g))
    density <-  graph.density(g)
    cc <- transitivity(g, type="global")

    outcomes <- lapply(Sys.glob(attrglobs[i]), 
                       FUN = function(f) read_outcome_file(f))
    positivecount <- sapply(outcomes, sum)
    pospercent <- 100*as.vector(positivecount) / nodecount
    cat(descrs[i], nodecount, format(components, digits=3, nsmall=2),
        format(meandegree, digits=3, nsmall=2),
        maxdegree,
        format(round(density, digits=5), digits=6, nsmall=5),
        format(round(cc, digits=5), digits=6, nsmall=5), 
        format(mean(pospercent), digits=2, nsmall=0),
        format(sd(pospercent), digits=3, nsmall=2),
        sep=' & ')
    cat('\\\\\n')
}
cat('\\hline\n')
cat('\\end{tabular}\n')
