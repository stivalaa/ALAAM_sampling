#!/usr/bin/Rscript
#
# File:    visualizeNetworkRandomSampleFromNexus.R
# Author:  Alex Stivala
# Created: March 2016
#
# 
# Do random node sampling a (large) network from the Nexus igraph
# repository, and plot network visualiation of the induced subgraph.
#
# The graph may be directed or undirected. 
#
#
# Input graph is a named graph from the Nexus igraph repository
# (nexus.igraph.org) e.g. "condmatcollab2005"
#
# Output file is PNG file of network plot.
#
# Usage:
# 
# Rscript visualizeNetworkRandomSampleFromNexus.R nexus_name num_nodes output_ps_file
#
#    nexus_name is name of a graph in the Nexus repository e.g. condmatcollab2005
#    num_nodes is number of nodes to sample
#    output_ps_file is filename of PNG file to write
#        (WARNING: overwritten)
#

library(igraph)

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 3) {
  cat("Usage:  Rscript visualizeNetworkRandomSampleFromNexus.R nexus_name num_nodes output_ps_file\n")
  quit(save="no")
}
nexus_graph_name <- args[1]
num_nodes <- as.integer(args[2])
output_filename <- args[3]


cat("reading graph ", nexus_graph_name, " from Nexus...\n")
print(system.time(g <- simplify(nexus.get(nexus_graph_name),
                                remove.multiple=T, remove.loops=T)))
print(g) # print info about graph
stopifnot(num_nodes <= vcount(g))

keepNodeNums <- sample.int(vcount(g), num_nodes, replace=FALSE)
g <- induced.subgraph(g, V(g)[keepNodeNums])
print(g) # print info about graph

png(file=output_filename)

colours <- 'blue'

plot(g, layout=layout.fruchterman.reingold, vertex.size=5,
     vertex.label=NA,
     edge.width = 2,
     edge.color = "black",
     edge.arrow.size = 0.5,
     vertex.color = colours,
     vertex.frame.color = colours)

dev.off()

