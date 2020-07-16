# 
# File:    readAddHealthData.R
# Author:  Alex Stivala
# Created: Augstu 2016
#
# $Id: readAddHealthData.R 572 2016-08-23 06:16:47Z stivalaa $
# 
# Function to load the Add Health (Wave I) network data obtained via
# KONECT (original from Linton Freeman's website)
# TSV files and convert to igraph format
# for use in R with statnet and to save externally for Snowball PNet etc.
#
# Data downloaded from KONECT
# http://konect.uni-koblenz.de/networks/moreno_health
#
# Directed, no attributes on nodes, edges have weights, but we do not use them.
# Also convert graph to undirected

library(igraph)


#
# read_addhealth_data() - load the Add Health network
#
# Paramters: 
#   addhealth_dir - directory containing the data
#
# Return value:
#    igraph object of Add Health friendship network
#
read_addhealth_data <- function(addhealth_dir) {
                  
  edgelist <- read.table(paste(addhealth_dir, 'out.moreno_health_health',
                               sep=.Platform$file.sep),
                          header=FALSE, comment.char='%')
  # first two columns are node ids, thried is weight
  # remove thrid column to just get edge list
  edgelist$V3 <- NULL
  g <- graph.edgelist(as.matrix(edgelist), directed=TRUE)
  stopifnot(vcount(g) == 2539)
  stopifnot(ecount(g) == 12969)
  g <- simplify(as.undirected(g))
  V(g)$name <- as.integer(V(g)) # 'name' is special in igraph, lets us index directly
  print(g)  #XXX
  return(g)
}

