#!/usr/bin/Rscript
#
# File:    visualizeNetworkAlaam.R
# Author:  Alex Stivala
# Created: October 2015
#
# Draw network visualizaton of netowrk with binary and conitnuous attributse
# as well as binary outcome (ALAAM repsonse variable).
#
#
# Usage: Rscript visualizeNetworkAlaam.R [-d] graphfile.txt binattributesfile.txt continuousAttributsfile.txt outcomefile.txt output_postscript_file.eps
#
#  -d  : graph is directed (default undirected)
#  graphfile.txt    is adjacnecy matrix
#  binaryattributesfile.txt is binary attributes file (one column, binary value)
#  continuousattributsefile.txt is continuous attributes file (one column, float)
#  outcomefile.txt is binary outcome file (one column, binary value, Pajek header)
#  output_postscript_file.eps is filename to write postscript of graph viz to (WARNING: overwritten)
#
#
# Example usage:
#  Rscript visualizeNetworkAlaam.R n500_kstar_simulate12750000_matrix.txt  binaryAttribute_50_50_n500.txt  continuousAttributes_n500.txt sample-n500_bin_cont4800000.clu n500_sample4800000.eps
#

library(igraph)
library(RColorBrewer)

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

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 5 || length(args) > 6) {
  cat("Usage: Rscript visualizeNetworkAlaam.R [-d] graph_filename.txt binattr_filename.txt contattr_filename.txt outcomefilename.clu output_file.eps\n")
  quit(save="no")
}
basearg <- 0
directed = FALSE
if (length(args) == 6) {
    if (args[1] == "-d") {
        directed <- TRUE
        basearg <- basearg + 1
    }
    else {
        cat("Usage: Rscript visualizeNetworkAlaam.R [-d] graph_filename.txt binattr_filename.txt contattr_filename.txt outcomefilename.clu output_file.eps\n")
        quit(save="no")
    }
}
graph_filename <- args[basearg+1]
binattr_filename <- args[basearg+2]
contattr_filename <- args[basearg+3]
outcome_filename <- args[basearg+4]
output_postscript_filename <- args[basearg+5]

g <- read_graph_matrix_file(graph_filename,directed=directed)
binattr <- read_attr_file(binattr_filename)
contattr <- read_attr_file(contattr_filename)
outcome <- read_outcome_file(outcome_filename)


stopifnot(length(binattr) == vcount(g))
stopifnot(length(contattr) == vcount(g))
stopifnot(all(binattr %in% c(0,1)))
print(g) # print info about graph
cat("fraction of outcome=1 nodes: ", sum(outcome)/length(outcome), "\n")

V(g)$binattr <- binattr # binary attributes line up with vertices
V(g)$contattr <- contattr # continuous attributes line up with vertices
V(g)$outcome <- outcome # outcome binary vars line up with vertices

palette <- rev(brewer.pal(max(binattr)+1, "YlGnBu")) #colorblind-safe,print friendly
colours <- palette[(outcome+1)]

# for inclusion in LaTeX or epstopdf, stop it rotating it
postscript(output_postscript_filename, horizontal=FALSE,onefile=FALSE)

#http://stackoverflow.com/questions/8929663/r-legend-placement-in-a-plot
#XXX layout(rbind(1,2), heights=c(9,1)) # put legend on bottom 1/10th of the chart


# scale vertexsizes according to continuous attribute
minsize <- 1 # smallest vertex size
maxsize <- 6 # largest vertex size
vertexsizes <-  minsize + ((maxsize-minsize) * (V(g)$contattr - min(V(g)$contattr)) / (max(V(g)$contattr) - min(V(g)$contattr)))
zerosize <-  minsize + ((maxsize-minsize) * (0 - min(V(g)$contattr)) / (max(V(g)$contattr) - min(V(g)$contattr)))
plot(g, layout = layout.kamada.kawai,
     vertex.shape  = sapply(V(g)$binattr, 
                            function(x) if (x == 1) "square" else "circle"),
     vertex.size = vertexsizes,
     vertex.label = NA,
     vertex.color = colours      )

# setup for no margins on the legend
#XXX par(mar=c(0,0,0,0))
# c(bottom,left,top,right)
#XXX plot.new()
binattlabels <- sort(unique(V(g)$binattr))
outcomelabels <- sort(unique(V(g)$outcome))
shapes=c(19, 15) #  circle, square
legend("bottomleft", horiz=TRUE, legend=binattlabels, 
       pch=19, col=palette , title="Outcome", 
       bty='n',
       cex=1.0)
legend("bottomright", horiz=TRUE, legend=outcomelabels, 
       pch=shapes, col="black"   , title="Binary attribute",
       bty='n',
       cex=1.0)

# https://stackoverflow.com/questions/38451431/add-legend-in-igraph-to-annotate-difference-vertices-size

contattrlabels <- c(format(min(V(g)$contattr), digits=2), 
                    format(0,digits=2), 
                    format(max(V(g)$contattr),digits=2))
leg<-legend("bottom", horiz=TRUE, legend=contattrlabels, 
       pch=21,  title="Continuous attribute",
       bty='n', cex=1.0,
       pt.cex=c(min(vertexsizes), mean(vertexsizes), max(vertexsizes)),
       col='white')
       

summary(V(g)$contattr)#XXX
summary(vertexsizes)#XXX
#print(leg$text$x)
#print(leg$rect)
x <- (leg$text$x) - 0.030 # trial & error
y <- leg$text$y
symbols(x, y, 
        circles=c(min(vertexsizes), zerosize, max(vertexsizes))/200,
        inches=FALSE, add=TRUE, bg='black')
dev.off()


