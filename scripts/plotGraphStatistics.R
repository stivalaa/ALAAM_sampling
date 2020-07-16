#
# File:    plotGraphStatistics.R
# Author:  Alex Stivala
# Created: January 2015
#
# 
# R function to plot statistics about a graph: degree distribution,
# component size distribution, cumuluative degree distributino,
# transitivity vs degree (loglog) etc.
#
# 

library(igraph)

#
# plot_graph_statistics() - plot graph statistics, multiple plots on eps file
#
# Parameters:
#      g - igraph graph object
#      filename - filename of .eps file to write plots to
#
plot_graph_statistics <- function(g, filename) {
  postscript(filename, onefile=FALSE,
           paper="special", horizontal=FALSE, width=9, height=6)
  par(mfrow = c(2, 2))

  # histograms of degree and component size
  hist(degree(g), xlab = 'Degree', main = paste('N = ',  vcount(g), 
      ', Median = ', format(median(degree(g)), digits=4),
      ', Mean = ', format(mean(degree(g)), digits=4), sep=''))
  components <- decompose.graph(g)
  component_sizes <- sapply(components, function(g) vcount(g))
  hist(component_sizes, xlab = 'Component size', 
       main = paste('N = ', length(components),
       ', Median = ', format(median(component_sizes), digits=4), 
       ', Mean = ', format(mean(component_sizes), digits=4), sep=''))

  # cumulative degree distribution, log-log plot
  dtab <- table(degree(g))
  plot(as.numeric(names(dtab)), 1 - cumsum(as.numeric(dtab)/vcount(g)),
       log='xy', xlab='Degree (d)', ylab=expression(Pr(x >= d)), main='CDF')
 
  # local transitivity by degree, log-log plot
  #if  ( max(transitivity(g, type='local')) > 0  ) { #fails, error
  #if  ( ! is.na(max(transitivity(g, type='local'))) ) {
  if  ( ! all( transitivity(g, type='local') == 0 | is.na(transitivity(g, type='local'))) ) {
    plot(degree(g), transitivity(g, type='local'), xlab = 'Degree',
        ylab='Local clustering coefficient', log='xy')
  }

  dev.off()
}
