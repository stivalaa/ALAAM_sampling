#!/usr/bin/Rscript
#
# File:    plotPowerLaw.R
# Author:  Alex Stivala
# Created: December 2015
#
# Function to plot power law and log-normal fit 
# to graph degree distribution,
# using the poweRlaw package (Gillespie 2015 J. Stat. Soft)
#

library(igraph)
library(poweRlaw)


bootstrap_num_sims <- 5000
bootstrap_num_threads <- 16


# read in R source file from directory where this script is located
#http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
source_local <- function(fname){
  argv <- commandArgs(trailingOnly = FALSE)
  base_dir <- dirname(substring(argv[grep("--file=", argv)], 8))
  source(paste(base_dir, fname, sep=.Platform$file.sep))
}

# 
# plot_power_law() fit an plot power-law and log-normal distribution
#                      to degree distribution of a graph
#
#  Parmaters:
#      g - igraph graph object
#      title - text for main graph title
#      useoutdegree - True for out degree, else in degree
#
# Return value: None. Does an R plot,, with no device or dev.off(), so
#               can be called multiple times with par(mfrow=c(3,3)) etc.
#
plot_power_law <- function(g, title, useoutdegree) {
	# fit power-law distribution
	if (useoutdegree) {
		dd <- degree(g, mode='out')
	} else {
		dd <- degree(g, mode='in')
	}
  dd <- dd[which(dd > 0)]  # must be strictly positive for poweRlaw
  pl_m<-displ$new(dd)
  est_pl <- estimate_xmin(pl_m)
  pl_m$setXmin(est_pl)
	print(pl_m)

	# get p-value from bootstrap
	bs_p <- bootstrap_p(pl_m, no_of_sims=bootstrap_num_sims, threads=bootstrap_num_threads, seed=1)
	cat("powerlaw bootstrap p-value = ", bs_p$p, "\n")

	# fit log-normal distribution
	ln_m <- dislnorm$new(dd)
  est_ln <- estimate_xmin(ln_m)
  ln_m$setXmin(est_ln)
	print(ln_m)
	# get p-value from bootstrap
	ln_bs_p <- bootstrap_p(ln_m, no_of_sims=bootstrap_num_sims, threads=bootstrap_num_threads, seed=1)
	cat("log-normal bootstrap p-value = ", ln_bs_p$p, "\n")

	if (est_ln$xmin != est_pl$xmin) {
			cat("WARNING: powerlaw Xmin = ", est_pl$xmin, 
			    "but lognormal Xmin = ", est_ln$xmin,
			    "cannot compare distributions\n")
	} else {
		# compare distributions
		comp <- compare_distributions(pl_m, ln_m) 
		cat("compare power law and lognoramal test statistic = ", comp$test_statistic, "\n")
		cat("p-value = ", comp$p_two_sided, "\n")
	}

	# plot data and distributions
	if (useoutdegree) {
		#plot(pl_m, xlab='out-degree', ylab='CDF', main=title)
		plot(pl_m, xlab='Degree', ylab='CDF', main=title)
	} else {
		plot(pl_m, xlab='in-degree', ylab='CDF', main=title)
	}
	#pl_txt <- bquote(plain("power law") ~~ alpha == .(pl_m$pars)) # does not work
	#pl_txt <- bquote(expression(plain("power law") ~~ alpha == .(pl_m$pars))) # does not work
	#pl_txt <- expression(substitute(alpha == valpha, list(valpha=pl_m$pars[1]))) # does not work
	#pl_txt <- substitute(expression(alpha == valpha), list(valpha=pl_m$pars[1])) # does not work
	pl_txt <- as.expression(substitute(plain("Power law") ~~ alpha == valpha, list(valpha=pl_m$pars[1])))
	# Finally! Why is R so difficult??? I just want Greek letters in my plot!??
	#ln_txt <- paste("lognormal mu =", ln_m$pars[1], "sigma =", ln_m$pars[2])
	ln_txt <- as.expression(substitute(plain("Log-normal") ~~ mu == vmu ~~ sigma == vsigma, list(vmu=ln_m$pars[1], vsigma=ln_m$pars[2])))
	colors <- c(2,3)
	ltys <- c(1,2)
	lines(pl_m, col=colors[1], lty=ltys[1])
	lines(ln_m, col=colors[2], lty=ltys[2])
	legend('bottomleft', legend=c(pl_txt, ln_txt), col=colors, lty=ltys,bty='n')
}
