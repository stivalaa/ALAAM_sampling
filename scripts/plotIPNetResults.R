#!/usr/bin/Rscript
#
# File:    plotIPNetResults.R
# Author:  Alex Stivala
# Created: September 2015
#
# $Id: plotIPNetResults.R 696 2018-07-18 01:51:50Z stivalaa $
#
# Read IPNet estimation results (from collect_estimation_results.sh) 
# and plot graphs of snowball estimates and standard errors
#
# Usage: Rscript plotIPNetResults.R results_file.txt
#
#
# Output files are PostScript files named
#
#  basename-meanse.eps and
#  basename-boxplot.eps
#  basename-stderrhistogram.eps
# 
# and text file
#
#  basename_error_statistics.txt
#
# with bias, RMSE, etc. in format for R read.table()
#
# where basenanme is basename of results_file.txt
#
# WARNING: all output files are overwritten if they exist.
#
# e.g.: Rscript plotIPNetResults.R estimation_n500_bin_cont_results.txt
# 

library(ggplot2)
library(grid)
library(gridExtra)
library(PropCIs) # for Wilson score test for false negative/positive rate
library(boot)    # for nonparameteric boostrap percenage ci on bias on rmse

#zSigma <- 1.96 # number of standard deviations for 95% confidence interval
#zSigma <- 2.58 # number of standard deviations for 99% confidence interval
zSigma <- 2


options(digits=4) # for printing rmse etc. values on x axis label

Replicates <- 20000  # bootstrap replicates


#
# function to obtain RMSE estimate, to be used as parameter to boot()
#   data is matrix where dimensino 1 is index and dimension 2 is
#     1: estimated parameter value
#     2: true parameter value
rmse_bootwrapper <- function(data, indices) {
    estimates <- data[indices,1]
    true_parameters <- data[indices,2]
    rmse <- sqrt(mean((estimates - true_parameters)^2))
    stopifnot(all(!is.na(rmse)))
    return(rmse)
}

#
# function to obtain bias estimate, to be used as parameter to boot()
#   data is matrix where dimensino 1 is index and dimension 2 is
#     1: estimated parameter value
#     2: true parameter value
bias_bootwrapper <- function(data, indices) {
    estimates <- data[indices,1]
    true_parameters <- data[indices,2]
    bias <- mean(estimates - true_parameters)
    return(bias)
}

#
# Use boot package to compute confidence interval with bootstrap
# percentile method.
#
# Parameters:
#     EstSample - vector of esimated values
#     true_parameter - true value
#
# Return value:
#    named list with three elements:
#       value - estimated value
#       lower - lower value of c.i.
#       upper - upper value of c.i.
#
bootstrap_rmse <- function(EstSample, true_parameter) {
    NumSamples <- length(EstSample)
    bootresults <- boot(data = cbind(EstSample, rep(true_parameter, NumSamples)),
                        statistic = rmse_bootwrapper,
                        R = Replicates,
                        parallel="multicore")
    bootCIresult <- boot.ci(bootresults, type="perc")  # 95% C.I. by default
    return(list(value=bootresults$t0,
                lower=bootCIresult$perc[4],
                upper=bootCIresult$perc[5]))
}

#
# Use boot package to compute confidence interval with bootstrap
# percentile method.
#
# Parameters:
#     EstSample - vector of esimated values
#     true_parameter - true value
#
# Return value:
#    named list with three elements:
#       value - estimated value
#       lower - lower value of c.i.
#       upper - upper value of c.i.
#
bootstrap_bias <- function(EstSample, true_parameter) {
    NumSamples <- length(EstSample)
    bootresults <- boot(data = cbind(EstSample, rep(true_parameter, NumSamples)),
                        statistic = bias_bootwrapper,
                        R = Replicates,
                        parallel="multicore")
    bootCIresult <- boot.ci(bootresults, type="perc")  # 95% C.I. by default
    return(list(value=bootresults$t0,
                lower=bootCIresult$perc[4],
                upper=bootCIresult$perc[5]))
}




args <- commandArgs(trailingOnly=TRUE)
results_filename <- args[1]
basefilename <- sub("(.+)[.].+", "\\1", basename(results_filename))

stats_filename <- paste(basefilename, '_error_statistics.txt', sep='')

if (length(grep("directed", results_filename)) > 0) {
    stopifnot(TRUE) # not supported
} else {
    effects <- c('Attribute_Density', 'Activity', 'Contagion', 'oOb_for_Attribute1', 'oOc_Continuous_Attribute1')

    # output effect names, corresponding to above
    effect_names <- c('Density', 'Activity', 'Contagion', 'Binary', 'Continuous')

    # known true values of effects above for computing rmse and biase and
    # false negative or positive rates and plotting true values on graphs
    if (length(grep("project90", results_filename)) > 0) {
        if (length(grep("realattributes_", results_filename)) > 0) {
            effects <- c('Attribute_Density', 'Activity', 'Contagion', 'oOb_for_Attribute1', 'oOb_for_Attribute2', 'oOb_for_Attribute3', 'oOb_for_Attribute4', 'oOb_for_Attribute10', 'oOb_for_Attribute11', 'oOb_for_Attribute12', 'oO_Osame_for_Attribute1')
            # output effect names, corresponding to above
            effect_names <- c('Density', 'Activity', 'Contagion', 'Female', 'Sex worker', 'Pimp', 'Sex work client', 'Disabled', 'Unemployed', 'Homeless', 'Same race')
            true_parameters <- c(-22.00, 0.55, 1.00, 1.00, 1.20, 1.00, 1.00, 1.00, 1.20, 1.20, 1.00)
        } else {
            true_parameters <- c(-15.00, 0.55, 1.00, 1.20, 1.15)
        }
    } else if (length(grep("addhealth_", results_filename)) > 0) {
        true_parameters <- c(-12.50, 0.55, 1.00, 1.20, 1.15)
    } else if (length(grep("powerlaw_", results_filename)) > 0) {
        true_parameters <- c(-4.90, 0.55, 1.00, 1.20, 1.15)
    } else if (length(grep("n1000_", results_filename)) > 0) {
        true_parameters <- c(-8.05, 0.55, 1.00, 1.20, 1.15)
    } else {
        if (length(grep("cont2_", results_filename)) > 0) {
            true_parameters <- c(-7.20, 0.55, 1.00, 1.20, 1.15)
        } else if (length(grep("lowcontagion_", results_filename)) > 0) {
            true_parameters <- c(-7.20, 0.55, 0.20, 1.20, 1.15)
        } else if (length(grep("mediumcontagion_", results_filename)) > 0) {
            true_parameters <- c(-7.20, 0.55, 0.50, 1.20, 1.15)
        } else {
            true_parameters <- c(-4.50, 0.25, 1.00, 0.20, 0.25)
        }
    }
    # check for an effect set to zero
    zero_effect <- NA
    if (length(grep("_activity0", results_filename)) > 0) {
        true_parameters[2] <- 0
        zero_effect <- "Activity"
    }
    if (length(grep("_contagion0", results_filename)) > 0) {
      true_parameters[3] <- 0
      zero_effect <- "Contagion"
    }
    if (length(grep("_binary0", results_filename)) > 0) {
      true_parameters[4] <- 0
      zero_effect <- "oOb_for_Attribute1"
    }
    if (length(grep("_continuous0", results_filename)) > 0) {
      true_parameters[5] <- 0
      zero_effect <- "oOc_Continuous_Attribute1"
    }
}
             

D <- read.table(results_filename, header=TRUE, stringsAsFactors=TRUE)

if ("percentRemoved" %in% colnames(D)) {
    stopifnot(length(unique(D$percentRemoved)) == 1)
    percentRemoved <- D$percentRemoved[1]
} else if ("numWaves" %in% colnames(D)) {
    if ("maxLinks" %in% colnames(D)) {
#XXX        stopifnot(length(unique(as.list(as.data.frame(t(D["maxLinks", "numWaves", "numSeeds"]))))) == 1)
        maxLinks <- D$maxLinks[1]
    } else {
        stopifnot(length(unique(as.list(as.data.frame(t(D["numWaves", "numSeeds"]))))) == 1)
    }
    numWaves <- D$numWaves[1]
    numSeeds <- D$numSeeds[1]
}

# sort by sampleId so samples arranged horizontally in consistent order
D$sampleId <- as.factor(D$sampleId)
D <- D[sort.list(D$sampleId),]

# remove any non-converged estimates
removecount <- 0
oldrows <- nrow(D)
orig_count <- oldrows
D <- D[which(abs(D$tRatio) < 0.1),]
if (nrow(D) != oldrows) {
    removecount <- removecount + oldrows - nrow(D)
    cat('removed ', oldrows - nrow(D), ' non-converged estimates from ', results_filename, '\n')
}

# remove bad estimate values (sometimes tRatio is 0.0 but estimate bad)
oldrows <- nrow(D)
D <- D[which(abs(D$Estimate) < 1e03),]
if (nrow(D) != oldrows) {
    removecount <- removecount + oldrows - nrow(D)    
    cat('removed ', oldrows - nrow(D), ' bad estimates from ', results_filename, '\n')
}

cat('SUMMARY: removed ', removecount, ' of total ', orig_count, ' estimates from ', results_filename, '\n')


plotlist <- list()
boxplotlist <- list()
histogramlist <- list()
stats <- data.frame()
for (i in 1:length(effects)) {
    effect <- effects[i]
    effect_name <- effect_names[i]
    De <- D[which(D$Effect == effect),]

    if (length(De$Effect) == 0) {
        cat("skipping effect ", effect, " not present in data\n")
        # put an empty plot in list at position where this effect would
        # be, to make position of effect plot in grid same, easier to compare
        emptyplot <- ggplot(data.frame()) + geom_blank() +theme(panel.background=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), plot.background=element_blank(),strip.background=element_blank(),axis.ticks=element_blank(), axis.title=element_blank(), axis.text=element_blank()) +xlim(0,1)+ylim(0,1) +annotate("text",label=paste("no", effect, "parameter",sep=' '), x=0.5,y=0.5,colour="grey")
        plotlist <- c(plotlist, list(emptyplot))
        boxplotlist <- c(boxplotlist, list(emptyplot))
        histogramlist <- c(histogramlist, list(emptyplot))
        next
    }

    stopifnot(all(!is.na(De$Estimate)))
    rmse_result <- bootstrap_rmse(De$Estimate, true_parameters[i])
    bias_result <- bootstrap_bias(De$Estimate, true_parameters[i])
    

    # count number of times the CI includes true value
    num_in_ci <- sum((De$Estimate < true_parameters[i] & De$Estimate + zSigma*De$StdErr >= true_parameters[i]) | (De$Estimate >= true_parameters[i] & De$Estimate - zSigma*De$StdErr <= true_parameters[i]))
    perc_in_ci <- 100 * num_in_ci / length(De$Estimate)

    if (true_parameters[i] == 0.0) {
        # for purely inference (sign and significance, not actual
        # value of estimate) compute False Postivie rate, as the
        # number of times the estimate CI does not include zero
        false_positive_count <- sum(
            (De$Estimate < 0 & De$Estimate + zSigma*De$StdErr < 0) |
            (De$Estimate > 0 & De$Estimate - zSigma*De$StdErr > 0) )
        false_positive_perc <- 100 * false_positive_count / length(De$Estimate)
        false_negative_perc <- NA
        fnr_lower <- NA
        fnr_upper <- NA
        confint <- scoreci(false_positive_count, length(De$Estimate), 0.95)
        fpr_lower <- confint$conf.int[1] * 100
        fpr_upper <- confint$conf.int[2] * 100
    } else {
        # for purely inference (sign and significance, not actual
        # value of estimate) compute False Negative rate, as the
        # number of times the estimate is the right sign, but the
        # CI includes zero; or, is the wrong sign.
        false_negative_count <-
            sum( (sign(De$Estimate) == sign(true_parameters[i]) & 
                  ((De$Estimate < 0 & De$Estimate + zSigma*De$StdErr >= 0) |
                   (De$Estimate >= 0 & De$Estimate - zSigma*De$StdErr <= 0)) ) |
                sign(De$Estimate) != sign(true_parameters[i]) )
        false_negative_perc <- 100 * false_negative_count / length(De$Estimate)
        false_positive_perc <- NA
        fpr_upper <- NA
        fpr_lower <- NA
        confint <- scoreci(false_negative_count, length(De$Estimate), 0.95)
        fnr_lower <- confint$conf.int[1] * 100
        fnr_upper <- confint$conf.int[2] * 100
    }
    
    # plot each sample estimate with error bar for CI 
    p <- ggplot(De, aes(x = sampleId, y = Estimate))
    p <- p + geom_point()
    p <- p + geom_errorbar(aes(ymax = Estimate + zSigma*StdErr,
                               ymin = Estimate - zSigma*StdErr))
    p <- p + geom_hline(yintercept = true_parameters[i], colour='red')
    p <- p + theme_bw()
    p <- p + theme(panel.background = element_blank(),
                   ## panel.grid.major = element_blank(),
                   ## panel.grid.minor = element_blank(),
                   plot.background = element_blank(),
                   strip.background = element_blank(),
                   panel.border = element_rect(color = 'black')
                   )
    p <- p + ylab(effect_names[i])
    p <- p + theme(axis.ticks.x = element_blank())
    p <- p + scale_x_discrete(labels=NULL)
    if (true_parameters[i] == 0.0) {
        p <- p + xlab(bquote(atop(list(bias == .(bias_result$value), RMSE == .(rmse_result$value)), list("% In CI" == .(perc_in_ci), "FPR%" == .(false_positive_perc)))))
    }  else {
        p <- p + xlab(bquote(atop(list(bias == .(bias_result$value), RMSE == .(rmse_result$value)), list("% In CI" == .(perc_in_ci), "FNR%" == .(false_negative_perc)))))
    }
    plotlist <- c(plotlist, list(p))

    # plot boxplot of all sample estimates
    p <- ggplot(De, aes(x=Effect,y=Estimate))
    p <- p + geom_boxplot()
    p <- p + geom_hline(yintercept = true_parameters[i])
    p <- p + theme_bw()
    p <- p + theme(panel.background = element_blank(),
                   ## panel.grid.major = element_blank(),
                   ## panel.grid.minor = element_blank(),
                   plot.background = element_blank(),
                   strip.background = element_blank(),
                   panel.border = element_rect(color = 'black')
                   )
    p <- p + ylab(effect_names[i])
    p <- p + theme(axis.ticks.x = element_blank())
    p <- p + scale_x_discrete(labels=NULL)
    if (true_parameters[i] == 0.0) {
        p <- p + xlab(bquote(atop(list(bias == .(bias_result$value), RMSE == .(rmse_result$value)), list("% In CI" == .(perc_in_ci), "FPR%" == .(false_positive_perc)))))
    } else {
        p <- p + xlab(bquote(atop(list(bias == .(bias_result$value), RMSE == .(rmse_result$value)), list("% In CI" == .(perc_in_ci), "FNR%" == .(false_negative_perc)))))
    }
    boxplotlist <- c(boxplotlist, list(p))

    # plot histogram of standard error values
    p <- ggplot(De, aes(x=StdErr))
    p <- p + geom_histogram(colour='black',fill='white')
    p <- p + ggtitle(effect_names[i])
    p <- p + geom_vline(xintercept = mean(De$StdErr), colour='blue', linetype='longdash')
    p <- p + geom_vline(xintercept = sd(De$Estimate), colour='red')
    p <- p + theme_bw()
    p <- p + theme(panel.background = element_blank(),
                   ## panel.grid.major = element_blank(),
                   ## panel.grid.minor = element_blank(),
                   plot.background = element_blank(),
                   strip.background = element_blank(),
                   panel.border = element_rect(color = 'black')
                   )
    p <- p + xlab('standard error')
    histogramlist <- c(histogramlist, list(p))

    if ("percentRemoved" %in% colnames(D)) {
        dfrow <- data.frame(percentRemoved=percentRemoved,
                            numConverged=length(De$Estimate),
                            Effect=effect,
                            ZeroEffect=zero_effect,
                            Bias=bias_result$value,
                            Bias_lower=bias_result$lower,
                            Bias_upper=bias_result$upper,
                            RMSE=rmse_result$value,
                            RMSE_upper=rmse_result$upper,
                            RMSE_lower=rmse_result$lower,
                            percentInCI=perc_in_ci,
                            FNRpercent=false_negative_perc,
                            FNRpercentLower=fnr_lower,
                            FNRpercentUpper=fnr_upper,
                            FPRpercent=false_positive_perc,
                            FPRpercentLower=fpr_lower,
                            FPRpercentUpper=fpr_upper)
    } else if ("numWaves" %in% colnames(D)) {
        if ("maxLinks" %in% colnames(D)) {
            dfrow <- data.frame(maxLinks=maxLinks,
                                numWaves=numWaves,
                                numSeeds=numSeeds,
                                meanNumNodes=mean(D$numNodes),
                                sdNumNodes=sd(D$numNodes),
                                numConverged=length(De$Estimate),
                                Effect=effect,
                                ZeroEffect=zero_effect,
                                Bias=bias_result$value,
                                Bias_lower=bias_result$lower,
                                Bias_upper=bias_result$upper,
                                RMSE=rmse_result$value,
                                RMSE_lower=rmse_result$lower,
                                RMSE_upper=rmse_result$upper,
                                percentInCI=perc_in_ci,
                                FNRpercent=false_negative_perc,
                                FNRpercentLower=fnr_lower,
                                FNRpercentUpper=fnr_upper,
                                FPRpercent=false_positive_perc,
                                FPRpercentLower=fpr_lower,
                                FPRpercentUpper=fpr_upper)
        } else {
            dfrow <- data.frame(numWaves=numWaves,
                                numSeeds=numSeeds,
                                meanNumNodes=mean(D$numNodes),
                                sdNumNodes=sd(D$numNodes),
                                numConverged=length(De$Estimate),
                                Effect=effect, 
                                ZeroEffect=zero_effect,
                                Bias=bias_result$value,
                                Bias_lower=bias_result$lower,
                                Bias_upper=bias_result$upper,
                                RMSE=rmse_result$value,
                                RMSE_lower=rmse_result$lower,
                                RMSE_upper=rmse_result$upper,
                                percentInCI=perc_in_ci,
                                FNRpercent=false_negative_perc,
                                FNRpercentLower=fnr_lower,
                                FNRpercentUpper=fnr_upper,
                                FPRpercent=false_positive_perc,
                                FPRpercentLower=fpr_lower,
                                FPRpercentUpper=fpr_upper)
        }
        
    }
    stats <- rbind(stats, dfrow)
}

postscript(paste(basefilename, '-meanse.eps', sep=''), onefile=FALSE,
           paper="special", horizontal=FALSE, width=9, height=6)
do.call(grid.arrange, plotlist)
dev.off()

postscript(paste(basefilename, '-boxplot.eps', sep=''), onefile=FALSE,
           paper="special", horizontal=FALSE, width=9, height=6)
do.call(grid.arrange, boxplotlist)

postscript(paste(basefilename, '-stderrhistogram.eps', sep=''), onefile=FALSE,
           paper="special", horizontal=FALSE, width=9, height=6)
do.call(grid.arrange, histogramlist)

write.table(stats, stats_filename, quote=FALSE, row.name=FALSE)


