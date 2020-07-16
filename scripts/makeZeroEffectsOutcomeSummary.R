#!/usr/bin/Rscript
#
# File:    makeZeroEffectsOutcomeSummary.R
# Author:  Alex Stivala
# Created: January 2015
#
# Read in outcome files from ALAAM simulations with zero effect and write
# LaTeX table with positive outcome percentage mean and s.d.
#
#
# Usage:
# 
# Rscript makeZeroEffectsOutcomeSummary.R
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

# glob patterns of outcome (attribute) files 
attrglobs <- c("../simulated_n500_bin_cont2/sample-n500_bin_cont*.clu",
               "../simulated_n500_bin_cont2_activity0/sample-n500_bin_cont*.clu",               
               "../simulated_n500_bin_cont2_contagion0/sample-n500_bin_cont*.clu",
               "../simulated_n500_bin_cont2_binary0/sample-n500_bin_cont*.clu",
               "../simulated_n500_bin_cont2_continuous0/sample-n500_bin_cont*.clu",
               "../simulated_n1000_bin_cont/sample-n1000_bin_cont*.clu",
               "../simulated_n1000_bin_cont_activity0/sample-n1000_bin_cont*.clu",               
               "../simulated_n1000_bin_cont_contagion0/sample-n1000_bin_cont*.clu",
               "../simulated_n1000_bin_cont_binary0/sample-n1000_bin_cont*.clu",
               "../simulated_n1000_bin_cont_continuous0/sample-n1000_bin_cont*.clu",
               "../simulated_n500_bin_cont2lowcontagion/sample-n500_bin_cont*.clu",
               "../simulated_n500_bin_cont2lowcontagion_activity0/sample-n500_bin_cont*.clu",               
               "../simulated_n500_bin_cont2lowcontagion_contagion0/sample-n500_bin_cont*.clu",
               "../simulated_n500_bin_cont2lowcontagion_binary0/sample-n500_bin_cont*.clu",
               "../simulated_n500_bin_cont2lowcontagion_continuous0/sample-n500_bin_cont*.clu",
               "../simulated_n500_bin_cont2mediumcontagion/sample-n500_bin_cont*.clu",
               "../simulated_n500_bin_cont2mediumcontagion_activity0/sample-n500_bin_cont*.clu",               
               "../simulated_n500_bin_cont2mediumcontagion_contagion0/sample-n500_bin_cont*.clu",
               "../simulated_n500_bin_cont2mediumcontagion_binary0/sample-n500_bin_cont*.clu",
               "../simulated_n500_bin_cont2mediumcontagion_continuous0/sample-n500_bin_cont*.clu",
               "../simulated_n500_bin_cont2highcontagion/sample-n500_bin_cont*.clu",
               "../simulated_n500_bin_cont2highcontagion_activity0/sample-n500_bin_cont*.clu",               
               "../simulated_n500_bin_cont2highcontagion_contagion0/sample-n500_bin_cont*.clu",
               "../simulated_n500_bin_cont2highcontagion_binary0/sample-n500_bin_cont*.clu",
               "../simulated_n500_bin_cont2highcontagion_continuous0/sample-n500_bin_cont*.clu",
               "../simulated_Project90/sample-project90*.clu",
               "../simulated_Project90_activity0/sample-project90*.clu",
               "../simulated_Project90_contagion0/sample-project90*.clu",
               "../simulated_Project90_binary0/sample-project90*.clu",
               "../simulated_Project90_continuous0/sample-project90*.clu",
               "../simulated_AddHealth/sample-addhealth*.clu",
               "../simulated_AddHealth_activity0/sample-addhealth*.clu",
               "../simulated_AddHealth_contagion0/sample-addhealth*.clu",
               "../simulated_AddHealth_binary0/sample-addhealth*.clu",
               "../simulated_AddHealth_continuous0/sample-addhealth*.clu",
               "../simulated_n1000_powerlaw/sample-n1000_powerlaw*.clu",
               "../simulated_n1000_powerlaw_activity0/sample-n1000_powerlaw*.clu",
               "../simulated_n1000_powerlaw_contagion0/sample-n1000_powerlaw*.clu",
               "../simulated_n1000_powerlaw_binary0/sample-n1000_powerlaw*.clu",
               "../simulated_n1000_powerlaw_continuous0/sample-n1000_powerlaw*.clu",
               "../simulated_n1000_powerlawtri/sample-n1000_powerlawtri*.clu",
               "../simulated_n1000_powerlawtri_activity0/sample-n1000_powerlawtri*.clu",
               "../simulated_n1000_powerlawtri_contagion0/sample-n1000_powerlawtri*.clu",
               "../simulated_n1000_powerlawtri_binary0/sample-n1000_powerlawtri*.clu",
               "../simulated_n1000_powerlawtri_continuous0/sample-n1000_powerlawtri*.clu")

# network description, must line up with above
descr <- c('ERGM', 'ERGM', 'ERGM', 'ERGM', 'ERGM',
           'ERGM', 'ERGM', 'ERGM', 'ERGM', 'ERGM',
           'ERGM', 'ERGM', 'ERGM', 'ERGM', 'ERGM',
           'ERGM', 'ERGM', 'ERGM', 'ERGM', 'ERGM',
           'ERGM', 'ERGM', 'ERGM', 'ERGM', 'ERGM',
           'Project 90', 'Project 90', 'Project 90', 'Project 90', 'Project 90',
           'Add Health', 'Add Health', 'Add Health', 'Add Health', 'Add Health',
           'B-A'       , 'B-A',        'B-A',        'B-A',        'B-A',
           'B-A $m=4$' , 'B-A $m=4$',  'B-A $m-4$',  'B-A $m=4$',  'B-A $m=4$')

# zero effect name, must line up with above
zero_effect <- c("-", "Activity", "Contagion", "Binary", "Continuous",
                 "-", "Activity", "Contagion", "Binary", "Continuous",
                 "-", "Activity", "Contagion", "Binary", "Continuous",
                 "-", "Activity", "Contagion", "Binary", "Continuous",
                 "-", "Activity", "Contagion", "Binary", "Continuous",
                 "-", "Activity", "Contagion", "Binary", "Continuous",
                 "-", "Activity", "Contagion", "Binary", "Continuous",
                 "-", "Activity", "Contagion", "Binary", "Continuous",
                 "-", "Activity", "Contagion", "Binary", "Continuous")

# value of Contagion parameter, must line up with above
contagion_parameter <- c(1.0, 1.0, 0.0, 1.0, 1.0,
                         1.0, 1.0, 0.0, 1.0, 1.0,
                         0.2, 0.2, 0.0, 0.2, 0.2,
                         0.5, 0.5, 0.0, 0.5, 0.5,
                         1.5, 1.5, 0.0, 1.5, 1.5,
                         1.0, 1.0, 0.0, 1.0, 1.0,
                         1.0, 1.0, 0.0, 1.0, 1.0,
                         1.0, 1.0, 0.0, 1.0, 1.0,
                         1.0, 1.0, 0.0, 1.0, 1.0)                         

n <- length(attrglobs)
stopifnot(n == length(descr))
stopifnot(n == length(zero_effect))
stopifnot(n == length(contagion_parameter))

cat('\\begin{tabular}{lrrlrr}\n')
cat('\\hline\n')
cat('Network & N  & Contagion & Zero effect &\\multicolumn{2}{c}{Positive outcome \\%}\\\\\n')
cat('        &    & parameter  &            & mean & s.d.             \\\\\n')
cat('\\hline\n')
for (i in 1:length(attrglobs)) {
    if (length(Sys.glob(attrglobs[i])) == 0) {
      cat("skipping ", attrglobs[i], " no files", '\n', file=stderr())
      next
    }
    outcomes <- lapply(Sys.glob(attrglobs[i]), 
                       FUN = function(f) read_outcome_file(f))
    nodecounts <- sapply(outcomes, length)
    stopifnot(diff(range(nodecounts)) == 0) # all must have same N
    nodecount <- nodecounts[1]              # all the same, asserted above
    positivecount <- sapply(outcomes, sum)
    pospercent <- 100*as.vector(positivecount) / nodecount
    cat(descr[i],
        nodecount,
        format(contagion_parameter[i], nsmall=1),
        format(zero_effect[i]),
        format(round(mean(pospercent)), digits=2, nsmall=0),
        format(sd(pospercent), digits=3, nsmall=2),
        sep=' & ')
    cat('\\\\\n')
}
cat('\\hline\n')
cat('\\end{tabular}\n')
