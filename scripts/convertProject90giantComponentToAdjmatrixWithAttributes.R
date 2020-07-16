#!/usr/bin/Rscript
#
# File:    convertProject90giantComponentToAdjmatrixWithAttributes.R
# Author:  Alex Stivala
# Created: August 2016
#
# $Id: convertProject90giantComponentToAdjmatrixWithAttributes.R 538 2016-08-15 04:37:28Z stivalaa $
#
# Load the Project 90 data and convert the giant component of the 
# network to adjacency matrix format for use in IPNet etc, and
# also write all the (binary) attributes to a file in IPNet format
# (NB no header in this format so also write binattr_names.txt
# file with attribute names in order, that would have been header)
#
# Becauase IPNet cannot handle NA values, NA on binary attribute is converted
# to 0 and NA on categorical attribute is changed to its own category
# (highest existing categorical number plus one)
#
# Usage: RScript convertProject90giantComponentToAdjmatrixWithAttributes.R datadir
#
#  datadir is the directory containing the project 90 data
#
#  Output is written to ./project90_giantcomponent_adjmatrix.txt
#                       ./project90_giantcomponent_binattr.txt
#                       ./project90_giantcomponent_catattr.txt
#                       ./project90_giantcomponent_binattr_names.txt
#                       ./project90_giantcomponent_catattr_names.txt
#  (WARNING overwritten if they exist)
#

library(igraph)

# read in R source file from directory where this script is located
#http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
source_local <- function(fname){
  argv <- commandArgs(trailingOnly = FALSE)
  base_dir <- dirname(substring(argv[grep("--file=", argv)], 8))
  source(paste(base_dir, fname, sep=.Platform$file.sep))
}

source_local('readProject90Data.R')
source_local('readFiles.R')
source_local('snowballSample.R')

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 1) {
 cat('Usage: Rscript convertProject90giantComponentToAdjmatrixwithAttributes.R datadir\n')
 quit(save="no")
}

datadir <-args[1]

g <- read_project90_data(datadir)
g <- giant.component(g)
print(g)
# See Goel & Salganik 2010 "Assessing respondent-driven sampling"
# PNAS 107(15):6743-6747 [p. 6744 "Data and Methods"]
stopifnot(vcount(g) == 4430) 
stopifnot(ecount(g) == 18407)

write_graph_file("./project90_giantcomponent_adjmatrix.txt", g, write_header=FALSE)

f <- file("./project90_giantcomponent_binattr.txt", open="wt")
for (i in 1:vcount(g)) {
  for (attr_name in project90_attr_names) {
    if (attr_name %in% project90_categorical_attr_names) {
      next
    }
    attr_value <- get.vertex.attribute(g, attr_name, V(g)[i])
    if (is.na(attr_value)) {
      attr_value <- 0
    } else {
      stopifnot(attr_value %in% c(0, 1))
    }
    cat(attr_value, ' ', file=f, sep='')
  }
  cat('\n', file=f, sep='')
}
close(f)
f <- file("./project90_giantcomponent_binattr_names.txt", open="wt")
for (attr_name in project90_attr_names) {
    if (attr_name %in% project90_categorical_attr_names) {
      next
    }
  cat(attr_name, ' ',  file=f, sep='')
}
cat('\n', file=f, sep='')
close(f)

f <- file("./project90_giantcomponent_catattr.txt", open="wt")
for (i in 1:vcount(g)) {
  for (attr_name in project90_categorical_attr_names) {
    maxcat <- max(get.vertex.attribute(g, attr_name, V(g)), na.rm=TRUE)
    attr_value <- get.vertex.attribute(g, attr_name, V(g)[i])
    if (is.na(attr_value)) {
      attr_value <- maxcat + 1
    } else {
#      cat(i, attr_name, maxcat, attr_value) 
      stopifnot(attr_value >= 0 && attr_value <= maxcat)
    }
    cat(attr_value, ' ', file=f, sep='')
  }
  cat('\n', file=f, sep='')
}
close(f)
f <- file("./project90_giantcomponent_catattr_names.txt", open="wt")
for (attr_name in project90_categorical_attr_names) {
  cat(attr_name, ' ', file=f, sep='')
}
cat('\n', file=f, sep='')
close(f)

