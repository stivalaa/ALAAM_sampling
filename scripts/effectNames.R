#!/usr/bin/Rscript
#
# File:    effectNames.R
# Author:  Alex Stivala
# Created: September 2016
#
# $Id: effectNames.R 648 2016-09-27 07:40:15Z stivalaa $
#
#
# Effect names and printable versions of effect names, to be included in
# plotting scripts.
#

#
# get_effects() - return list of effects given filename describing network
#
get_effects <- function(filename) {
    if (length(grep('project90realattributes', filename)) > 0){
        effects <- c('Attribute_Density', 'Activity', 'Contagion', 'oOb_for_Attribute1', 'oOb_for_Attribute2', 'oOb_for_Attribute3', 'oOb_for_Attribute4', 'oOb_for_Attribute10', 'oOb_for_Attribute11', 'oOb_for_Attribute12', 'oO_Osame_for_Attribute1')
    } else {    
        effects <- c('Attribute_Density', 'Activity', 'Contagion', 'oOb_for_Attribute1', 'oOc_Continuous_Attribute1')
    }
    return(effects)
}

#
# get_effect_names() - return list of effect names lining up with effects,
#                      given filename describing network
#
get_effect_names <- function(filename) {
    if (length(grep('project90realattributes', filename)) > 0) {
        effect_names <- c('Density', 'Activity', 'Contagion', 'Female', 'Sex worker', 'Pimp', 'Sex work client', 'Disabled', 'Unemployed', 'Homeless', 'Same race')        
    } else {
        effect_names <- c('Density', 'Activity', 'Contagion', 'Binary', 'Continuous')        
    }
    return (effect_names)
}
    


#
# label function for ggplot2 labeller
#
label_function <- function(variable, value) {
    sapply(value, FUN = function(xval) 
        if (variable == 'numWaves') {
            paste('Number of waves:', format(xval))
        }
        else if (variable == 'Effect') {
            switch(format(xval),
                   'aaaAttribute_Density' = 'Density', #aaa prefix needed for hack in new version to make it work with level orders like it used to
                   'Attribute_Density' = 'Density',
                   'Activity' = 'Activity',
                   'Contagion' = 'Contagion',
                   'oOb_for_Attribute1' = 'Binary',
                   'oOc_Continuous_Attribute1' = 'Continuous'
                   )
        }
        else {
            bquote(.(variable) == .(xval))
        }
    )
}

#
# label function for ggplot2 labeller for Project 90 real attributes
#
label_function_project90realattributes <- function(variable, value) {
    sapply(value, FUN = function(xval) 
        if (variable == 'numWaves') {
            paste('Number of waves:', format(xval))
        }
        else if (variable == 'Effect') {
            switch(format(xval),
                   'Attribute_Density'       = 'Density',
                   'Activity'                = 'Activity',
                   'Contagion'               = 'Contagion',
                   'oOb_for_Attribute1'      = 'Female',
                   'oOb_for_Attribute2'      = 'Sex worker',
                   'oOb_for_Attribute3'      = 'Pimp',
                   'oOb_for_Attribute4'      = 'Sex work client',
                   'oOb_for_Attribute10'     = 'Disabled',
                   'oOb_for_Attribute11'     = 'Unemployed',
                   'oOb_for_Attribute12'     = 'Homeless',
                   'oO_Osame_for_Attribute1' = 'Same race'     
                   )
            
        }
        else {
            bquote(.(variable) == .(xval))
        }
    )
}


#
# get_label_function() - return label function for ggplot2 given filename
#                        describing network
#
get_label_function <- function(filename) {
    if (length(grep('project90realattributes', filename)) > 0) {
        labelfunction <- label_function_project90realattributes
    } else {
        labelfunction <- label_function
    }
    return (labelfunction)
}
