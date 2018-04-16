###############################################################################
###
###   Off-grid Analysis
###   PURPOSE: plot optimization results
###
###############################################################################

# Clear workspace
rm(list = ls())

# Packages
library(pacman)
p_load(magrittr, dplyr, stringr, ggplot2, ggforce)

# Set working directory
DIR = "C:\\Users\\will-\\Google Drive\\UCBerkeley\\2018Spring\\Comp Programing in Econ\\Project\\"
#DIR = "C:\\Users\\Will\\GoogleDrive\\UCBerkeley\\2018Spring\\Comp Programing in Econ\\Project\\"
OUT = "out"
INPUT = "in"
##########################################################
## I. read in output data ################################
##########################################################

sizing <- read.csv(paste0(DIR,OUT,"\\opt_result.csv"))

##########################################################
## I. plot pdf of results ################################
##########################################################

##get pages
p <- ggplot(sizing, aes(x = shed_frac, y = pv)) +
  geom_point(aes(x = shed_frac, y = pv), col = 1) +
  geom_point(aes(x = shed_frac, y = storage), col = 2) +
  facet_wrap_paginate(~id, ncol = 4, nrow = 4, page = 1)
print(p)

length <- ceiling(length(unique(sizing$id))/16)

#plotting
pdf(paste0(DIR,OUT,'\\multiple_plot.pdf'), width = 18, height = 11)

for (index in 1:length){
  
  ##PV
  print(ggplot(sizing, aes(x = shed_frac, y = pv)) +
          geom_point(col=1) +
          facet_wrap_paginate(~id, ncol = 4, nrow = 4, page = index)) 
  
  ##storage
  print(ggplot(sizing, aes(x = shed_frac, y = storage)) +
          geom_point(col=2) +
          facet_wrap_paginate(~id, ncol = 4, nrow = 4, page = index)) 
  
}
dev.off()


