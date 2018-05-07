###############################################################################
###
###   Off-grid Analysis
###   PURPOSE: aggregate inputs for plotting
###
###############################################################################

# Clear workspace
rm(list = ls())

# Packages
library(pacman)
p_load(magrittr, dplyr, stringr, ggplot2,data.table)

# Set working directory
#DIR = "C:\\Users\\will-\\GoogleDrive\\UCBerkeley\\2018Spring\\Comp Programing in Econ\\Project\\"
DIR = "C:\\Users\\Will\\GoogleDrive\\UCBerkeley\\2018Spring\\Comp Programing in Econ\\Project\\"
OUT = "out"
INPUT = "in"
##########################################################
## I. read in output data ################################
##########################################################
id = fread(paste0(DIR,INPUT,"\\id.csv"))

sol <- data.frame()
for (i in 1:length(id$ID)) {
  temp.full <- fread(paste0(DIR,INPUT,"\\res_solar\\",id[i], "TYA.CSV.csv"))
  temp.full$hr <- rep(1:24,365)
  temp.full$id <- id[i]
  
  temp <- temp.full %>% group_by(hr, id) %>% summarize(out = mean(Watt)) %>% as.data.frame()
  
  
  sol <- rbind(sol,temp)
}


load <- data.frame()
for (i in 1:length(id$ID)) {
  temp.full <- fread(paste0(DIR,INPUT,"\\BASE\\",id[i], ".csv"))
  colnames(temp.full) <- c("num","Watt")
  temp.full$hr <- rep(1:24,365)
  temp.full$id <- id[i]
  
  temp <- temp.full %>% group_by(hr, id) %>% 
    summarize(avg = mean(Watt)*1000,
               tot = sum(Watt)) %>% as.data.frame()
  
  
  load <- rbind(load,temp)
}

##output results
fwrite(sol,paste0(DIR,OUT,"\\sol_agg.csv"))
fwrite(load,paste0(DIR,OUT,"\\load_agg.csv"))

##########################################################
## II. plot load and solar ###############################
##########################################################
sol <- fread(paste0(DIR,OUT,"\\sol_agg.csv"))
load <- fread(paste0(DIR,OUT,"\\load_agg.csv"))

#solar plots
jpeg(filename = paste0(DIR,"\\images\\solar.jpg"), width = 950, height = 480)
ggplot(data=sol, aes(hr,out)) + geom_line(aes(color = id, group = id)) +
  xlab(label = "Hour of Day") + ylab(label = "Watt output (W)") +
  ggtitle(label = "Figure 2: Solar output multiplier across 933 locations")
dev.off()

#load plots
jpeg(filename = paste0(DIR,"\\images\\load.jpg"), width = 950, height = 480)
ggplot(data=load, aes(hr,avg)) + geom_line(aes(color = id, group = id)) +
  xlab(label = "Hour of Day") + ylab(label = "Watt consumption (W)") +
  ggtitle(label = "Figure 1: Load consumption across 933 locations")
dev.off()

##########################################################
## II. data analysis #####################################
##########################################################

ann <- load %>% group_by(id) %>% 
  summarize(kwh = sum(tot))


