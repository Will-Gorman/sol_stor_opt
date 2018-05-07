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
p_load(magrittr, dplyr, stringr, ggplot2, ggforce, data.table,rjson, maps,maptools,
       spatstat,rgeos, broom)

# Set working directory
#DIR = "C:\\Users\\will-\\GoogleDrive\\UCBerkeley\\2018Spring\\Comp Programing in Econ\\Project\\"
DIR = "C:\\Users\\Will\\GoogleDrive\\UCBerkeley\\2018Spring\\Comp Programing in Econ\\Project\\"
OUT = "out"
INPUT = "in"
##########################################################
## I. read in output data ################################
##########################################################

sizing <- fread(paste0(DIR,OUT,"\\results_v5.csv"))
hourly <- fread(paste0(DIR,OUT,"\\outcome_v5.csv"), nrows = 438000)
fwrite(hourly,paste0(DIR,OUT,"\\outcome_view.csv"))

##########################################################
## I. data checking ################################
##########################################################
sizing$check <- sizing$load - sizing$solar_tot
sizing$energy_cost <- sizing$load * 0.12
sizing$pv_cost <- sizing$pv * 3000 * 0.07 / sizing$solar_tot
sizing$shed_tot <- as.numeric(sizing$shed_tot)


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


##########################################################
## I. Simple graphs ######################################
##########################################################
sizing$id <- as.numeric(sizing$id)
jpeg(filename = paste0(DIR,"\\images\\sol_size.jpg"), width = 950, height = 480)
ggplot(data=sizing, aes(shed_frac,pv)) + geom_point(aes(color=id)) +
  xlab(label = "% energy from the grid") + ylab(label = "PV system size (kW)") +
  ggtitle(label = "Figure 3: Solar sizing under increasing reliability constraints")
dev.off()

jpeg(filename = paste0(DIR,"\\images\\stor_size.jpg"), width = 950, height = 480)
ggplot(data=sizing, aes(shed_frac,storage)) + geom_point(aes(color=id)) +
  xlab(label = "% energy from the grid") + ylab(label = "storage system size (kWh)") +
  ggtitle(label = "Figure 4: Storage sizing under increasing reliability constraints")
dev.off()

max <- sizing %>% group_by(shed_frac) %>% 
  summarize(sol_max = max(pv),sol_min = min(pv),stor_max = max(storage),
             stor_min = min(storage))

##########################################################
## I. Geospatial Analysis ################################
##########################################################

#define functions
latlong2county <- function(pointsDF, string) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map(string, fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

##load in lat long for tmy3
tmy_data <- fromJSON(file = paste0(DIR,INPUT,"\\tmy3_lat_lng.json"))
names <- data.frame(names(tmy_data))
tmy_coor <- data.frame(matrix(unlist(tmy_data), nrow = 1020, byrow=T))
tmy_coor <-cbind(names, tmy_coor)
colnames(tmy_coor) <- c("id", "lat","long")
coor <- tmy_coor[,c(3,2)]
tmy_coor$loc <- latlong2county(coor, "county")
data <- str_split_fixed(tmy_coor$loc,",",2)
colnames(data) <- c("state","county")
tmy_coor <- cbind(tmy_coor, data)
rm(names, tmy_data, data, coor)

#merge in lat long
sizing$id <- as.factor(sizing$id)
sizing <- merge(sizing, tmy_coor, by = "id")
sizing <- subset(sizing, state != "")

#create averages by county
avg <- sizing %>% group_by(county, state, shed_frac) %>% 
  summarize(pv = mean(pv), storage = mean(storage), num = n()) %>% as.data.frame()
colnames(avg)[1] <- "subregion"
colnames(avg)[2] <- "region"

#set initial map
usa <- tidy(map('county', fill = TRUE))

# Plot the tidied shapefile
jpeg(filename = paste0(DIR,"\\images\\sol_1.jpg"), width = 950, height = 480)
usa2 <- arrange(arrange(merge(usa,filter(avg, shed_frac==0.5), by=c("subregion","region"), all=T), order), group)
ggplot(data = usa2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = pv), color = "black", size=0.07) +
  geom_path(size = 0.3) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Solar Variation with 50% grid energy") +
  coord_map()
dev.off()

jpeg(filename = paste0(DIR,"\\images\\sol_2.jpg"), width = 950, height = 480)
usa2 <- arrange(arrange(merge(usa,filter(avg, shed_frac==0.1), by=c("subregion","region"), all=T), order), group)
ggplot(data = usa2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = pv), color = "black", size=0.07) +
  geom_path(size = 0.3) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Solar Variation with 10% grid energy") +
  coord_map()
dev.off()

jpeg(filename = paste0(DIR,"\\images\\sol_3.jpg"), width = 950, height = 480)
usa2 <- arrange(arrange(merge(usa,filter(avg, shed_frac==0.00001), by=c("subregion","region"), all=T), order), group)
ggplot(data = usa2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = pv), color = "black", size=0.07) +
  geom_path(size = 0.3) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Solar Variation with 0% grid energy") +
  coord_map()
dev.off()

##storage ones
jpeg(filename = paste0(DIR,"\\images\\stor_1.jpg"), width = 950, height = 480)
usa2 <- arrange(arrange(merge(usa,filter(avg, shed_frac==0.5), by=c("subregion","region"), all=T), order), group)
ggplot(data = usa2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = storage), color = "black", size=0.07) +
  geom_path(size = 0.3) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Storage Variation with 50% grid energy") +
  coord_map()
dev.off()

jpeg(filename = paste0(DIR,"\\images\\stor_2.jpg"), width = 950, height = 480)
usa2 <- arrange(arrange(merge(usa,filter(avg, shed_frac==0.1), by=c("subregion","region"), all=T), order), group)
ggplot(data = usa2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = storage), color = "black", size=0.07) +
  geom_path(size = 0.3) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Storage Variation with 10% grid energy") +
  coord_map()
dev.off()

jpeg(filename = paste0(DIR,"\\images\\stor_3.jpg"), width = 950, height = 480)
usa2 <- arrange(arrange(merge(usa,filter(avg, shed_frac==0.00001), by=c("subregion","region"), all=T), order), group)
ggplot(data = usa2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = storage), color = "black", size=0.07) +
  geom_path(size = 0.3) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Storage Variation with 0% grid energy") +
  coord_map()
dev.off()