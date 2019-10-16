
##Aggregate SILO monthly and annual and build map layers
##================================
rm(list=ls())

##Libraries and functions=================================================================
#install.packages("raster")
#install.packages("ncdf4")
#install.packages("RNetCDF")
#install.packages("data.table")
#install.packages("reshape2")
#install.packages("doBy")
#install.packages("maptools")
#install.packages("maps")
#install.packages("rasterVis")
#install.packages("lattice")
#install.packages("mapdata")
#install.packages("RColorBrewer")
#install.packages("lubridate")
#install.packages("spatial.tools")
#install.packages("mapdata")
#install.packages("RSenaps")
#install.packages("settings")
#install.packages("httr")
#install.packages("maps")


library(sp)
library(rgdal)
library(raster)

library(ncdf4)
library(RNetCDF)
library(RColorBrewer)
library(data.table)
library(reshape2)
library(doBy)
library(maptools)
library(maps)
library(lattice)
library(latticeExtra)
library(rasterVis)
library(mapdata)

library(lubridate)
library(spatial.tools)
library(mapdata)
require(RSenaps) #error message for my R version
library(settings)
library(httr)
library(sf)

#===========================
# Compute temp variables ------------------------------------------------------




############################################################################################################################
################### Start here ############################################################################################

barrossa_st <- st_read("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/Vine_health_data/CSIRO/GI/baroosa_ext_WGS_buff3.shp")
barrossa_sf <- as(barrossa_st, "Spatial") #convert to a sp object

#as a function per year
#function one
function_daily_mean_temp <- function(min, max) {
  daily_mean_temp <- (min +max)/2
  return(daily_mean_temp)
}
#function two
function_jan_mean_temp_by_yr <- function(year_input) {
  
  min_1 <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/min_temp/",
          year_input, ".min_temp.nc", sep = ""),varname = "min_temp")
  max_1 <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/max_temp/",
          year_input , ".max_temp.nc", sep = ""),varname = "max_temp")
  
  min <- crop(min_1, barrossa_sf)
  max <- crop(max_1, barrossa_sf)
  
  daily_mean_temp <- overlay(min, max, fun = function_daily_mean_temp)
  daily_mean_temp_jan <- subset(daily_mean_temp, 1:30) #pull out the first 30 days of mean temp ? should this be 31??
  av_jan_mean_temp <- mean(daily_mean_temp_jan)
}




### list of years ####
#jax_list <- c("2016", "2017", "2018") #subset of data
jax_list <- as.character(c(1989:2018)) #30 years of data as string
jax_list

#make loop ooh seems to be running that created a raster of mean jan temp for each year
for (i in jax_list) {
  assign(paste0("jan_temp", i), function_jan_mean_temp_by_yr(i))
}





STACK1 <- stack(jan_temp1989, jan_temp1990, jan_temp1991, jan_temp1992, jan_temp1993, jan_temp1994,
                jan_temp1995, jan_temp1996, jan_temp1997, jan_temp1998, jan_temp1999, jan_temp2000,
                jan_temp2001, jan_temp2002, jan_temp2003, jan_temp2004, jan_temp2005, jan_temp2006,
                jan_temp2007, jan_temp2008, jan_temp2009, jan_temp2010, jan_temp2011, jan_temp2012,
                jan_temp2013, jan_temp2014, jan_temp2015, jan_temp2016, jan_temp2017, jan_temp2018)
means_jan_temp <- calc(STACK1, fun = mean, na.rm = T)
means_jan_temp
plot(means_jan_temp)
# #Write
writeRaster(means_jan_temp, "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/means_jan_temp_1989_2018",format = "GTiff", overwrite = TRUE) #average jan temp for 30yrs



#########################################################################################################################
##############                 end of code for mean jan temp                       ######################################
#########################################################################################################################



#########################################################################################################################
####                           create a plot of how Jan temp has changed over time
#########################################################################################################################


STACK1

##### bring in and use a shapefile which conatins the points I want to extract

#this is a barossa modified grid as a series of points (shapefile)
barrossa_st_extract <- st_read("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/extract_jan_temp_yrs_WGS.shp")
barrossa_extract_sf <- as(barrossa_st_extract, "Spatial") #convert to a sp object
class(barrossa_extract_sf)
plot(barrossa_extract_sf)
library(raster)
library(rasterVis)
plt <- levelplot(STACK1$layer.1, margin=F, 
                 main="Mean Jan temp for first year")
plt + layer(sp.points(barrossa_extract_sf, col="black", pch=16, cex=0.5))

crs(barrossa_extract_sf)
crs(STACK1)


####
mean_jan_temp_extract <- extract(STACK1, barrossa_extract_sf, method="simple")
class(mean_jan_temp_extract)
head(mean_jan_temp_extract)

pts_jan_temp_wide <- data.frame(barrossa_extract_sf$POINT_X, barrossa_extract_sf$POINT_Y, mean_jan_temp_extract)
head(pts_jan_temp_wide)

names(pts_jan_temp_wide) <- c("POINT_X", "POINT_Y", "1989", "1990", "1991", "1992", "1993", "1994",
                "1995", "1996", "1997", "1998", "1999", "2000",
                "2001", "2002", "2003", "2004", "2005", "2006",
                "2007", "2008", "2009", "2010", "2011", "2012",
                "2013", "2014", "2015", "2016", "2017", "2018")

head(pts_jan_temp_wide)
##### make the data narrow
library(dplyr)
library(tidyverse)
pts_jan_temp_narrow <- gather(pts_jan_temp_wide, key = "year", value = "Mean_Jan_temp", `1989`:`2018` )
head(pts_jan_temp_narrow)


######export as  csv this is a slow step
write.csv(pts_jan_temp_narrow,
          "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/pts_jan_temp_narrow_pts.csv") 


#library(ggplot2)
ggplot(pts_jan_temp_narrow, aes(Mean_Jan_temp))+
  geom_density()+
  facet_wrap(.~year)



#ggplot(pts_jan_temp_narrow, aes(year, Mean_Jan_temp))+
#  geom_boxplot()+
#  theme_classic()+
#  theme(axis.text.x = element_text(angle = 90, hjust=1),
#        plot.caption = element_text(hjust = 0))+
#  labs(x = "Year",
#       y = "Mean Jan temperature",
#       title = "Sample points over the Barossa",
#       caption = "First the mean January temperature is calculated for each pixel by year, then the values for each pixel is extracted point by point. This is achieved by using the Barossa modified boundary and converting it into a shapefile
#       ")

pts_jan_temp_narrow <- mutate(pts_jan_temp_narrow, year_as_double = as.double(year))

ggplot(pts_jan_temp_narrow, aes(factor(year_as_double), Mean_Jan_temp))+
  geom_boxplot()+
  #geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+ #straight line regression
  geom_smooth(color="black", aes(group=1))+ #smooth line
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        plot.caption = element_text(hjust = 0))+
  labs(x = "Year",
       y = "Mean Jan temperature",
       title = "Sample points over the Barossa",
       caption = "First the mean January temperature is calculated for each pixel by year, then the values for each pixel is extracted point by point. This is achieved by using the Barossa modified boundary and converting it into a shapefile
       ")































































































