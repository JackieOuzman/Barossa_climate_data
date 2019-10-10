####################################################################################################################
################# Import the library - some I wont use in this anlysis ###########################################
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


#####################################################################################################################
######    Cal the growing dregee days ##############################################################################
#####################################################################################################################
#function one just get the daily average
function_daily_mean_temp <- function(min, max) {
  daily_mean_temp <- (min +max)/2
  return(daily_mean_temp)
}
#function two only select temp greater than 10 dregees
f <- function(daily_mean_temp) {
  ifelse( daily_mean_temp>10,daily_mean_temp, NA)
}

#function three put it all togther
function_GG_leap_yrs <- function(yr) {
    min <- brick(
      paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/min_temp/",
        yr, ".min_temp.nc", sep = ""),varname = "min_temp")
#bring in the max grid for year
    max <- brick(
      paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/max_temp/",
        yr , ".max_temp.nc", sep = ""),varname = "max_temp")
#cal the mean daily temp for each grid cell per day 
    daily_mean_temp <- overlay(min, max, fun = function_daily_mean_temp)
#retain only values greater than 10 using the below function
    daily_mean_temp_above10 <- overlay(daily_mean_temp, fun = f)

#extrcat only values between 1st Oct and 30April
Oct_dec_leap_yrs <-   subset(daily_mean_temp_above10, 304:365) #62
jan_april_leap_yrs <- subset(daily_mean_temp_above10, 1:120) #120
GS_leap_yrs <- stack(Oct_dec_leap_yrs, jan_april_leap_yrs) #this should be 182 n layers (62 + 120)

#sum all the layers in the raster stack
sum_GDD_leap_yrs <- stackApply(GS_leap_yrs, indices = 1, fun=sum)
sum_GDD_leap_yrs
}


leap_years <- c( "1992", "1996","2000", "2004" ,"2008", "2012", "2016")


non_leap_years <- c("1989", "1990" ,"1991",  "1993", "1994" ,"1995", "1997",
                    "1998" ,"1999" ,"2001", "2002", "2003", "2005", "2006",
                    "2007" , "2009", "2010", "2011" , "2013", "2014" ,"2015" ,
                    "2017" ,"2018")



#make loop raster of GGD for GS leap year
for (i in leap_years) {
  assign(paste0("GGD_leap_yrs_", i), function_GG_leap_yrs(i))
}



GDD_all_leap_yrs <- stack(GGD_leap_yrs_1992, GGD_leap_yrs_1996, 
                     GGD_leap_yrs_2000, GGD_leap_yrs_2004, 
                     GGD_leap_yrs_2008, GGD_leap_yrs_2012,
                     GGD_leap_yrs_2016)
                     


#function four
function_GG_non_leap_yrs <- function(yr) {
  min <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/min_temp/",
          yr, ".min_temp.nc", sep = ""),varname = "min_temp")
  #bring in the max grid for year
  max <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/max_temp/",
          yr , ".max_temp.nc", sep = ""),varname = "max_temp")
  #cal the mean daily temp for each grid cell per day 
  daily_mean_temp <- overlay(min, max, fun = function_daily_mean_temp)
  #retain only values greater than 10 using the below function
  daily_mean_temp_above10 <- overlay(daily_mean_temp, fun = f)
  
  #extrcat only values between 1st Oct and 30April
  Oct_dec_non_leap_yrs <-   subset(daily_mean_temp_above10, 303:365) #62
  jan_april_non_leap_yrs <- subset(daily_mean_temp_above10, 1:119) #119
  GS_non_leap_yrs <- stack(Oct_dec_non_leap_yrs, jan_april_non_leap_yrs) #this should be 181 n layers (62 + 119)
  
  #sum all the layers in the raster stack
  sum_GDD_non_leap_yrs <- stackApply(GS_non_leap_yrs, indices = 1, fun=sum)
  sum_GDD_non_leap_yrs
}


#make loop raster of GGD for GS non leap year
for (i in non_leap_years) {
  assign(paste0("GGD_non_leap_yrs_", i), function_GG_non_leap_yrs(i))
}


GDD_all_non_leap_yrs <- stack(GGD_non_leap_yrs_1989, GGD_non_leap_yrs_1990, 
                              GGD_non_leap_yrs_1991, GGD_non_leap_yrs_1993, 
                              GGD_non_leap_yrs_1994, GGD_non_leap_yrs_1995,
                              GGD_non_leap_yrs_1997, GGD_non_leap_yrs_1998,
                              GGD_non_leap_yrs_1999, GGD_non_leap_yrs_2001,
                              GGD_non_leap_yrs_2002, GGD_non_leap_yrs_2003,
                              GGD_non_leap_yrs_2005, GGD_non_leap_yrs_2006,
                              GGD_non_leap_yrs_2007, GGD_non_leap_yrs_2009,
                              GGD_non_leap_yrs_2010, GGD_non_leap_yrs_2011,
                              GGD_non_leap_yrs_2013, GGD_non_leap_yrs_2014, 
                              GGD_non_leap_yrs_2015, GGD_non_leap_yrs_2017,
                              GGD_non_leap_yrs_2018)



GDD_all_yrs <- stack(GDD_all_leap_yrs, GDD_all_non_leap_yrs)

mean_GDD_all_yrs <- calc(GDD_all_yrs, fun = mean, na.rm = T)
mean_GDD_all_yrs


library(sf)

barrossa_st <- st_read("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/Vine_health_data/CSIRO/GI/ZONE/barossa_WGS.shp")
barrossa_sf <- as(barrossa_st, "Spatial") #convert to a sp object


mean_GDD_all_yrs_c <- crop(mean_GDD_all_yrs, barrossa_sf)
mean_GDD_all_yrs_c
plot(mean_GDD_all_yrs_c)

mean_GDD_all_yrs_m <- mask(mean_GDD_all_yrs_c, barrossa_sf)
mean_GDD_all_yrs_m
plot(mean_GDD_all_yrs_m)


# #Write
writeRaster(mean_GDD_all_yrs_c, "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/mean_GDD_all_yrs_1989_2018",format = "GTiff", overwrite = TRUE) 




######################################################################################################################
######################## End od script ##############################################################################

