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
Oct_dec_leap_yrs <-   subset(daily_mean_temp_above10, 275:366) #62
jan_april_leap_yrs <- subset(daily_mean_temp_above10, 1:121) #121
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
  Oct_dec_non_leap_yrs <-   subset(daily_mean_temp_above10, 274:365) #91
  jan_april_non_leap_yrs <- subset(daily_mean_temp_above10, 1:120) #120
  GS_non_leap_yrs <- stack(Oct_dec_non_leap_yrs, jan_april_non_leap_yrs) #this should be 211 n layers ()
  
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

#barrossa_st <- st_read("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/Vine_health_data/CSIRO/GI/ZONE/barossa_WGS.shp")
barrossa_st <- st_read("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/Vine_health_data/CSIRO/GI/baroosa_ext_WGS_buff3.shp")
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




#########################################################################################################################
####                           create a plot of how  has changed over time
#########################################################################################################################


##### clip the grids for each year to the study area
GDD_all_yrs

GDD_all_yrs_c <- crop(GDD_all_yrs, barrossa_sf)
GDD_all_yrs_c
plot(GDD_all_yrs_c)

GDD_all_yrs_m <- mask(GDD_all_yrs_c, barrossa_sf)
GDD_all_yrs_m
plot(GDD_all_yrs_m)

##### bring in and use a shapefile which conatins the points I want to extract

#this is a barossa modified grid as a series of points (shapefile)
barrossa_st_extract <- st_read("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/extract_jan_temp_yrs_WGS.shp")
barrossa_extract_sf <- as(barrossa_st_extract, "Spatial") #convert to a sp object
class(barrossa_extract_sf)
plot(barrossa_extract_sf)
library(raster)
library(rasterVis)
#plt <- levelplot(layer.1, margin=F, 
#                 main="Mean Jan temp for first year")
#plt + layer(sp.points(barrossa_extract_sf, col="black", pch=16, cex=0.5))

#crs(barrossa_extract_sf)
#crs(layer.1)
GDD_all_yrs_extract <- extract(GDD_all_yrs, barrossa_extract_sf, method="simple")
class(GDD_all_yrs_extract)
head(GDD_all_yrs_extract)

GDD_all_yrs_wide <- data.frame(barrossa_extract_sf$POINT_X, barrossa_extract_sf$POINT_Y, GDD_all_yrs_extract)
head(GDD_all_yrs_wide)
#I am not confiedent in the order think it might be leap year first and then non leap years
#names(GDD_all_yrs_wide) <- c("POINT_X", "POINT_Y", "1989", "1990", "1991", "1992", "1993", "1994",
#                              "1995", "1996", "1997", "1998", "1999", "2000",
#                              "2001", "2002", "2003", "2004", "2005", "2006",
#                              "2007", "2008", "2009", "2010", "2011", "2012",
#                              "2013", "2014", "2015", "2016", "2017", "2018")

names(GDD_all_yrs_wide) <- c("POINT_X", "POINT_Y", "1992", "1996", "2000", "2004", "2008", "2012",
                             "2016", "1989", "1990", "1991", "1993", "1994",
                             "1995", "1997", "1998", "1999", "2001", "2002",
                             "2003", "2005", "2006", "2007", "2009", "2010",
                             "2011", "2013", "2014", "2015", "2017", "2018")


head(GDD_all_yrs_wide)
##### make the data narrow
library(dplyr)
library(tidyverse)
GDD_all_yrs_narrow <- gather(GDD_all_yrs_wide, key = "year", value = "GDD_all", `1989`:`2018` )
head(GDD_all_yrs_narrow)

######export as  csv this is a slow step
write.csv(GDD_all_yrs_narrow,
          "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/GDD_all_yrs_narrow_extract_pts.csv") 

#library(ggplot2)
ggplot(GDD_all_yrs_narrow, aes(GDD_all))+
  geom_density()+
  facet_wrap(.~year)




head(GDD_all_yrs_narrow)
#change year to double to get geom_smooth to work
GDD_all_yrs_narrow <- mutate(GDD_all_yrs_narrow, year_as_double = as.double(year))

ggplot(GDD_all_yrs_narrow, aes(factor(year_as_double), GDD_all))+
  geom_boxplot()+
  #geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+ #straight line regression
  geom_smooth(color="black", aes(group=1))+ #smooth line
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        plot.caption = element_text(hjust = 0))+
  labs(x = "Year",
       y = "Growing degreee days",
       title = "Sample points over the Barossa",
       subtitle = "GS defined as 1st Oct to 30th April",
       caption = "First the GGD is calculated for each pixel by year, then the values for each pixel is extracted point by point. This is achieved by using the Barossa modified boundary and converting it into a shapefile
       ")



######################################################################################################
#I have a line through the middle of the grid running weat to east of low values this looks like an error??
plot(mean_GDD_all_yrs_c)



GDD_all_yrs #30 years of GGD 
check_GDD_all_yrs_c <- crop(GDD_all_yrs, barrossa_sf)
check_GDD_all_yrs_c
plot(check_GDD_all_yrs_c$index_1.23)
