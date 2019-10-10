
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
install.packages("RSenaps")
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

#===========================



########################################################################################################################
###########################                 start here                                ##################################
########################################################################################################################


#as a function to calulate the mean temp for every day per year
#function one
function_daily_mean_temp <- function(min, max) {
  daily_mean_temp <- (min +max)/2
  return(daily_mean_temp)
}
#function two this uses the above function for the whole raster per day
function_mean_temp_by_yr <- function(year_input) {
  
  min <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/min_temp/",
          year_input, ".min_temp.nc", sep = ""),varname = "min_temp")
  max <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/max_temp/",
          year_input , ".max_temp.nc", sep = ""),varname = "max_temp")
  
  daily_mean_temp <- overlay(min, max, fun = function_daily_mean_temp)
  
}

###### better #######

function_mean_temp_by_leap_yr <- function(year_input) {
  
  min <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/min_temp/",
          year_input, ".min_temp.nc", sep = ""),varname = "min_temp")
  max <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/max_temp/",
          year_input , ".max_temp.nc", sep = ""),varname = "max_temp")
  
  daily_mean_temp <- overlay(min, max, fun = function_daily_mean_temp)
  daily_mean_temp_oct_dec <- subset( daily_mean_temp , 304:365)
  daily_mean_temp_jan_april <- subset(daily_mean_temp, 1:120) #pull out the days counting from start of year to 30th april 
  daily_mean_temp_GS <- stack(daily_mean_temp_oct_dec, daily_mean_temp_jan_april) #this should be 182 n layers 
  GS_mean_leap_yrs <- mean(daily_mean_temp_GS)
  
}

function_mean_temp_by_nonleap_yr <- function(year_input) {
  
  min <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/min_temp/",
          year_input, ".min_temp.nc", sep = ""),varname = "min_temp")
  max <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/max_temp/",
          year_input , ".max_temp.nc", sep = ""),varname = "max_temp")
  
  daily_mean_temp <- overlay(min, max, fun = function_daily_mean_temp)
  daily_mean_temp_oct_dec <- subset( daily_mean_temp , 303:364)
  daily_mean_temp_jan_april <- subset(daily_mean_temp, 1:119) #pull out the days counting from start of year to 1st april 
  daily_mean_temp_GS <- stack(daily_mean_temp_oct_dec, daily_mean_temp_jan_april) #his should be 181 n layers
  GS_mean_leap_yrs <- mean(daily_mean_temp_GS)
  
}


#### I want to loop through all years and make mean temp raster #######

#because we have leap years and the satrt number and end number of 1st Oct is not the same its 303 or 304
leap_years <- c( "1992", "1996","2000", "2004" ,"2008", "2012", "2016")


non_leap_years <- c("1989", "1990" ,"1991",  "1993", "1994" ,"1995", "1997",
                    "1998" ,"1999" ,"2001", "2002", "2003", "2005", "2006",
                    "2007" , "2009", "2010", "2011" , "2013", "2014" ,"2015" ,
                    "2017" ,"2018")


#make loop  that created a raster of daily mean temp year one for leap years and one for non leap years
for (i in leap_years) {
  assign(paste0("mean_temp_leap_yrs", i), function_mean_temp_by_leap_yr(i))
}

for (i in non_leap_years) {
  assign(paste0("mean_temp_nonleap_yrs", i), function_mean_temp_by_nonleap_yr(i))
}









#### stack all these raster into one raster and run mean on it...

GS_leap_yrs <- stack(mean_temp_leap_yrs1992,
                     mean_temp_leap_yrs1996,
                     mean_temp_leap_yrs2000,
                     mean_temp_leap_yrs2004,
                     mean_temp_leap_yrs2008,
                     mean_temp_leap_yrs2012,
                     mean_temp_leap_yrs2016)

GS_nonleap_yrs <- stack(mean_temp_nonleap_yrs1989,
                        mean_temp_nonleap_yrs1990,
                        mean_temp_nonleap_yrs1991,
                        mean_temp_nonleap_yrs1993,
                        mean_temp_nonleap_yrs1994,
                        mean_temp_nonleap_yrs1995,
                        mean_temp_nonleap_yrs1997,
                        mean_temp_nonleap_yrs1998,
                        mean_temp_nonleap_yrs1999,
                        mean_temp_nonleap_yrs2001,
                        mean_temp_nonleap_yrs2002,
                        mean_temp_nonleap_yrs2003,
                        mean_temp_nonleap_yrs2005,
                        mean_temp_nonleap_yrs2006,
                        mean_temp_nonleap_yrs2007,
                        mean_temp_nonleap_yrs2009,
                        mean_temp_nonleap_yrs2010,
                        mean_temp_nonleap_yrs2011,
                        mean_temp_nonleap_yrs2013,
                        mean_temp_nonleap_yrs2014,
                        mean_temp_nonleap_yrs2015,
                        mean_temp_nonleap_yrs2017,
                        mean_temp_nonleap_yrs2018)

GS_nonleap_leap_yrs <- stack(GS_nonleap_yrs, GS_leap_yrs)
GS_nonleap_leap_yrs_mean <- mean(GS_nonleap_leap_yrs)
GS_nonleap_leap_yrs_mean
plot(GS_nonleap_leap_yrs_mean)
# #Write
#writeRaster(GS_nonleap_leap_yrs_mean, "GS_nonleap_leap_yrs_mean",format = "GTiff", overwrite = TRUE) 

############# Perhaps can do this???

#for(i in 1:length(GS_leap_yrs)){
#  STACK1 <- stack(GS_leap_yrs)
#  mean_GS_leap_yrs <- calc(STACK1, fun = mean, na.rm = T)
#}



############### Step 2 with barossa data###################################################################
library(sf)

barrossa_st <- st_read("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/Vine_health_data/CSIRO/GI/ZONE/barossa_WGS.shp")
barrossa_sf <- as(barrossa_st, "Spatial") #convert to a sp object
#might need to fix this up extent is not quite right
#perhaps also try re projecting in R to GDA

GS_nonleap_leap_yrs_mean_c <- crop(GS_nonleap_leap_yrs_mean, barrossa_sf)
GS_nonleap_leap_yrs_mean_c
plot(GS_nonleap_leap_yrs_mean_c)

#GS_nonleap_leap_yrs_mean_m <- mask(GS_nonleap_leap_yrs_mean_c, barrossa_sf)
#GS_nonleap_leap_yrs_mean_m
#plot(GS_nonleap_leap_yrs_mean_m)


# #Write
writeRaster(GS_nonleap_leap_yrs_mean_c, 
            "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/GS_temp_1989_2018",
            format = "GTiff", overwrite = TRUE) #average jan temp for 30yrs



#########################################################################################################################
##############                 end of code for GS leap years mean                     ######################################
#########################################################################################################################























  









































# set up input files source ------------------------------------------------------------------
raster_file_list <- list.files( path = '//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/input_data',
                                pattern = ".nc" , all.files = FALSE , full.names = FALSE )
raster_file_list



# inputs data range------------------------------------------------------------------
#starting year of climate record

start_year <- '1988' #4 years of data
#start_year <- '1993' #4 years of data
#start_year <- '1994' #4 years of data
#start_year <- '1999' #4 years of data
#start_year <- '2004' #4 years of data
#start_year <- '2008' #4 years of data
#start_year <- '2012' #4 years of data
#start_year <- '2016' #4 years of data

#this uses the a list of the raster and drops the file name only keeping the year
#then using the start date I have specified from start_year works out the place in the list
#for example if the start year was 1890 then I would get a value of 1
#for 1988 it is value of 99, for 2015 it is 126
i_start <- which(substring(raster_file_list, 1,4) == start_year)
print(i_start)
#this is the place in the the list that the start year appears

print(raster_file_list[i_start])


#=========================================
# Compute annual grids ----------------------------------------------------


#Start loop on annual monthly nc files-------------------------
b_all <- stack()
b_ann <- stack()
#two empty rasters
b_all
b_ann
####################### step 1 seems to be the sum of rainfall###################################################################
#Loop start year
for (i in (i_start : length(raster_file_list))) {
  
  print(paste("Processing rasters for ", raster_file_list[i], sep = ""))
  
  ##Open netcdf as raster brick -------------------------------
  b <- brick(
        paste("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/input_data/",
                raster_file_list[i], sep = ""),varname = "monthly_rain")
  mons <- c("01", "02", "03","04", "05","06", "07", "08","09", "10", "11", "12")
  #Extract year from file
  yr <- as.integer(substr(raster_file_list[i],1,4))
  mon_yr <- paste(yr, ".", mons, sep ="")
  #Add to existing brick
  names(b) <- mon_yr
  #Add brick to long-term stack
  b_all <-stack(b_all, b) 
  
  
}


############### Step 2 ###################################################################

yrs <- seq(1988,2018, by =1)




############### Step 2 with barossa data###################################################################
library(sf)
barrossa_st <- st_read("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/Vine_health_data/CSIRO/GI/baroosa_ext_WGS.shp")
barrossa_sf <- as(barrossa_st, "Spatial") #convert to a sp object

b #last raster that was added?

# all the month is for the years specified eg 24 month for 2 years of data
ball_c <- crop(b_all, barrossa_sf)
ball_c
plot(ball_c)

ball_m <- mask(ball_c, barrossa_sf)
ball_m
plot(ball_m)


#### step 3 Calculations on monthly time series--------------------------------------------

#get the date from the names of the layers and extract the month
indices <- rep(seq(1,12, by=1), length(yrs))
indices
#sum the layers
#bmon <- stackApply(ball_m, indices, fun = mean)

bmon <- stackApply(ball_m, indices, fun = mean) #average rainfall for each month
bmon
#Mean annual and monthly rain layers---------------------

bann <- sum(bmon) #sum of average rainfall for each month
bann

bwin <- sum(bmon[[4:10]]) #sum of average rainfall for april to oct
bwin
plot(bwin)

b_jan <- bmon[[1]] #average jan rainfall 
b_jan

# #Write
writeRaster(bmon, "bmon1988_2018.tif",format = "GTiff", overwrite = TRUE) #average rainfall for each month 30yrs
writeRaster(bann, "bann1988_2018.tif",format = "GTiff", overwrite = TRUE) #sum of average rainfall for each month 30yrs
writeRaster(bwin, "bwin1988_2018.tif",format = "GTiff", overwrite = TRUE) #average rainfall for april to oct 30 yrs
writeRaster(b_jan, "b_jan.tif",format = "GTiff", overwrite = TRUE) ##average jan rainfall 30yrs



# End code ----------------------------------------------------

