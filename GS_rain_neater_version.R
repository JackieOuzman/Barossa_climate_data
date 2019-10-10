
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

# set up input files source ------------------------------------------------------------------
raster_file_list <- list.files( path = '//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/input_data',
                                pattern = ".nc" , all.files = FALSE , full.names = FALSE )
raster_file_list



# inputs data range------------------------------------------------------------------
#starting year of climate record

start_year <- '1989' #4 years of data
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
b_all

############### Step 2 ###################################################################

yrs <- seq(1989,2018, by =1)




############### Step 2 with barossa data###################################################################
library(sf)
barrossa_st <- st_read("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/Vine_health_data/CSIRO/GI/ZONE/barossa_WGS.shp")
barrossa_sf <- as(barrossa_st, "Spatial") #convert to a sp object

b #last raster that was added?

# all the month is for the years specified eg 24 month for 2 years of data
ball_c <- crop(b_all, barrossa_sf)
ball_c
plot(ball_c)

#ball_m <- mask(ball_c, barrossa_sf)
#ball_m
#plot(ball_m)


#### step 3 Calculations on monthly time series--------------------------------------------

#get the date from the names of the layers and extract the month
indices <- rep(seq(1,12, by=1), length(yrs))
indices
#sum the layers
#bmon <- stackApply(ball_m, indices, fun = mean)

bmon <- stackApply(ball_c, indices, fun = mean) #average rainfall for each month
bmon
#Mean annual and monthly rain layers---------------------

bann <- sum(bmon) #sum of average rainfall for each month
bann

rain_jan_april <- sum(bmon[[1:4]]) #sum of average rainfall 
rain_oct_dec <- sum(bmon[[10:12]]) #sum of average rainfall 
rain_sep_dec <- sum(bmon[[9:12]]) #s  ###NOT RUN
rain_gs <-sum(rain_jan_april, rain_oct_dec)
rain_gs_sep <-sum(rain_jan_april, rain_sep_dec) ###NOT RUN
plot(rain_gs)



# #Write
writeRaster(bann, 
            "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/bann.tif",
            format = "GTiff", overwrite = TRUE) #sum of average rainfall for each month
writeRaster(rain_gs, 
            "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/rain_gs",
            format = "GTiff", overwrite = TRUE) #sum of average GS rainfall for each month 30yrs
#writeRaster(bwin, "bwin1988_2018.tif",format = "GTiff", overwrite = TRUE) #average rainfall for april to oct 30 yrs
#writeRaster(b_jan, "b_jan.tif",format = "GTiff", overwrite = TRUE) ##average jan rainfall 30yrs



# End code ----------------------------------------------------

