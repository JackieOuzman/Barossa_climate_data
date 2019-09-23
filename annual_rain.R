
##Aggregate SILO daily into monthly and annual and build map layers
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

#Change options
#options('download.file.method'='curl')

#######Change options for raster temp storage
#rasterOptions(format="CDF",overwrite=TRUE,maxmemory = 1e+09, chunksize=1e+08,progress="text",tmpdir="C:/")
#rasterTmpFile("clean_this_after_")

##Setup dirs -------------------------------------------

#this is not working? I only have read access could that be the problem?
#WD <- setwd(as.character('//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/climate/GriddedRain'))

#WD <- setwd(as.character('//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/input_data'))
#nc_dir_mon <- as.character('//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/climate/GriddedRain')
#nc_dir_day <- as.character('//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/daily_rain')

#raster_file_mon <- list.files( path = nc_dir_mon,
#                               pattern = ".nc" , all.files = FALSE , full.names = FALSE )

raster_file_list <- list.files( path = '//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/input_data',
                                pattern = ".nc" , all.files = FALSE , full.names = FALSE )
raster_file_list

#nc_out <- paste('//af-climdecteam-work.dataset.csiro.au/af_climdecteam_work/Projects/DAS/map_layers', sep="")
#nc_out <- paste('//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers', sep="")
#nc_out
#try(dir.create(nc_out)) #says its already made but I can't see it


# inputs ------------------------------------------------------------------
#starting year of climate record
#start_year <- '1951'
#start_year <- '1988'#want this for final data
start_year <- '2017' #test on subset

#this uses the a list of the raster and drops the file name only keeping the year
#then using the start date I have specified from start_year works out the place in the list
#for example if the start year was 1890 then I would get a value of 1
#for 1988 it is vlue of 99, for 2015 it is 126
i_start <- which(substring(raster_file_list, 1,4) == start_year)
print(i_start)
#this is the place in the the list that the start year appears

print(raster_file_list[i_start])
#raster_file_list
#all the rasters in the folder


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
    #paste(WD,"/monthly_nc/",raster_file_list[i], sep = ""),varname = "rain") #something wrong with file path now hard coded, also changed rain to monthly rain
    
    paste("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/input_data/",
                raster_file_list[i], sep = ""),varname = "monthly_rain")
  mons <- c("01", "02", "03","04", "05","06", "07", "08","09", "10", "11", "12")
  #Extract year from file
  #yr <- as.integer(substr(raster_file_list[i],18,21))
  yr <- as.integer(substr(raster_file_list[i],1,4))
  mon_yr <- paste(yr, ".", mons, sep ="")
  #Add to existing brick
  names(b) <- mon_yr
  #Add brick to long-term stack
  b_all <-stack(b_all, b) 
  b_sum <- sum(b_all)
  b_ann <- stack(b_ann, b_sum)
  
}



##########Checking stuff run the loop at line of code to check
#raster_file_list
#yr <-as.integer(substr(raster_file_list,1,4))

#mons <- c("01", "02", "03","04", "05","06", "07", "08","09", "10", "11", "12")
#mon_yr <- paste(yr, ".", mons, sep ="")
#print(mon_yr)




############### Step 2 ###################################################################
#yrs <- seq(1950,2016, by =1)
yrs <- seq(2017,2018, by =1)




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

#this has nlayer related to the years of data 
# check b_ann <- crop(b_ann, barrossa_sf)
# check b_ann <- mask(b_ann, barrossa_sf)
# checkplot(b_ann)
# check b_ann

# checkb_all <- crop(b_all, barrossa_sf)
# checkb_all <- mask(b_all, barrossa_sf)
# checkb_sum <- crop(b_sum, barrossa_sf)
# checkb_sum <- mask(b_sum, barrossa_sf)
# checkb_sum



#write the files to check in arcmap

# check writeRaster(b_ann, 'b_ann20172018.tif',format = "GTiff", overwrite = TRUE)
# check writeRaster(b_all, 'b_all20172018.tif',format = "GTiff", overwrite = TRUE)
# check writeRaster(b_sum, 'b_sum20172018.tif',format = "GTiff", overwrite = TRUE)



#Calculations on monthly time series--------------------------------------------
# nc <- nc_open("1950_2016_mean_monthly2.nc")  
#get the date from the names of the layers and extract the month
indices <- rep(seq(1,12, by=1), length(yrs))
indices
#sum the layers
#bmon <- stackApply(ball_m, indices, fun = mean)
#I think ball_m is the same as cropped and masked b_all
bmon <- stackApply(ball_m, indices, fun = mean)
bmon
#Mean annual and monthly rain layers---------------------
#bmon <- brick("2015_2018_mean_monthly.nc", varname = "rain")#this is not a file I have

bann <- sum(bmon)
bann

bwin <- sum(bmon[[4:10]])
bwin
plot(bwin)

b_jan <- bmon[[1]]
b_jan

# #Write
writeRaster(bmon, "bmon2017_2018.tif",format = "GTiff", overwrite = TRUE)
writeRaster(bann, "bann2017_2018.tif",format = "GTiff", overwrite = TRUE)
writeRaster(bwin, "bwin2017_2018.tif",format = "GTiff", overwrite = TRUE)
writeRaster(b_jan, "b_jan.tif",format = "GTiff", overwrite = TRUE)



# End code ----------------------------------------------------

