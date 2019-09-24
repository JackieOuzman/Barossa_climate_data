
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
# Compute temp variables ------------------------------------------------------


raster_file_list_min <- list.files( path = '//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/min_temp',
                                pattern = ".nc" , all.files = FALSE , full.names = FALSE )
raster_file_list_max <- list.files( path = '//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/max_temp',
                                pattern = ".nc" , all.files = FALSE , full.names = FALSE )

raster_file_list_min
raster_file_list_max
start_year <- '2018' 
i_start <- which(substring(raster_file_list, 1,4) == start_year)
print(i_start)
print(raster_file_list_min[i_start])
print(raster_file_list_max[i_start])

#Start to understand loop for Jan temp nc files-------------------------


#by year 2018
min_2018 <- brick(
  "//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/min_temp/2018.min_temp.nc",varname = "min_temp")
max_2018 <- brick(
  "//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/max_temp/2018.max_temp.nc",varname = "max_temp")
min_2018 # this is a raster with 365 n layer which relate to each day of the year for 2018
max_2018 # this is a raster with 365 n layer which relate to each day of the year for 2018

function_daily_mean_temp <- function(min, max) {
  daily_mean_temp <- (min +max)/2
  return(daily_mean_temp)
}

daily_mean_temp <- overlay(min_2018, max_2018, fun = function_daily_mean_temp)
daily_mean_temp #this is one raster with 365 layers 1 for each day

daily_mean_temp_jan2018 <- subset(daily_mean_temp, 1:30) #pull out the first 30 days of mean temp ? should this be 31??
daily_mean_temp_jan2018

av_jan_mean_temp_2018 <- mean(daily_mean_temp_jan2018) #average jan mean temp 
av_jan_mean_temp_2018



#-----  bad loops / functions by jaxs
#List of years to use as loop
#start_year <- "1999"
start_year <- "2017"
end_year <- "2018"
start_year_i <- which(substring(raster_file_list_min, 1,4) == start_year)
end_year_i <- which(substring(raster_file_list_min, 1,4) == end_year)
Jax_list_placement_start <- substr(raster_file_list_min[start_year_i],1,4)
Jax_list_placement_start
Jax_list_placement_end <- substr(raster_file_list_min[end_year_i],1,4)
Jax_list_placement_end

Jax_list <- c(substr(start_year_i,1,4): substr(end_year_i,1,4))
Jax_list #this is a list of the files I want to use and there placement


#as a function per year
#function one
function_daily_mean_temp <- function(min, max) {
  daily_mean_temp <- (min +max)/2
  return(daily_mean_temp)
}
#function two
function_jan_mean_temp_by_yr <- function(year_input) {
  
  min <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/min_temp/",
          year_input, ".min_temp.nc", sep = ""),varname = "min_temp")
  max <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/min_temp/",
          year_input , ".min_temp.nc", sep = ""),varname = "min_temp")
  
  daily_mean_temp <- overlay(min, max, fun = function_daily_mean_temp)
  daily_mean_temp_jan <- subset(daily_mean_temp, 1:30) #pull out the first 30 days of mean temp ? should this be 31??
  av_jan_mean_temp <- mean(daily_mean_temp_jan)
}

jan_2018 <- function_jan_mean_temp_by_yr("2018")
jan_2018


### list of years ####
jax_list <- c("2016", "2017", "2018")
jax_list_numb <- c(1:10)
jax_list_numb
u1 <- rnorm(30)
empty_data_frame <- data.frame()
print("This loop calculates the square of the first 10 elements of empty_data_frame")

# Initialize `usq`
data <- 0
str(data)

for(i in jax_list_numb) {
  # i-th element of `u1` squared into `i`-th position of `usq`
  data[i] <- 10+i
  return(data)
  
}

print(i)
print(data)
str(data)

######################################################################################################################
#############    suss out the logic of what I want to do   ##########################################################
######################################################################################################################
#by year 2018
#bring in the .nc file
min_2018 <- brick(
  "//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/min_temp/2018.min_temp.nc",varname = "min_temp")
max_2018 <- brick(
  "//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/max_temp/2018.max_temp.nc",varname = "max_temp")
min_2018 # this is a raster with 365 n layer which relate to each day of the year for 2018
max_2018 # this is a raster with 365 n layer which relate to each day of the year for 2018
#make a function to do raster cal
function_daily_mean_temp <- function(min, max) {
  daily_mean_temp <- (min +max)/2
  return(daily_mean_temp)
}

daily_mean_temp <- overlay(min_2018, max_2018, fun = function_daily_mean_temp)
daily_mean_temp #this is one raster with 365 layers 1 for each day

daily_mean_temp_jan2018 <- subset(daily_mean_temp, 1:30) #pull out the first 30 days of mean temp ? should this be 31??
daily_mean_temp_jan2018

av_jan_mean_temp_2018 <- mean(daily_mean_temp_jan2018) #average jan mean temp 
av_jan_mean_temp_2018




#as loop for Jan temp nc files-------------------------


















#Start loop on annual nc files-------------------------
min_all <- brick()
min_all #not sure why the default is this the rainfall data has stack eg. min_all <- stack()
for (i in (i_start : length(raster_file_list))) {
#for (i in 1 :length(raster_file_list)) {
  
  print(paste("Processing rasters for ", raster_file_list[i], sep = ""))
  
  b <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/min_temp",
          raster_file_list[i], sep = ""),varname = "min_temp")
  days <- c("01", "02", "03","04", "05","06", "07", "08","09", "10", "11", "12")
  
  
  # # define the name of a temp directory where raster tmp files will be stored
  raster_tmp_dir <- paste("//af-climdecteam-work.dataset.csiro.au/af_climdecteam_work/Projects/DAS/raster_temp/",raster_file_list[i], sep= "")
  # # create the directory
  dir.create(raster_tmp_dir, showWarnings = F, recursive = T)
  rasterOptions(tmpdir =file.path(raster_tmp_dir))
  
  #Open netcdf as raster brick -------------------------------
  b <- brick(paste(nc_dir,"/",raster_file_list[i], sep = ""),varname = "min_temp")
  # e <- extent(144, 148, -44.1, -40)
  # bc <- crop(b, e)
  
  #Extract year from file
  yr <- as.integer(substr(raster_file_list[i],1,4))
  #reclassify min temp < 0 to 1 and >0 to 0
  fr <- reclassify(b, c(-Inf,0,1,  0.1,Inf,0))
  
  #Sum all days in stack
  frsum <- sum(fr, na.rm = TRUE, file)
  unlink(paste(raster_tmp_dir,"/*.gri",sep = ""), recursive =F, force = T)
  
  #Create new raster dir
  rasterOptions(tmpdir =file.path(raster_tmp_dir))
  names(frsum) <- yr
  frall <- stack(frsum,frall)
  
}









f<- frall[[2:20]]
#Write
# writeRaster(f, paste(nc_dir,"/1950_2016_daily_frostdays.tif", sep=""),format = "GTiff", overwrite = TRUE,
#             bylayer = FALSE)
# #WOrkaround for issues
# frall1 <- stack(paste(nc_dir,"/frost_days_19512008.nc", sep = ""),varname = "frostdays")
# frall2 <- brick(paste(nc_dir,"/frost_days_20052016.nc", sep = ""),varname = "frostdays")
# frall3 <- frall1[[4:59]]
# frall4 <- stack(frall2, frall3)
# fmean <- mean(frall4)

#Crop and mask to oz coastline
oz <- getData('GADM', country='AUS', level=1)
fmean_c <- crop(fmean, oz)
fmean_m <- mask(fmean_c, oz)

#Write
writeRaster(fmean_m, paste(nc_dir,"/ann_frostdays_1950_2016.nc", sep=""), format = "CDF", overwrite=TRUE,
            varname="frost days",longnams = "Mean number of days per year < 0 degC", varunit="days",xname="lon",yname="lat",NAflag=-9999)

# End code ----------------------------------------------------










































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

