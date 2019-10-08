
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

#===========================
# Compute temp variables ------------------------------------------------------




############################################################################################################################
################### Start here ############################################################################################
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
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/max_temp/",
          year_input , ".max_temp.nc", sep = ""),varname = "max_temp")
  
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

jan_temp2016 #these are raster made in the loop  now called - jan_temp2016 , jan_temp2017, jan_temp2018
jan_temp1989
jan_temp1990


#Now cal the average for each cell using the input raster

#means <- paste0("jan_temp",(as.character(c(2016:2018)))) #subset to make sure it works
#means <- paste0("jan_temp",(as.character(c(1989:1990)))) #full dataset
#means
STACK1 <- stack(jan_temp1989, jan_temp1990, jan_temp1991, jan_temp1992, jan_temp1993, jan_temp1994,
                jan_temp1995, jan_temp1996, jan_temp1997, jan_temp1998, jan_temp1999, jan_temp2000,
                jan_temp2001, jan_temp2002, jan_temp2003, jan_temp2004, jan_temp2005, jan_temp2006,
                jan_temp2007, jan_temp2008, jan_temp2009, jan_temp2010, jan_temp2011, jan_temp2012,
                jan_temp2013, jan_temp2014, jan_temp2015, jan_temp2016, jan_temp2017, jan_temp2018)
means_jan_temp <- calc(STACK1, fun = mean, na.rm = T)
means_jan_temp

#for(i in 1:length(means)){
#  STACK1 <- stack(means)
#  means_jan_temp <- calc(STACK1, fun = mean, na.rm = T)
#}



############### Step 2 with barossa data###################################################################
library(sf)

barrossa_st <- st_read("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/Vine_health_data/CSIRO/GI/ZONE/barossa_WGS.shp")
barrossa_sf <- as(barrossa_st, "Spatial") #convert to a sp object
#might need to fix this up extent is not quite right
#perhaps also try re projecting in R to GDA

means_jan_temp_c <- crop(means_jan_temp, barrossa_sf)
means_jan_temp_c
plot(means_jan_temp)

means_jan_temp_m <- mask(means_jan_temp_c, barrossa_sf)
means_jan_temp_m
plot(means_jan_temp_m)


# #Write
writeRaster(means_jan_temp_c, "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/means_jan_temp_1989_2018",format = "GTiff", overwrite = TRUE) #average jan temp for 30yrs

  

#########################################################################################################################
##############                 end of code for mean jan temp                       ######################################
#########################################################################################################################



#########################################################################################################################
####                           Try a  jan temp for the raster for each year
#########################################################################################################################


##### clip the grids for each year to the study area

STACK1_meanjan_temp <- stack(jan_temp1989, jan_temp1990, jan_temp1991, jan_temp1992, jan_temp1993, jan_temp1994,
                jan_temp1995, jan_temp1996, jan_temp1997, jan_temp1998, jan_temp1999, jan_temp2000,
                jan_temp2001, jan_temp2002, jan_temp2003, jan_temp2004, jan_temp2005, jan_temp2006,
                jan_temp2007, jan_temp2008, jan_temp2009, jan_temp2010, jan_temp2011, jan_temp2012,
                jan_temp2013, jan_temp2014, jan_temp2015, jan_temp2016, jan_temp2017, jan_temp2018)



STACK1_meanjan_temp_c <- crop(STACK1_meanjan_temp, barrossa_sf)
STACK1_meanjan_temp_c
plot(STACK1_meanjan_temp_c)

STACK1_meanjan_temp_m <- mask(STACK1_meanjan_temp_c, barrossa_sf)
STACK1_meanjan_temp_m
plot(STACK1_meanjan_temp_m)

#unstack(STACK1_meanjan_temp_m) # this is mean jan temp grids for the barossa area only
#list2env(setNames(unstack(STACK1_meanjan_temp_m), names(STACK1_meanjan_temp_m)), .GlobalEnv)
#plot(layer.1)

#this is a barossa modified grid as a series of points (shapefile)
barrossa_st_extract <- st_read("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/extract_jan_temp_yrs_WGS.shp")
barrossa_extract_sf <- as(barrossa_st_extract, "Spatial") #convert to a sp object
class(barrossa_extract_sf)
plot(barrossa_extract_sf)
library(raster)
library(rasterVis)
plt <- levelplot(layer.1, margin=F, 
                 main="Mean Jan temp for first year")
plt + layer(sp.points(barrossa_extract_sf, col="black", pch=16, cex=0.5))

crs(barrossa_extract_sf)
crs(layer.1)
mean_jan_temp_extract <- extract(STACK1_meanjan_temp, barrossa_extract_sf, method="simple")
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
#library(dplyr)
#library(tidyverse)
pts_jan_temp_narrow <- gather(pts_jan_temp_wide, key = "year", value = "Mean_Jan_temp", `1989`:`2018` )
head(pts_jan_temp_narrow)

#library(ggplot2)
ggplot(pts_jan_temp_narrow, aes(Mean_Jan_temp))+
  geom_density()+
  facet_wrap(.~year)



ggplot(pts_jan_temp_narrow, aes(year, Mean_Jan_temp))+
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        plot.caption = element_text(hjust = 0))+
  labs(x = "Year",
       y = "Mean Jan temperature",
       title = "Sample points over the Barossa",
       caption = "First the mean January temperature is calculated for each pixel by year, then the values for each pixel is extracted point by point. This is achieved by using the Barossa modified boundary and converting it into a shapefile
       ")

























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





















##### Stuff around with transforming raster

barrossa_st_GDA <- st_read("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/Vine_health_data/CSIRO/GI/ZONE/barossa.shp")
barrossa_sf <- as(barrossa_st_GDA, "Spatial") #convert to a sp object
#might need to fix this up extent is not quite right
st_crs(barrossa_st_GDA) <- 28354
barrossa_st_GDA 
barrossa_st_GDA_transform <- st_transform(barrossa_st_GDA, crs = 28354)
barrossa_st_GDA_transform
barrossa_sf
the_crs <- crs(barrossa_st_GDA_transform, asText= TRUE)  
the_crs
old_crs <- crs(av_jan_mean_temp2017, asText= TRUE)  
old_crs
check <- projectRaster(av_jan_mean_temp2017, crs = crs(barrossa_st_GDA_transform))
check
writeRaster(check, 
            "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/checkmeans_jan_temp_2017",
            format = "GTiff", overwrite = TRUE) #average jan temp for 1yr





















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

