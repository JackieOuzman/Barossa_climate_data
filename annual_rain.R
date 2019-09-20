
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
options('download.file.method'='curl')

#Change options for raster temp storage
rasterOptions(format="CDF",overwrite=TRUE,maxmemory = 1e+09, chunksize=1e+08,progress="text",tmpdir="C:/")
rasterTmpFile("clean_this_after_")

##Setup dirs -------------------------------------------


getwd()

#this is not working? I only have read access could that be the problem?
WD <- setwd(as.character('//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/climate/GriddedRain'))

#WD <- setwd(as.character('//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/input_data'))
nc_dir_mon <- as.character('//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/climate/GriddedRain')
nc_dir_day <- as.character('//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/daily_rain')

raster_file_mon <- list.files( path = nc_dir_mon,
                               pattern = ".nc" , all.files = FALSE , full.names = FALSE )

#nc_out <- paste('//af-climdecteam-work.dataset.csiro.au/af_climdecteam_work/Projects/DAS/map_layers', sep="")
nc_out <- paste('//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers', sep="")

try(dir.create(nc_out))


# inputs ------------------------------------------------------------------
#starting year of climate record
#start_year <- '1951'
#start_year <- '1988'#want this for final data
start_year <- '2015' #test on subset

#this uses the a list of the raster and drops the file name only keeping the year
#then using the start date I have specified from start_year works out the place in the list
#for example if the start year was 1890 then I would get a value of 1
#for 1988 it is vlue of 99, for 2015 it is 126
i_start <- which(substring(raster_file_mon, 1,4) == start_year)
print(i_start)
print(raster_file_mon)


#=========================================
# Compute annual grids ----------------------------------------------------

##Setup dirs --------------------------------------------

WD<- setwd(as.character("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/input_data"))
WD
#WD <- setwd(as.character('//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/climate/GriddedRain'))
nc_dir_mon <- as.character('//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/climate/GriddedRain')

#--------------------------------------------------------
raster_file_list <- list.files( path = WD,
                                pattern = ".nc" , all.files = FALSE , full.names = FALSE )

print(raster_file_list)
#
#Start loop on annual monthly nc files-------------------------
b_all <- stack()
b_ann <- stack()


####################### step 1 ###################################################################
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
  yr <- as.integer(substr(raster_file_list[i],18,21))
  mon_yr <- paste(yr, ".", mons, sep ="")
  #Add to existing brick
  names(b) <- mon_yr
  #Add brick to long-term stack
  b_all <-stack(b_all, b) 
  b_sum <- sum(b_all)
  b_ann <- stack(b_ann, b_sum)
  
}





############### Step 2 ###################################################################
#yrs <- seq(1950,2016, by =1)
yrs <- seq(2015,2018, by =1)
oz <- getData('GADM', country='AUS', level=1)
ball_c <- crop(b_all, oz)
ball_c
ball_m <- mask(ball_c, oz)
ball_m



# writeRaster(ball_m, paste(nc_dir,"/1950_2016_monthly_rain2.nc", sep=""), "CDF", overwrite=TRUE,
#             varname="rain",varunit="mm",longname="Monthly rain data",
#             xname="lon",yname="lat", zname="year.mon", zunit="month",bylayer=FALSE)

#Calculations on monthly time series--------------------------------------------
# nc <- nc_open("1950_2016_mean_monthly2.nc")  
#get the date from the names of the layers and extract the month
indices <- rep(seq(1,12, by=1), length(yrs))

#sum the layers
#bmon <- stackApply(ball_m, indices, fun = mean)
bmon <- stackApply(ball_c, indices, fun = mean)
#Mean annual and monthly rain layers---------------------
bmon <- brick("1950_2016_mean_monthly.nc", varname = "rain")
bann <- sum(bmon)

# writeRaster(bmon, paste(nc_dir,"/1950_2016_mean_monthly.nc", sep=""), "CDF", overwrite=TRUE,
#             varname="rain",varunit="mm",longname="Monthly rain data",
#             xname="lon",yname="lat", zname="month", zunit="month",NAflag=-9999)
writeRaster(bann, paste(nc_dir,"/ann_rainfall_1950_2017.nc", sep=""), "CDF", overwrite=TRUE,
            varname="rainfall",varunit="mm",longname="Mean annual rainfall",
            xname="lon",yname="lat", zname="annual", zunit="",NAflag=-9999)
# #Write
# writeRaster(bmon, paste(nc_dir,"/1950_2016_mean_monthly.tif", sep=""),format = "GTiff", overwrite = TRUE)
# writeRaster(bann, paste(nc_dir,"/1950_2016_mean_annual.tif", sep=""),format = "GTiff", overwrite = TRUE)

# ##Calc winter and summer wet season rain -----------------------------            
# bwin <- sum(bmon[[4:10]])
# bsum <- sum(bmon[[c(1:3,10:12)]])

# #Calc trends/percent change in two time periods
# #Calc total rain each year
# indices2 <- rep(1:length(yrs), each = 12)
# ball_a <- stackApply(ball_m, indices2, fun = sum) 
# names(ball_a) <- seq(1950, 2016 , by =1)
# writeRaster(bann, paste(nc_dir,"/1950_2016_annual_rain.nc", sep=""), "CDF", overwrite=TRUE,
#             varname="rain",varunit="mm",longname="Annual rain data",
#             xname="lon",yname="lat", zname="annual", zunit="year",NAflag=-9999)
# 
# yr <- substring(names(ball_a),2,5)
# old_yrs <-seq(1950, 1979 , by =1)
# new_yrs <-seq(1980, 2016 , by =1)
# b_old <- mean(ball_a[[which(yr %in% old_yrs)]])
# b_new <- mean(ball_a[[which(yr %in% new_yrs)]])
# b_mean_ann <- mean(ball_a)
# b_ann_tr <- ((b_new/b_mean_ann)-1)*100
# writeRaster(b_ann_tr, paste(nc_dir,"/1980_2016_annual_change.nc", sep=""), "CDF", overwrite=TRUE,
#             varname="rain",varunit="percent",longname="percent change of 1980-2016 rain from 1950-1979",
#             xname="lon",yname="lat", zname="annual", zunit="year",NAflag=-9999)
# writeRaster(b_ann_tr, filename="1980_2016_annual_change2.tif",format = "GTiff", overwrite = TRUE)




# End code ----------------------------------------------------

