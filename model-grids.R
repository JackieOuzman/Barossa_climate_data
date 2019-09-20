
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
start_year <- '1951'
#start_year <- '1988'#want this for final data
#start_year <- '2015' #test on subset

#this uses the a list of the raster and drops the file name only keeping the year
#then using the start date I have specified from start_year works out the place in the list
#for example if the start year was 1890 then I would get a value of 1
#for 1988 it is vlue of 99, for 2015 it is 126
i_start <- which(substring(raster_file_mon, 1,4) == start_year)
#print(i_start)
#print(raster_file_mon)


# Code --------------------------------------------------------------------
b_all1 <- stack()
#stack is a collection of raster layer empty for now


for (i in (i_start :length(raster_file_mon))) {
  print(paste("Processing rasters for ", raster_file_mon[i], sep = ""))
  ##Open netcdf as raster brick -------------------------------
  b <- brick(paste(nc_dir_mon, "/",raster_file_mon[i], sep = ""),varname = "monthly_rain")
  #Add brick to long-term stack
  b_all1 <-stack(b_all1, b) 
}

#this put each month for all the years into a large raster stack called b_all1

#plot(b_all1)


  #Get current dates info
  yr <- year(Sys.Date())
  mon <- month(Sys.Date())-1
  mon2 <- month.abb[mon]
  analysis_yrs <- seq(1950, yr-1, by =1)
  #analysis_yrs <- seq(2014, yr-1, by =1)
  #this creates a list of years using the years before start year - hard coded
  #head(analysis_yrs)
  #str(analysis_yrs)
  
  #Input dirs
  # input_dir <-historic_rainfall_data_baseurl
  
  #Open the most recent
  #daily rainfall data - Jax I am not sure I need this
  print('Opening recent files')
  raster_file_recent <- list.files(path = nc_dir_day,pattern = ".nc" , all.files = FALSE , full.names = FALSE)
  curr_nc <- raster_file_recent[which(substr(raster_file_recent, start = 1, stop = 4)== yr)]
  #print(raster_file_recent)
  #print(curr_nc)
  
  ##Open netcdf as raster brick -------------------------------
  print("Getting current ncdf")
  b1 <- brick(paste(nc_dir_day, "/",raster_file_recent[which(substr(raster_file_recent, start = 1, stop = 4)== yr)], sep = ""),
              varname = "daily_rain")
  ##Extract year from layer names in raster brick
  b_names <- names(b1)
  nc_mons <- substr(b_names, 7,8)
  #Calc cum rain for each grid cell by month, stack and write to netcdf----------------------
  months <- seq(1,mon,1)
  #Add leading zeros
  months <- sprintf("%02d",months)
  
  #Create empty stack to add new layers to
  b_mon <- stack()
  for (m in 1:length(months)){
    mon_rain <-  which(nc_mons == months[m])
    #ras <- paste("mon_", m, sep="")
    mon_sum <- (sum(b1[[mon_rain]]))
    names(mon_sum) <- c(paste('X',yr,'.', months[m], '.01',sep = ''))  
    b_mon <-stack(b_mon, mon_sum) 
  }
  
  #Calculations on monthly time series--------------------------------------------
  b_a <- stack(b_all1,b_mon)
  b_a2 <- b_a[[(mon+1):nlayers(b_a)]]
  #Setup index for processing
  stop_mon <- nlayers(b_a2)
  start_mon <- stop_mon - 11
  ytd <- b_a2[[start_mon:stop_mon]]
  b_ytd <- sum(ytd)
  
  #Chunk monthly stack and sum annually -----------------------------------
  print("Doing stack calcs: sum")
  
  start_time <- Sys.time()
  #Chunks
  chunks <- list(1:(12*20), (12*20+1):(40*12), (40*12+1):(12*60), (12*60+1):nlayers(b_a2))
  b_sum <- stack()
  #Iterate over chunks
  for (i in 1:length(chunks)) {
    list_i <- chunks[[i]]
    b_i <- b_a2[[list_i]]
    indices_i <- rep(1:(length(list_i)/12), each = 12)
    b_sumi <- stackApply(b_i, indices_i, fun = sum, na.rm=TRUE)
    b_sum <- stack(b_sum, b_sumi)
  }
  
  writeRaster(b_sum, paste(nc_out,'/', month.abb[mon],'_', month.abb[mon+1], '_annualrain_', yr,'.nc', sep=""), "CDF", overwrite=TRUE,
              varname="rain",varunit="NA",longname="Annual rain (mm)",
              xname="lon",yname="lat", zname="years", zunit="",NAflag=-9999)
  
  print("Doing stack calcs: percentiles")
  #find 5th percentile
  ann_05 <- calc(b_sum, function(x) {quantile(x,0.05)}) 
  #find 10th percentile
  ann_10 <- calc(b_sum, function(x) {quantile(x,0.10)})
  
  print("Doing stack calcs: raster reclass and mask")
  #Reclass cells for 5th percentile----------------- 
  print('Reclass 5th percentile data')
  r05_o <- overlay(b_ytd, ann_05, fun=function(x,y) { x[x >= y] <- NA; x })
  r05_u <- overlay(b_ytd, ann_05, fun=function(x,y) { x[x <= y] <- NA; x })
  r05_om <- mask(b_ytd, r05_o, updatevalue = 0)
  r05_um <- mask(r05_om, r05_u, updatevalue = 2)
  
  #Crop and mask to oz coastline
  print('Get coastal outline as shape')
  oz <- getData('GADM', country='AUS', level=1)
  #Read in shape file
  dsn <- getwd() 
  # oz <- readOGR(dsn = dsn  ,layer = "Aus_coast" ) 
  print('Crop and mask 5th percentile data')
  r05_crop <- crop(r05_um, oz)
  r05 <- mask(r05_crop, oz)
  
  #Reclass cells for 10th percentile----------------- 
  print('Reclass 10th percentile data')
  r10_o <- overlay(b_ytd, ann_10, fun=function(x,y) { x[x >= y] <- NA; x })
  r10_u <- overlay(b_ytd, ann_10, fun=function(x,y) { x[x <= y] <- NA; x })
  r10_om <- mask(b_ytd, r10_o, updatevalue = 0)
  r10_um <- mask(r10_om, r10_u, updatevalue = 1)
  
  #Crop and mask to oz coastline
  print('Crop and mask 10th percentile data')
  r10_crop <- crop(r10_um, oz)
  r10 <- mask(r10_crop, oz)
  r10_05 <- mask(r10, r05, maskvalue =2, updatevalue = 2)
 
  #Write ncdfs
  print('Write rasters and put')
  # output$raindeficit_05perc <- r05
  # output$raindeficit_10perc <- r05
  # output$raindeficit_10_05perc <- r10_05
  
  r_all <- stack(r10_05,r10,r05)
  plot(r_all)
  
  writeRaster(r_all, paste(nc_out,'/rain_deficit_alllayers_',  mon2, '_', yr, '.nc', sep=""), "CDF", overwrite=TRUE,
              varname="Rain_deficit",varunit="NA",longname="1 is 10th; 2 is 5th percentile deficit over 12 months",
              xname="lon",yname="lat", zname="deficit_type", zunit="",NAflag=-9999)
 
#=========================================
# Compute annual grids ----------------------------------------------------

##Setup dirs --------------------------------------------
# WD<- setwd(as.character("C:/Data/CSIRO Postdoc/Ag_climate/R_code/NT_analysis"))
WD <- setwd(as.character('//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/climate/GriddedRain'))
nc_dir_mon <- as.character('//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/climate/GriddedRain')

#--------------------------------------------------------
raster_file_list <- list.files( path = WD,
                                pattern = ".nc" , all.files = FALSE , full.names = FALSE )
#
#Start loop on annual monthly nc files-------------------------
b_all <- stack()
b_ann <- stack()

#Loop start year
for (i in (i_start : length(raster_file_list))) {
  
  print(paste("Processing rasters for ", raster_file_list[i], sep = ""))
  
  ##Open netcdf as raster brick -------------------------------
  b <- brick(paste(WD,"/monthly_nc/",raster_file_list[i], sep = ""),varname = "rain")
  
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

yrs <- seq(1950,2016, by =1)
oz <- getData('GADM', country='AUS', level=1)
ball_c <- crop(b_all, oz)
ball_m <- mask(ball_c, oz)

# writeRaster(ball_m, paste(nc_dir,"/1950_2016_monthly_rain2.nc", sep=""), "CDF", overwrite=TRUE,
#             varname="rain",varunit="mm",longname="Monthly rain data",
#             xname="lon",yname="lat", zname="year.mon", zunit="month",bylayer=FALSE)

#Calculations on monthly time series--------------------------------------------
# nc <- nc_open("1950_2016_mean_monthly2.nc")  
#get the date from the names of the layers and extract the month
indices <- rep(seq(1,12, by=1), length(yrs))

#sum the layers
bmon <- stackApply(ball_m, indices, fun = mean)

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

  
#===========================
# Compute frost days ------------------------------------------------------
  rasterOptions(format="CDF",overwrite=TRUE,maxmemory = 1e+09, chunksize=1e+08,progress="text",tmpdir="C:/")
  rasterTmpFile("clean_this_after_")
  
  WD <- setwd(as.character('//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/min_temp'))
  nc_dir <- as.character('//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/min_temp')
  
  #--------------------------------------------------------
  raster_file_list <- list.files( path = nc_dir,
                                  pattern = ".nc" , all.files = FALSE , full.names = FALSE )
  
  #Start loop on annual nc files-------------------------
  frall <- brick()
  
  for (i in 1 :length(raster_file_list)) {
    
    print(paste("Processing rasters for ", raster_file_list[i], sep = ""))
    
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

