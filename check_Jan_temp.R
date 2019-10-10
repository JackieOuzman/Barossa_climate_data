

function_daily_mean_temp <- function(min, max) {
  daily_mean_temp <- (min +max)/2
  return(daily_mean_temp)
}

######################################################################################################################################################
###############   2018  #########################
  
  min_2018 <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/min_temp/",
          "2018", ".min_temp.nc", sep = ""),varname = "min_temp")
  max_2018 <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/max_temp/",
          "2018" , ".max_temp.nc", sep = ""),varname = "max_temp")
  
  ####Extra step to speed stuff up
  min_2018_c <- crop(min_2018, barrossa_sf)
  #min_2018_m <- mask(min_2018_c, barrossa_sf)
  #min_2018_m$X2018.01.01
  #plot(min_2018_m)
  
  max_2018_c <- crop(max_2018, barrossa_sf)
  max_2018_c
  #max_2018_m <- mask(max_2018_c, barrossa_sf)
  #max_2018_m$X2018.01.01
  #plot(max_2018_m)
  ###mean daily temprature
  daily_mean_temp2018 <- overlay(min_2018_c, max_2018_c, fun = function_daily_mean_temp)
  daily_mean_temp2018
  ####pull out the first 30 days of the year
  daily_mean_temp_jan2018 <- subset(daily_mean_temp2018, 1:30) #pull out the first 30 days of mean temp ? should this be 31??
  daily_mean_temp_jan2018
  plot(daily_mean_temp_jan2018)
  ####cal the average temp of the first 30 days
  av_jan_mean_temp2018 <- mean(daily_mean_temp_jan2018)
  plot(av_jan_mean_temp2018)
  av_jan_mean_temp2018
  
  writeRaster(av_jan_mean_temp2018, 
              "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/means_jan_temp_2018",
              format = "GTiff", overwrite = TRUE) #average jan temp for 1yr
  
  
  writeRaster(max_2018_m, 
              "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/max_2018_m",
              format = "GTiff", overwrite = TRUE) #max jan temp for 1yr
  
  writeRaster(min_2018_m, 
              "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/min_2018_m",
              format = "GTiff", overwrite = TRUE) #min jan temp for 1yr
  
  
  ######################################################################################################################################################
  ###############   2017  #########################
  
  min_2017 <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/min_temp/",
          "2017", ".min_temp.nc", sep = ""),varname = "min_temp")
  max_2017 <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/max_temp/",
          "2017" , ".max_temp.nc", sep = ""),varname = "max_temp")
  
  ####Extra step to speed stuff up
  min_2017_c <- crop(min_2017, barrossa_sf)
  #min_2017_m <- mask(min_2017_c, barrossa_sf)
  #min_2017_m$X2017.01.01
  #plot(min_2017_m)
  
  max_2017_c <- crop(max_2017, barrossa_sf)
  max_2017_c
  plot(max_2017_c)
  #max_2017_m <- mask(max_2017_c, barrossa_sf)
  #max_2017_m$X2017.01.01
  #plot(max_2017_m)
  ###mean daily temprature
  daily_mean_temp2017 <- overlay(min_2017_c, max_2017_c, fun = function_daily_mean_temp)
  daily_mean_temp2017
  ####pull out the first 30 days of the year
  daily_mean_temp_jan2017 <- subset(daily_mean_temp2017, 1:30) #pull out the first 30 days of mean temp ? should this be 31??
  daily_mean_temp_jan2017
  plot(daily_mean_temp_jan2017)
  ####cal the average temp of the first 30 days
  av_jan_mean_temp2017 <- mean(daily_mean_temp_jan2017)
  plot(av_jan_mean_temp2017)
  av_jan_mean_temp2017
  
  writeRaster(av_jan_mean_temp2017, 
              "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/means_jan_temp_2017",
              format = "GTiff", overwrite = TRUE) #average jan temp for 1yr
  
  writeRaster(max_2017_m, 
              "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/max_2017_m",
              format = "GTiff", overwrite = TRUE) #max jan temp for 1yr
  
  writeRaster(min_2017_m, 
              "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/min_2017_m",
              format = "GTiff", overwrite = TRUE) #min jan temp for 1yr
#######################################################################################################################################
  
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
  