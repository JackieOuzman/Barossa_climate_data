
#####################################################################################################################
######    Cal the growing dregee days ##############################################################################
#####################################################################################################################
#function one
function_daily_mean_temp <- function(min, max) {
  daily_mean_temp <- (min +max)/2
  return(daily_mean_temp)
}
#function two
f <- function(daily_mean_temp) {
  ifelse( daily_mean_temp>10,daily_mean_temp, NA)
}

#function three
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





#################################################################################################################################################
###############     Get the working correct on subset of data ##################################################################################
#################################################################################################################################################


##### Perhaps try without the functions #####

#function one
#bring in the min grid for year
min <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/min_temp/",
          1990, ".min_temp.nc", sep = ""),varname = "min_temp")
#bring in the max grid for year
max <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/max_temp/",
          1990 , ".max_temp.nc", sep = ""),varname = "max_temp")
#cal the mean daily temp for each grid cell per day 
daily_mean_temp <- overlay(min, max, fun = function_daily_mean_temp)
#retain only values greater than 10 using the below function
f <- function(daily_mean_temp) {
  ifelse( daily_mean_temp>10,daily_mean_temp, NA)
}
daily_mean_temp_above10 <- overlay(daily_mean_temp, fun = f)
daily_mean_temp_above10
#extrcat only values between 1st Oct and 30April
Oct_dec_leap_yrs <-   subset(daily_mean_temp_above10, 304:365) #62
jan_april_leap_yrs <- subset(daily_mean_temp_above10, 1:119) #119
GS_leap_yrs <- stack(Oct_dec_leap_yrs, jan_april_leap_yrs) #this should be 181 n layers (62 + 119)

#sum all the layers in the raster stack
sum_GDD_leap_yrs <- stackApply(GS_leap_yrs, indices = 1, fun=sum)


Oct_dec_non_leap_yrs <- subset(daily_mean_temp_above10, 303:364) #62
jan_april_non_leap_yrs <- subset(daily_mean_temp_above10, 1:120) #120
GS_leap_non_yrs <- stack(Oct_dec_non_leap_yrs, jan_april_non_leap_yrs) #this should be 182 n layers (62 + 120)
sum_GDD_non_leap_yrs <- stackApply(GS_leap_non_yrs, indices = 1, fun=sum)

leap_years <- c( "1992", "1996","2000", "2004" ,"2008", "2012", "2016")


non_leap_years <- c("1989", "1990" ,"1991",  "1993", "1994" ,"1995", "1997",
                    "1998" ,"1999" ,"2001", "2002", "2003", "2005", "2006",
                    "2007" , "2009", "2010", "2011" , "2013", "2014" ,"2015" ,
                    "2017" ,"2018")


GDD_all_yrs <- stack(jan_temp1989, jan_temp1990, jan_temp1991, jan_temp1992, jan_temp1993, jan_temp1994,
                jan_temp1995, jan_temp1996, jan_temp1997, jan_temp1998, jan_temp1999, jan_temp2000,
                jan_temp2001, jan_temp2002, jan_temp2003, jan_temp2004, jan_temp2005, jan_temp2006,
                jan_temp2007, jan_temp2008, jan_temp2009, jan_temp2010, jan_temp2011, jan_temp2012,
                jan_temp2013, jan_temp2014, jan_temp2015, jan_temp2016, jan_temp2017, jan_temp2018)
mean_GDD_all_yrs <- calc(GDD_all_yrs, fun = mean, na.rm = T)


#################################################################################################################################################



  min <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/min_temp/",
          1990, ".min_temp.nc", sep = ""),varname = "min_temp")
  #bring in the max grid for year
  max <- brick(
    paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/max_temp/",
          1990 , ".max_temp.nc", sep = ""),varname = "max_temp")
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


