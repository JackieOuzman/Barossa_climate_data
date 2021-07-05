
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
library(sf)

# set up input files source ------------------------------------------------------------------
raster_file_list <- list.files( path = '//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/input_data',
                                pattern = ".nc" , all.files = FALSE , full.names = FALSE )
raster_file_list



# inputs data range------------------------------------------------------------------
#starting year of climate record

start_year <- '1989' 

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

barrossa_st <- st_read("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/Vine_health_data/CSIRO/GI/baroosa_ext_WGS_buff3.shp")
barrossa_sf <- as(barrossa_st, "Spatial") #convert to a sp object


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
  #cut out just study area
  b_1 <- crop(b, barrossa_sf)
  #Add brick to long-term stack
  b_all <-stack(b_all, b_1) 
  
  
}
b_all

############### Step 2 ###################################################################

yrs <- seq(1989,2018, by =1)






#### step 3 Calculations on monthly time series--------------------------------------------

#get the date from the names of the layers and extract the month
indices <- rep(seq(1,12, by=1), length(yrs))
indices
#sum the layers
#bmon <- stackApply(ball_m, indices, fun = mean)

bmon <- stackApply(b_all, indices, fun = mean) #average rainfall for each month
bmon
#Mean annual and monthly rain layers---------------------

bann <- sum(bmon) #sum of average rainfall for each month
bann

rain_jan_april <- sum(bmon[[1:4]]) #sum of average rainfall 
rain_jan_march <- sum(bmon[[1:3]]) #sum of average rainfall - Rob new request 2021
rain_oct_dec <- sum(bmon[[10:12]]) #sum of average rainfall 
rain_sep_dec <- sum(bmon[[9:12]]) #s  ###alterntive GS period - Rob new request 2021

#rain_gs <-sum(rain_jan_april, rain_oct_dec)
rain_gs <-sum(rain_jan_march, rain_sep_dec)
rain_gs_sep <-sum(rain_jan_april, rain_sep_dec) #alt GS period
plot(rain_gs)



# #Write
writeRaster(bann, 
            "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/bann.tif",
            format = "GTiff", overwrite = TRUE) #sum of average rainfall for each month
writeRaster(rain_gs, 
            "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/rain_gs",
            format = "GTiff", overwrite = TRUE) #sum of average GS rainfall for each month 30yrs

# End code ----------------------------------------------------



########################################################################################################################
####                           create a plot of how  has changed over time
#########################################################################################################################


b_all


month <- seq(1,12, by =1)
month

#indices_yr <- rep(seq(1,12, by=12), length(month))
indices_yr <- c(rep(1, 12), rep(2, 12), rep(3, 12), rep(4, 12), rep(5, 12), rep(6, 12), rep(7, 12), rep(8, 12), rep(9, 12), rep(10, 12),
                rep(11, 12), rep(12, 12), rep(13, 12), rep(14, 12), rep(15, 12), rep(16, 12), rep(17, 12), rep(18, 12), rep(19, 12), rep(20, 12),
                rep(21, 12), rep(22, 12), rep(23, 12), rep(24, 12), rep(25, 12), rep(26, 12), rep(27, 12), rep(28, 12), rep(29, 12), rep(30, 12)
                )
indices_yr

test2 <- stackApply(b_all, indices_yr, fun = sum) 
#test2 <- calc(b_all, indices_yr, fun = sum)
#test2 <- calc(b_all,  fun = sum)

test2
plot(test2$index_1)
##### bring in and use a shapefile which conatins the points I want to extract

#this is a barossa modified grid as a series of points (shapefile)
barrossa_st_extract <- st_read("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/extract_jan_temp_yrs_WGS.shp")
barrossa_extract_sf <- as(barrossa_st_extract, "Spatial") #convert to a sp object
class(barrossa_extract_sf)
#plot(barrossa_extract_sf)
library(raster)
library(rasterVis)


#plt <- levelplot(index_1, margin=F, 
#                 main="annual rainfall")
#plt + layer(sp.points(barrossa_extract_sf, col="black", pch=16, cex=0.5))


crs(barrossa_extract_sf)
crs(test2)

#rainfall_all_yrs_extract <- extract(test2, barrossa_extract_sf, method="simple")
#rainfall_all_yrs_extract <- raster::extract(test2, barrossa_extract_sf, df = TRUE)
rainfall_all_yrs_extract <- raster::extract(test2, barrossa_extract_sf)
rainfall_all_yrs_extract

class(rainfall_all_yrs_extract)
head(rainfall_all_yrs_extract)

rainfall_all_yrs_wide <- data.frame(barrossa_extract_sf$POINT_X, barrossa_extract_sf$POINT_Y, rainfall_all_yrs_extract)
head(rainfall_all_yrs_wide)
#I am not confiedent in the order think it might be leap year first and then non leap years
names(rainfall_all_yrs_wide) <- c("POINT_X", "POINT_Y", "1989", "1990", "1991", "1992", "1993", "1994",
                             "1995", "1996", "1997", "1998", "1999", "2000",
                             "2001", "2002", "2003", "2004", "2005", "2006",
                             "2007", "2008", "2009", "2010", "2011", "2012",
                             "2013", "2014", "2015", "2016", "2017", "2018")



head(rainfall_all_yrs_wide)
##### make the data narrow
library(dplyr)
library(tidyverse)
rainfall_all_yrs_narrow <- gather(rainfall_all_yrs_wide, key = "year", value = "mean_rainfall", `1989`:`2018` )
head(rainfall_all_yrs_narrow)

######export as  csv this is a slow step
write.csv(rainfall_all_yrs_narrow,
          "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/mean_rainfall_all_yrs_narrow_pts.csv") 

### make changes to graphs - rainfall
rainfall_all_yrs_narrow <- read.csv("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/mean_rainfall_all_yrs_narrow_pts.csv") 





#change year to double to get geom_smooth to work
rainfall_all_yrs_narrow <- mutate(rainfall_all_yrs_narrow, year_as_double = as.double(year))

ggplot(rainfall_all_yrs_narrow, aes(factor(year_as_double), mean_rainfall))+
  geom_boxplot()+
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+ #straight line regression
  #geom_smooth(color="black", aes(group=1))+ #smooth line
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        plot.caption = element_text(hjust = 0))+
  labs(x = "Year",
       y = "Mean rainfall",
       title = "Sample points over the Barossa")
       #subtitle = "GS defined as 1st Sep to 31st March")#,
       #caption = "Values for each pixel is extracted point by point. This is achieved by using the Barossa modified boundary and converting it into a shapefile
       #")

###moving ave as line
head(rainfall_all_yrs_narrow)

rainfall_all_yrs_mean <- rainfall_all_yrs_narrow %>% 
  group_by(year_as_double) %>% 
  summarise(mean_rainfall = mean(mean_rainfall))
head(rainfall_all_yrs_mean)

# step 2 add another clm to data that has a rolling average
#this can be created using zoo package
library(zoo)
rainfall_all_yrs_mean$roll5 = zoo::rollmean(rainfall_all_yrs_mean$mean_rainfall, 5, na.pad=TRUE)


# step 3 plot the 2 data sets onto the same graph
head(rainfall_all_yrs_mean)
head(rainfall_all_yrs_narrow)

plot2 <- ggplot(rainfall_all_yrs_narrow, aes(factor(year_as_double), mean_rainfall))+
  geom_boxplot()+
  geom_smooth(data= rainfall_all_yrs_mean, aes(x= factor(year_as_double), y= roll5, group=1), color='blue', se=FALSE)+ #needs the group 1
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        plot.caption = element_text(hjust = 0))+
  labs(x = "",
       y = "Annual rainfall (mm)")+
  #annotate("text", x = factor(1990), y = 1250, label = "(c)", size = 8)+
  
  #scale_y_continuous(labels = scales::number_format())+
  theme(
    #axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 30, face = "bold"),
    
    axis.text.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=25),
    
    
    axis.ticks=element_line(color="black", size=0.5),
    axis.ticks.length=unit(-0.25, "cm"),
    
    
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
       #title = "Sample points over the Barossa")
       #subtitle = "GS defined as 1st Sep to 31st March")#,
#caption = "Values for each pixel is extracted point by point. This is achieved by using the Barossa modified boundary and converting it into a shapefile
#")
plot2
#ggsave(filename = "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/2021_analysis/plots/mean_rainfall_rolling_av.png", device = "png" ,dpi=400)

ggsave(filename = "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/2021_analysis/plots/mean_rainfall_rolling_av.png", 
       device = "png" ,
       dpi=600,
       width = 16,
       height = 9)


getwd()


################################## sum of GS rainfall ########


function_GS_rain <- function(yr) {
#bring in the rain grid for year
  rain_1 <- brick(
    paste("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/input_data/",
          yr, ".monthly_rain.nc", sep = ""),varname = "monthly_rain")
  mons <- c("01", "02", "03","04", "05","06", "07", "08","09", "10", "11", "12")
  
  rain <- crop(rain_1, barrossa_sf)
 
  #extrcat only values between 1st Oct and 30April
  # Oct_dec_ <-   subset(rain, 10:12) #old GS def
  # jan_april <- subset(rain, 1:4)#old GS def
  
  sep_dec_ <-   subset(rain, 9:12) #new GS def 2021
  jan_march <- subset(rain, 1:3) #new GS def 2021
  GS_rain <- stack(sep_dec_, jan_march) 
  
  #sum all the layers in the raster stack
  sum_GS_rain <- stackApply(GS_rain, indices = 1, fun=sum)
  sum_GS_rain
}
jax_list <- as.character(c(1989:2018)) #30 years of data as string
jax_list


for (i in jax_list) {
  assign(paste0("sum_rain_yr", i), function_GS_rain(i))
}
sum_rain_yr1989
sum_rain_all_yrs <- stack(sum_rain_yr1989, sum_rain_yr1990, sum_rain_yr1991, sum_rain_yr1992, sum_rain_yr1993, sum_rain_yr1994,
                  sum_rain_yr1995, sum_rain_yr1996, sum_rain_yr1997, sum_rain_yr1998, sum_rain_yr1999, sum_rain_yr2000,
                  sum_rain_yr2001, sum_rain_yr2002, sum_rain_yr2003, sum_rain_yr2004, sum_rain_yr2005, sum_rain_yr2006,
                  sum_rain_yr2007, sum_rain_yr2008, sum_rain_yr2009, sum_rain_yr2010, sum_rain_yr2011, sum_rain_yr2012,
                  sum_rain_yr2013, sum_rain_yr2014, sum_rain_yr2015, sum_rain_yr2016, sum_rain_yr2017, sum_rain_yr2018)


sum_rain_all_yrs



sum_rain_all_yrs_extract <- raster::extract(sum_rain_all_yrs, barrossa_extract_sf, method="simple")
class(sum_rain_all_yrs_extract)
head(sum_rain_all_yrs_extract)

pts_GS_rain_temp_wide <- data.frame(barrossa_extract_sf$POINT_X, barrossa_extract_sf$POINT_Y, sum_rain_all_yrs_extract)
head(pts_GS_rain_temp_wide)

names(pts_GS_rain_temp_wide) <- c("POINT_X", "POINT_Y", "1989", "1990", "1991", "1992", "1993", "1994",
                              "1995", "1996", "1997", "1998", "1999", "2000",
                              "2001", "2002", "2003", "2004", "2005", "2006",
                              "2007", "2008", "2009", "2010", "2011", "2012",
                              "2013", "2014", "2015", "2016", "2017", "2018")

head(pts_GS_rain_temp_wide)
##### make the data narrow
library(dplyr)
library(tidyverse)
pts_GS_rain_temp_narrow <- gather(pts_GS_rain_temp_wide, key = "year", value = "GS_rain", `1989`:`2018` )
head(pts_GS_rain_temp_narrow)

#change year to double to get geom_smooth to work
pts_GS_rain_temp_narrow <- mutate(pts_GS_rain_temp_narrow, year_as_double = as.double(year))

ggplot(pts_GS_rain_temp_narrow, aes(factor(year_as_double), GS_rain))+
  geom_boxplot()+
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+ #straight line regression
  #geom_smooth(color="black", aes(group=1))+ #smooth line
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        plot.caption = element_text(hjust = 0))+
  labs(x = "Year",
       y = "Mean GS rainfall",
       title = "Sample points over the Barossa",
       subtitle = "GS defined as 1st Sep to 31st March")#,
       #caption = "Values for each pixel is extracted point by point. This is achieved by using the Barossa modified boundary and converting it into a shapefile
       #")

#### add moving av line


head(pts_GS_rain_temp_narrow)

GS_rain_mean <- pts_GS_rain_temp_narrow %>% 
  group_by(year_as_double) %>% 
  summarise(GS_rain = mean(GS_rain))
head(GS_rain_mean)

# step 2 add another clm to data that has a rolling average
#this can be created using zoo package
library(zoo)
GS_rain_mean$roll5 = zoo::rollmean(GS_rain_mean$GS_rain, 5, na.pad=TRUE)


######import csv to make graph changes
#pts_GS_rain_temp_narrow <- read.csv("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/pts_GS_rain_temp_narrow_pts.csv") 
#GS_rain_mean <- pts_GS_rain_temp_narrow

# step 3 plot the 2 data sets onto the same graph
head(GS_rain_mean)
head(pts_GS_rain_temp_narrow)

plot3 <- ggplot(pts_GS_rain_temp_narrow, aes(factor(year_as_double), GS_rain))+
  geom_boxplot()+
  geom_smooth(data= GS_rain_mean, aes(x= factor(year_as_double), y= roll5, group=1), color='blue', se=FALSE)+ #needs the group 1
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        plot.caption = element_text(hjust = 0))+
  labs(x = "Year",
       y = "Growing season rainfall (mm)")+
  #annotate("text", x = factor(1990), y = 650, label = "(d)", size = 8)+
  
  #scale_y_continuous(labels = scales::number_format())+
  scale_y_continuous(breaks = c(100,200,300,400,500,600,700))+
  theme(
    axis.title.x = element_text(size = 30, face = "bold"),
    axis.title.y = element_text(size = 30, face = "bold"),
    
    axis.text.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=25),
    axis.text.x=element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=25, vjust = 0.3),
    
    
    axis.ticks=element_line(color="black", size=0.5),
    axis.ticks.length=unit(-0.25, "cm")
    
  )
       # title = "Sample points over the Barossa",
       # subtitle = "GS defined as 1st Sep to 31st March")#,
#caption = "Values for each pixel is extracted point by point. This is achieved by using the Barossa modified boundary and converting it into a shapefile
#")
plot3
#ggsave(filename = "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/2021_analysis/plots/mean_GS_rainfall_rolling_av.png", device = "png" ,dpi=600)

ggsave(filename = "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/2021_analysis/plots/mean_GS_rainfall_rolling_av.png", 
       device = "png" ,
       dpi=600,
       width = 16,
       height = 10)

######export as  csv this is a slow step
write.csv(pts_GS_rain_temp_narrow,
          "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/pts_GS_rain_temp_narrow_pts.csv") 
