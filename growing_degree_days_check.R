####################################################################################################################
################# Import the library - some I wont use in this anlysis ###########################################
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
library(ggplot2)


#####################################################################################################################
######    Cal the growing dregee days ##############################################################################
#####################################################################################################################
setwd("I:/work/silo") #the folder now has curley bracket which is means something in R so the is a work around
getwd()


#function one just get the daily average
function_daily_mean_temp <- function(min, max) {
  daily_mean_temp <- (min +max)/2
  return(daily_mean_temp)
}
#function two only select temp greater than 10 dregees
f <- function(daily_mean_temp) {
  ifelse( daily_mean_temp>10,daily_mean_temp, NA)
}

barrossa_st <- st_read("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/Vine_health_data/CSIRO/GI/baroosa_ext_WGS_buff3.shp")
barrossa_sf <- as(barrossa_st, "Spatial") #convert to a sp object

#function three put it all togther
function_GG_leap_yrs <- function(yr) {
    min_1 <- brick(
      paste("min_temp/",
        yr, ".min_temp.nc", sep = ""),varname = "min_temp")
#bring in the max grid for year
    max_1 <- brick(
      paste("max_temp/",
        yr , ".max_temp.nc", sep = ""),varname = "max_temp")
    
    min <- crop(min_1, barrossa_sf)
    max <- crop(max_1, barrossa_sf)
#cal the mean daily temp for each grid cell per day 
    daily_mean_temp <- overlay(min, max, fun = function_daily_mean_temp)
#retain only values greater than 10 using the below function
    daily_mean_temp_above10_a <- overlay(daily_mean_temp, fun = f)
    daily_mean_temp_above10 <- (daily_mean_temp_above10_a - 10)

#extrcat only values between 1st Oct and 30April
# Oct_dec_leap_yrs <-   subset(daily_mean_temp_above10, 275:366) #62
# jan_april_leap_yrs <- subset(daily_mean_temp_above10, 1:121) #121

Sep_dec_leap_yrs <-   subset(daily_mean_temp_above10, 245:366) 
jan_march_leap_yrs <- subset(daily_mean_temp_above10, 1:61) 
GS_leap_yrs <- stack(Sep_dec_leap_yrs, jan_march_leap_yrs) #this should be xx n layers (62 + 120)

#sum all the layers in the raster stack
sum_GDD_leap_yrs <- stackApply(GS_leap_yrs, indices = 1, fun=sum)
sum_GDD_leap_yrs
}


#leap_years <- c( "1992")


leap_years <- c( "1992", "1996","2000", "2004" ,"2008", "2012", "2016")
non_leap_years <- c("1989", "1990" ,"1991",  "1993", "1994" ,"1995", "1997",
                    "1998" ,"1999" ,"2001", "2002", "2003", "2005", "2006",
                    "2007" , "2009", "2010", "2011" , "2013", "2014" ,"2015" ,
                    "2017" ,"2018")



#make loop raster of GGD for GS leap year
for (i in leap_years) {
  assign(paste0("GGD_leap_yrs_", i), function_GG_leap_yrs(i))
}




#function four
function_GG_non_leap_yrs <- function(yr) {
  min_1 <- brick(
    paste("min_temp/",
          yr, ".min_temp.nc", sep = ""),varname = "min_temp")
  #bring in the max grid for year
  max_1 <- brick(
    paste("max_temp/",
          yr , ".max_temp.nc", sep = ""),varname = "max_temp")
  
  min <- crop(min_1, barrossa_sf)
  max <- crop(max_1, barrossa_sf)
  
  #cal the mean daily temp for each grid cell per day 
  daily_mean_temp <- overlay(min, max, fun = function_daily_mean_temp)
  #retain only values greater than 10 using the below function
  daily_mean_temp_above10_a <- overlay(daily_mean_temp, fun = f)
  daily_mean_temp_above10 <- (daily_mean_temp_above10_a - 10)
  
  #extrcat only values between 1st Oct and 30April
  # Oct_dec_non_leap_yrs <-   subset(daily_mean_temp_above10, 274:365) #91
  # jan_april_non_leap_yrs <- subset(daily_mean_temp_above10, 1:120) #120
  
  Sep_dec_non_leap_yrs <-   subset(daily_mean_temp_above10, 244:365) 
  jan_march_non_leap_yrs <- subset(daily_mean_temp_above10, 1:60) 
  GS_non_leap_yrs <- stack(Sep_dec_non_leap_yrs, jan_march_non_leap_yrs) #this should be xx n layers (62 + 120)
  
  
  
  #sum all the layers in the raster stack
  sum_GDD_non_leap_yrs <- stackApply(GS_non_leap_yrs, indices = 1, fun=sum)
  sum_GDD_non_leap_yrs
}


#make loop raster of GGD for GS non leap year
for (i in non_leap_years) {
  assign(paste0("GGD_non_leap_yrs_", i), function_GG_non_leap_yrs(i))
}


GDD_all_yrs <- stack(GGD_non_leap_yrs_1989, 
                      GGD_non_leap_yrs_1990, 
                      GGD_non_leap_yrs_1991,
                      GGD_leap_yrs_1992,
                      GGD_non_leap_yrs_1993,
                      GGD_non_leap_yrs_1994, 
                      GGD_non_leap_yrs_1995,
                      GGD_leap_yrs_1996,
                      GGD_non_leap_yrs_1997, 
                      GGD_non_leap_yrs_1998,
                      GGD_non_leap_yrs_1999,
                      GGD_leap_yrs_2000,
                      GGD_non_leap_yrs_2001,
                      GGD_non_leap_yrs_2002, 
                      GGD_non_leap_yrs_2003,
                      GGD_leap_yrs_2004,
                      GGD_non_leap_yrs_2005, 
                      GGD_non_leap_yrs_2006,
                      GGD_non_leap_yrs_2007,
                      GGD_leap_yrs_2008,
                      GGD_non_leap_yrs_2009,
                      GGD_non_leap_yrs_2010, 
                      GGD_non_leap_yrs_2011,
                      GGD_leap_yrs_2012,
                      GGD_non_leap_yrs_2013, 
                      GGD_non_leap_yrs_2014, 
                      GGD_non_leap_yrs_2015,
                      GGD_leap_yrs_2016, 
                      GGD_non_leap_yrs_2017,
                      GGD_non_leap_yrs_2018)


GGD_non_leap_yrs_1989
plot(GGD_non_leap_yrs_1989)

mean_GDD_all_yrs <- calc(GDD_all_yrs, fun = mean, na.rm = T)
mean_GDD_all_yrs
plot(mean_GDD_all_yrs)

# #Write
writeRaster(mean_GDD_all_yrs, "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/mean_GDD_all_yrs_1989_2018",format = "GTiff", overwrite = TRUE) 


######################################################################################################################
######################## End od script ##############################################################################




#########################################################################################################################
####                           create a plot of how  has changed over time
#########################################################################################################################



##### bring in and use a shapefile which conatins the points I want to extract

#this is a barossa modified grid as a series of points (shapefile)
barrossa_st_extract <- st_read("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/extract_jan_temp_yrs_WGS.shp")
barrossa_extract_sf <- as(barrossa_st_extract, "Spatial") #convert to a sp object
class(barrossa_extract_sf)
plot(barrossa_extract_sf)
library(raster)
library(rasterVis)
#plt <- levelplot(layer.1, margin=F, 
#                 main="Mean Jan temp for first year")
#plt + layer(sp.points(barrossa_extract_sf, col="black", pch=16, cex=0.5))

#crs(barrossa_extract_sf)
#crs(layer.1)
GDD_all_yrs_extract <- extract(GDD_all_yrs, barrossa_extract_sf, method="simple")
class(GDD_all_yrs_extract)
head(GDD_all_yrs_extract)

GDD_all_yrs_wide <- data.frame(barrossa_extract_sf$POINT_X, barrossa_extract_sf$POINT_Y, GDD_all_yrs_extract)
head(GDD_all_yrs_wide)
#I am not confiedent in the order think it might be leap year first and then non leap years
names(GDD_all_yrs_wide) <- c("POINT_X", "POINT_Y", "1989", "1990", "1991", "1992", "1993", "1994",
                              "1995", "1996", "1997", "1998", "1999", "2000",
                              "2001", "2002", "2003", "2004", "2005", "2006",
                              "2007", "2008", "2009", "2010", "2011", "2012",
                              "2013", "2014", "2015", "2016", "2017", "2018")



head(GDD_all_yrs_wide)
##### make the data narrow
library(dplyr)
library(tidyverse)
GDD_all_yrs_narrow <- gather(GDD_all_yrs_wide, key = "year", value = "GDD_all", `1989`:`2018` )
head(GDD_all_yrs_narrow)

######export as  csv this is a slow step
write.csv(GDD_all_yrs_narrow,
          "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/GDD_all_yrs_narrow_extract_pts.csv") 

#library(ggplot2)
ggplot(GDD_all_yrs_narrow, aes(GDD_all))+
  geom_density()+
  facet_wrap(.~year)




head(GDD_all_yrs_narrow)
#change year to double to get geom_smooth to work
GDD_all_yrs_narrow <- mutate(GDD_all_yrs_narrow, year_as_double = as.double(year))

ggplot(GDD_all_yrs_narrow, aes(factor(year_as_double), GDD_all))+
  geom_boxplot()+
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+ #straight line regression
  #geom_smooth(color="black", aes(group=1))+ #smooth line
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        plot.caption = element_text(hjust = 0))+
  labs(x = "Year",
       y = "Growing degreee days",
       title = "Sample points over the Barossa",
       subtitle = "GS defined as 1st Sep to 31st March",
       caption = "First the GGD is calculated for each pixel by year, then the values for each pixel is extracted point by point. This is achieved by using the Barossa modified boundary and converting it into a shapefile
       ")




#################################################################################################################################
#### Rob wants a moving average to be plotted on the graph.

#step 1 create a new dataframe that is the average
head(GDD_all_yrs_narrow)
GDD_all_yrs_narrow_mean <- GDD_all_yrs_narrow %>% 
  group_by(year_as_double) %>% 
  summarise(GDD_all = mean(GDD_all))
head(GDD_all_yrs_narrow_mean)

# step 2 add another clm to data that has a rolling average
#this can be created using zoo package
library(zoo)
GDD_all_yrs_narrow_mean$roll5 = zoo::rollmean(GDD_all_yrs_narrow_mean$GDD_all, 5, na.pad=TRUE)


# step 3 plot the 2 data sets onto the same graph
head(GDD_all_yrs_narrow_mean)
head(GDD_all_yrs_narrow)

# ggplot(GDD_all_yrs_narrow, aes(factor(year_as_double), GDD_all))+
#   geom_boxplot()+
#   geom_smooth(data= GDD_all_yrs_narrow_mean, aes(x= factor(year_as_double), y= roll5, group=1), color='blue', se=FALSE)+ #needs the group 1
#   theme_classic()+
#   theme(axis.text.x = element_text(angle = 90, hjust=1),
#         plot.caption = element_text(hjust = 0))+
#   labs(x = "Year",
#        y = "Growing degreee days",
#        title = "Sample points over the Barossa",
#        subtitle = "GS defined as 1st Sep to 31st March",
#        caption = "First the GGD is calculated for each pixel by year, then the values for each pixel is extracted point by point. This is achieved by using the Barossa modified boundary and converting it into a shapefile
#        ")
plot5 <- ggplot(GDD_all_yrs_narrow, aes(factor(year_as_double), GDD_all))+
  geom_boxplot()+
  geom_smooth(data= GDD_all_yrs_narrow_mean, aes(x= factor(year_as_double), y= roll5, group=1), color='blue', se=FALSE)+ #needs the group 1
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        plot.caption = element_text(hjust = 0))+
  labs(x = "Year",
       y = "Growing Degreee Days")+
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12)
  )

plot5
ggsave(filename = "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/2021_analysis/plots/Growing_degree_days_rolling_av.png", device = "png" ,dpi=600)




######################################################################################################

### mess about 


head(GDD_all_yrs_narrow)
test <- filter(GDD_all_yrs_narrow, year_as_double < "2000" )
head(test)
#install.packages("zoo")
library(zoo)

library(dplyr)
# for each year what is the GDD_all mean value ?
test_mean <- test %>% 
  group_by(year_as_double) %>% 
  summarise(mean_of_GDD_all_by_yr = mean(GDD_all))
test_mean
test_mean$roll5 = zoo::rollmean(test_mean$mean_of_GDD_all_by_yr, 5, na.pad=TRUE)
test_mean


test

ggplot(test, aes(factor(year_as_double), GDD_all))+
  geom_boxplot()

ggplot(test_mean, aes(factor(year_as_double), mean_of_GDD_all_by_yr))+
  geom_point()+
  geom_point(aes(factor(year_as_double), roll5), color = "green")+
  #geom_smooth(color="black", se=FALSE, aes(group=1))+
  geom_smooth(aes(factor(year_as_double), roll5), color = "green", se=FALSE)

ggplot() + 
  #geom_line(data=test_mean, aes(x = year_as_double, y= mean_of_GDD_all_by_yr), color='green') + 
  #geom_line(data=test_mean, aes(x = year_as_double, y= roll5), color='red')
  geom_smooth(data=test_mean, aes(x = year_as_double, y= mean_of_GDD_all_by_yr), color='green') + 
  geom_smooth(data=test_mean, aes(x = year_as_double, y= roll5), color='red')

test_mean

### I can add the line and the points onto the same graph but this is from the same df  
ggplot(test_mean, aes(year_as_double, mean_of_GDD_all_by_yr))+
  #geom_boxplot()+ #this doesnt work but might be the data set??
  geom_point()+
  #geom_line(aes(y=roll5)) +
  geom_smooth(aes(y= roll5), color='red', se=FALSE)+
  theme_bw()

### what about two different dfs - cool this works!!
head(test)
head(test_mean)
ggplot(test, aes(year_as_double, GDD_all))+
  #geom_boxplot()+ #this doesnt work but might be the data set??
  geom_point()+
  #geom_line(data = test_mean, aes(y=roll5)) +
  geom_smooth(data= test_mean, aes(y= roll5), color='red', se=FALSE)+
  theme_bw()



### what about two different dfs and year_as_double coded to factor?? _ not working...

ggplot(test, aes(factor(year_as_double), GDD_all))+
  geom_boxplot()+ #this doesnt work but might be the data set??
  geom_smooth(data= test_mean, aes(x= factor(year_as_double), y= roll5, group=1), color='blue', se=FALSE)+ #needs the group 1
  theme_bw()