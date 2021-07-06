## try put all the df togther
### fix up some graphs so just bring in the created data
library(zoo)
#devtools::install_github("zeehio/facetscales")
library(g)
library(facetscales)

library(lubridate)

library(dplyr)
library(tidyverse)


#########################################################################################################################
############ A growing season temp
GS_nonleap_leap_yrs_extract_narrow <- read.csv("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/GS_nonleap_leap_yrs_extract_narrow.csv")

GS_nonleap_leap_yrs_extract_narrow <- mutate(GS_nonleap_leap_yrs_extract_narrow, year_as_double = as.double(year))

GS_temp_mean <- GS_nonleap_leap_yrs_extract_narrow %>% 
  group_by(year_as_double) %>% 
  summarise(Mean_GS_temp = mean(Mean_GS_temp))


GS_temp_mean$roll5temp_mean = zoo::rollmean(GS_temp_mean$Mean_GS_temp, 5, na.pad=TRUE)



#for the large dataset
str(GS_nonleap_leap_yrs_extract_narrow)
min(GS_nonleap_leap_yrs_extract_narrow$Mean_GS_temp)
max(GS_nonleap_leap_yrs_extract_narrow$Mean_GS_temp)
mean(GS_nonleap_leap_yrs_extract_narrow$Mean_GS_temp)
## for the large dataset

#for the small dataset
str( GS_temp_mean)
min(GS_temp_mean$roll5temp_mean, na.rm = TRUE)
max(GS_temp_mean$roll5temp_mean, na.rm = TRUE)
mean(GS_temp_mean$roll5temp_mean, na.rm = TRUE)


plotA <- ggplot(GS_nonleap_leap_yrs_extract_narrow, aes(factor(year_as_double), Mean_GS_temp))+
  geom_boxplot()+
  geom_smooth(data= GS_temp_mean, aes(x= factor(year_as_double), y= roll5temp_mean, group=1), color='blue', se=FALSE)+ #needs the group 1
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        plot.caption = element_text(hjust = 0))+
  labs(x = "",
       y = "Mean growing season temperature (°C)")+
  #annotate("text", x = factor(1990), y = 20, label = "(a)", size = 8)+
  
  scale_y_continuous(labels = scales::number_format(), breaks = c(15,16,17,18,19,20))+
  theme(
    #axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 30, face = "bold"),
    
    #axis.text.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=25),
    axis.text.y=element_text(margin = margin(t = 0, r = 11, b = 0, l = 0), size=25),
    
    axis.ticks=element_line(color="black", size=0.5),
    axis.ticks.length=unit(-0.25, "cm"),
    
    
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

plotA
ggsave(filename = "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/2021_analysis/plots/GS temperature_rolling_av.png", 
       device = "png" ,
       dpi=600,
       width = 16,
       height = 9)

###########################################################################################################################
# plotA
# GS_nonleap_leap_yrs_extract_narrow
# GS_temp_mean
###########################################################################################################################

###########################################################################################################################
### B growing degree days #####

GDD_all_yrs_narrow <- read.csv("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/GDD_all_yrs_narrow_extract_pts.csv")
GDD_all_yrs_narrow <- GDD_all_yrs_narrow %>%   mutate(year_as_double = as.double(year))

GDD_all_yrs_narrow_mean <- GDD_all_yrs_narrow %>% 
  group_by(year_as_double) %>% 
  summarise(GDD_all = mean(GDD_all))

GDD_all_yrs_narrow_mean$roll5_GDD = zoo::rollmean(GDD_all_yrs_narrow_mean$GDD_all, 5, na.pad=TRUE)

#for the large dataset
str(GDD_all_yrs_narrow)
min(GDD_all_yrs_narrow$GDD_all)
max(GDD_all_yrs_narrow$GDD_all)
mean(GDD_all_yrs_narrow$GDD_all)
## for the large dataset

#for the small dataset
str( GDD_all_yrs_narrow_mean)
min(GDD_all_yrs_narrow_mean$GDD_all, na.rm = TRUE)
max(GDD_all_yrs_narrow_mean$GDD_all, na.rm = TRUE)
mean(GDD_all_yrs_narrow_mean$GDD_all, na.rm = TRUE)


plotB  <- ggplot(GDD_all_yrs_narrow, aes(factor(year_as_double), GDD_all))+
  geom_boxplot()+
  geom_smooth(data= GDD_all_yrs_narrow_mean, aes(x= factor(year_as_double), y= roll5_GDD, group=1), color='blue', se=FALSE)+ #needs the group 1
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        plot.caption = element_text(hjust = 0))+
  labs(x = "",
       y = "Growing degree days (°C)")+
  #annotate("text", x = factor(1990), y = 1900, label = "(b)", size = 8)+
  
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

plotB
ggsave(filename = "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/2021_analysis/plots/Growing_degree_days_rolling_av.png", 
       device = "png" ,
       dpi=600,
       width = 16,
       height = 9)
###########################################################################################################################
# plotB
# GDD_all_yrs_narrow
# GDD_all_yrs_narrow_mean
###########################################################################################################################


###########################################################################################################################
### C Annual rainfall #####

### make changes to graphs - rainfall
rainfall_all_yrs_narrow <- read.csv("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/mean_rainfall_all_yrs_narrow_pts.csv") 
rainfall_all_yrs_narrow <- mutate(rainfall_all_yrs_narrow, year_as_double = as.double(year))

rainfall_all_yrs_mean <- rainfall_all_yrs_narrow %>% 
  group_by(year_as_double) %>% 
  summarise(mean_rainfall = mean(mean_rainfall))

rainfall_all_yrs_mean$roll5_annual_rain = zoo::rollmean(rainfall_all_yrs_mean$mean_rainfall, 5, na.pad=TRUE)


#for the large dataset
str(rainfall_all_yrs_narrow)
min(rainfall_all_yrs_narrow$mean_rainfall)
max(rainfall_all_yrs_narrow$mean_rainfall)
mean(rainfall_all_yrs_narrow$mean_rainfall)
## for the large dataset

#for the small dataset
str( GDD_all_yrs_narrow_mean)
min(rainfall_all_yrs_mean$roll5_annual_rain, na.rm = TRUE)
max(rainfall_all_yrs_mean$roll5_annual_rain, na.rm = TRUE)
mean(rainfall_all_yrs_mean$roll5_annual_rain, na.rm = TRUE)

plotC <- ggplot(rainfall_all_yrs_narrow, aes(factor(year_as_double), mean_rainfall))+
  geom_boxplot()+
  geom_smooth(data= rainfall_all_yrs_mean, aes(x= factor(year_as_double), y= roll5_annual_rain, group=1), color='blue', se=FALSE)+ #needs the group 1
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

plotC
ggsave(filename = "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/2021_analysis/plots/mean_rainfall_rolling_av.png", 
       device = "png" ,
       dpi=600,
       width = 16,
       height = 9)
###########################################################################################################################
# plotC
# rainfall_all_yrs_narrow
# rainfall_all_yrs_mean
###########################################################################################################################

###########################################################################################################################
### GGD rainfall #####

pts_GS_rain_temp_narrow <- read.csv("//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/Climate+Forecast+Data+aggregation/map_layers/pts_GS_rain_temp_narrow_pts.csv") 

GS_rain_mean <- pts_GS_rain_temp_narrow %>% 
  group_by(year_as_double) %>% 
  summarise(GS_rain = mean(GS_rain))

GS_rain_mean$roll5_GS_rain = zoo::rollmean(GS_rain_mean$GS_rain, 5, na.pad=TRUE)

#for the large dataset
str(pts_GS_rain_temp_narrow)
min(pts_GS_rain_temp_narrow$GS_rain)
max(pts_GS_rain_temp_narrow$GS_rain)
mean(pts_GS_rain_temp_narrow$GS_rain)
## for the large dataset

#for the small dataset
str( GDD_all_yrs_narrow_mean)
min(GS_rain_mean$roll5_GS_rain, na.rm = TRUE)
max(GS_rain_mean$roll5_GS_rain, na.rm = TRUE)
mean(GS_rain_mean$roll5_GS_rain, na.rm = TRUE)


plotD <- ggplot(pts_GS_rain_temp_narrow, aes(factor(year_as_double), GS_rain))+
  geom_boxplot()+
  geom_smooth(data= GS_rain_mean, aes(x= factor(year_as_double), y= roll5_GS_rain, group=1), color='blue', se=FALSE)+ #needs the group 1
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

plotD

ggsave(filename = "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/2021_analysis/plots/mean_GS_rainfall_rolling_av.png", 
       device = "png" ,
       dpi=600,
       width = 16,
       height = 10)

###########################################################################################################################
# plotD
# pts_GS_rain_temp_narrow
# GS_rain_mean
###########################################################################################################################


#### Lets try and put all the data togther ###

A <-  GS_nonleap_leap_yrs_extract_narrow
A_av <- GS_temp_mean
str(A)
A <- A %>% 
  select(X, Mean_GS_temp, year_as_double)
str(A_av)
A_av <- A_av %>% 
  select(year_as_double, roll5temp_mean)


B <- GDD_all_yrs_narrow
B_av <-  GDD_all_yrs_narrow_mean
str(B)
B <- B %>% 
  select(X,GDD_all)
str(B_av)
B_av <- B_av %>% 
  select(year_as_double, roll5_GDD )


C <- rainfall_all_yrs_narrow
C_av <- rainfall_all_yrs_mean
str(C)
C <- C %>% 
  select(X,mean_rainfall)
str(C_av)
C_av <- C_av %>% 
  select(year_as_double, roll5_annual_rain )


D <- pts_GS_rain_temp_narrow
D_av <- GS_rain_mean
str(D)
D <- D %>% 
  select(X,GS_rain)
str(D_av)
D_av <- D_av %>% 
  select(year_as_double, roll5_GS_rain )

###########################################################################################################
#join all the large datasets togther
A_B <- left_join(A, B)
A_B_C <- left_join(A_B, C)
A_B_C_D <- left_join(A_B_C, D)
str(A_B_C_D)
A_B_C_D <- A_B_C_D %>% 
  rename(climate_GS_temp = Mean_GS_temp,
         climate_GDD = GDD_all,
         climate_rainfall = mean_rainfall,
         climate_GS_rainfall = GS_rain)

## I need to make the dataset long make a clm called climate varaiable

str(A_B_C_D)
A_B_C_D_long <- A_B_C_D %>%
  pivot_longer(
    cols =  starts_with("climate"),
    names_to  = "climate_varaiable",
    values_to = "value"
  )

str(A_B_C_D_long)


###########################################################################################################
#join all the small datasets togther
A_B_av <- left_join(A_av, B_av)
A_B_C_av <- left_join(A_B_av, C_av)
A_B_C_D_av <- left_join(A_B_C_av, D_av)
str(A_B_C_D_av)
A_B_C_D_av <- A_B_C_D_av %>% 
  rename(climate_GS_temp = roll5temp_mean,
         climate_GDD = roll5_GDD,
         climate_rainfall = roll5_annual_rain,
         climate_GS_rainfall = roll5_GS_rain)

## I need to make the dataset long make a clm called climate varaiable

str(A_B_C_D_av)
A_B_C_D_av_long <- A_B_C_D_av %>%
  pivot_longer(
    cols =  starts_with("climate"),
    names_to  = "climate_varaiable",
    values_to = "average"
  )

str(A_B_C_D_av_long)

############################################################################################
### This is names and orders 
A_B_C_D_av_long$climate_varaiable <- ordered(A_B_C_D_av_long$climate_varaiable,
                                             levels = c("climate_GS_temp",
                                                        "climate_GDD",
                                                        "climate_rainfall",
                                                        "climate_GS_rainfall"),
                                             labels = c("Mean growing season temperature (°C)",
                                                        "Growing degree days (°C)",
                                                        "Annual rainfall (mm)",
                                                        "Growing season rainfall (mm)"))
str(A_B_C_D_long)
unique(A_B_C_D_long$climate_varaiable)
A_B_C_D_long$climate_varaiable <- ordered(A_B_C_D_long$climate_varaiable,
                                          levels = c("climate_GS_temp",
                                                     "climate_GDD",
                                                     "climate_rainfall",
                                                     "climate_GS_rainfall"),
                                          labels = c("Mean growing season temperature (°C)",
                                                        "Growing degree days (°C)",
                                                        "Annual rainfall (mm)",
                                                        "Growing season rainfall (mm)"))


subsetA_B_C_D_long <- A_B_C_D_long %>%
  filter(year_as_double == 2000 |
           year_as_double == 2001 |
           year_as_double == 2002)
subsetA_B_C_D_av_long <- A_B_C_D_av_long %>%
  filter(year_as_double == 2000 |
           year_as_double == 2001 |
           year_as_double == 2002)


ggplot(subsetA_B_C_D_long, aes(factor(year_as_double), value))+
  geom_boxplot()+
  geom_smooth(data= subsetA_B_C_D_av_long, aes(x= factor(year_as_double), y= average, group=1), color='blue', se=FALSE)+ #needs the group 1
  facet_grid(climate_varaiable~.,scales = "free" )

unique(subsetA_B_C_D_long$climate_varaiable)

scales_y <- list(
  `Mean growing season temperature (°C)`  =   scale_y_continuous(breaks = seq(from = 14, to = 21, by = 1)),
  `Growing degree days (°C)`  =               scale_y_continuous(breaks = seq(0, 2000, 250)),
  `Annual rainfall (mm)`  = scale_y_continuous(breaks = seq(250, 1300, 250)),
  `Growing season rainfall (mm)`=scale_y_continuous(breaks = seq(0, 800, 100)))





## whith subset of data 
plot_ABCD <- ggplot(subsetA_B_C_D_long, aes(factor(year_as_double), value))+
  geom_boxplot()+
  geom_smooth(data= subsetA_B_C_D_av_long, aes(x= factor(year_as_double), y=average , group=1), color='blue', se=FALSE)+ #needs the group 1
  # facet_grid(climate_varaiable ~. ,scales = "free", 
  #            switch="y", 
  #            labeller = label_wrap_gen(multi_line = TRUE))+
  facet_grid_sc(rows = vars(climate_varaiable), scales = list(y = scales_y),
                switch="y", 
                labeller = label_wrap_gen(multi_line = TRUE))+

  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        plot.caption = element_text(hjust = 0))+
  labs(x = "Year",
       y = "")+
  
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    
    axis.text.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14),
    axis.text.x=element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=14, vjust = 0.3),
    
    
    axis.ticks=element_line(color="black", size=0.5),
    axis.ticks.length=unit(-0.25, "cm")
    
  )+
  theme(
    strip.background = element_blank(),
    #strip.text.y = element_text(size = 14, angle=0)
    strip.text.y = element_text(size = 16, angle=-180),
    strip.placement = "outside")
  

plot_ABCD




### all the data
plot_ABCD <- ggplot(A_B_C_D_long, aes(factor(year_as_double), value))+
  geom_boxplot()+
  geom_smooth(data= A_B_C_D_av_long, aes(x= factor(year_as_double), y=average , group=1), color='blue', se=FALSE)+ #needs the group 1
  facet_grid_sc(rows = vars(climate_varaiable), scales = list(y = scales_y),
                switch="y", 
                labeller = label_wrap_gen(multi_line = TRUE))+
  
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        plot.caption = element_text(hjust = 0))+
  labs(x = "Year",
       y = "")+
  
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    
    axis.text.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14),
    axis.text.x=element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=14, vjust = 0.3),
    
    
    axis.ticks=element_line(color="black", size=0.5),
    axis.ticks.length=unit(-0.25, "cm")
    
  )+
  theme(
    strip.background = element_blank(),
    #strip.text.y = element_text(size = 14, angle=0)
    strip.text.y = element_text(size = 16, angle=-180),
    strip.placement = "outside")

plot_ABCD


#save the plot and data
# the plot
ggsave(filename = "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/2021_analysis/plots/ABCD_plots.png", 
       device = "png" ,
       dpi=600,
       width = 10,
       height = 14)
# the data
str(A_B_C_D_long)
str(A_B_C_D_av_long)
write.csv(A_B_C_D_long,
          "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/2021_analysis/plots/A_B_C_D_long.csv") 
write.csv(A_B_C_D_av_long,
          "//FSSA2-ADL/CLW-SHARE3/Viticulture/Barossa terroir/climate/2021_analysis/plots/A_B_C_D_av_long.csv") 
