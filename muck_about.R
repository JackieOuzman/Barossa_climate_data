


start_year <- '1988'#want this for final data
start_year <- '2015' #test on subset





i_start_test <- which(substring(raster_file_mon, 1,4) == 1891)
#This is making a list of years removing the .monthly_tain.nc
print(i_start_test)
print(raster_file_mon)


test <- substring(raster_file_mon, 1,4)
print(test)




#######################################################################################
############    Having trouble here ###################################################
print(chunks) # grouped into 4 lists this is the months in the dataset
# list 1 is 1-240, list 2 is 241 - 480, list 3 481 -720, list 4 721-816
length(chunks)

list_check <- chunks[[1]] #this is just my first list of months #240
print(list_check)
list_check_long <- rep(1:50, each = 1) #same as above but not dervied from a subset of lists
print(list_check_long)

list_check_short <- rep(1:20, each = 1) #20 month
print(list_check_short)

b_i <- b_a2[[list_check]] #put the monthly data from my first list into this raster
b_i_short <- b_a2[[list_check_short]]
b_i_short

b_i_long <- b_a2[[list_check_long]]
b_i_long

#indices_check <- rep(1:(length(list_check)/12), each = 12)
indices_check_short <- rep(1:(length(list_check_short)/10), each = 10)
print(indices_check_short)

indices_check_long <- rep(1:(length(list_check_long)/10), each = 10)
print(indices_check_long)
#this creates a list of 240 values the first 12 values are 1 this might realte to the first 12 month in the first year of data
print(indices_check)

b_sumi_test <- stackApply(b_i_short, indices= c(indices_check_short), fun = sum, na.rm=TRUE)
b_sumi_test

b_sumi_test_long <- stackApply(b_i_long, indices= c(indices_check_long), fun = sum, na.rm=TRUE)
b_sumi_test_long

#stack apply apply the sum function onto as subser of a raster stack
#raster stack is b_1 this is the daily rainfall raster,
#indices_i is the montly data grouped into years
#the function is sum
#not working why???






plot(b_a2) #this is all the years and month raster into one raster
for (i in 1:length(chunks)) {
  list_i <- chunks[[i]]
  b_i <- b_a2[[list_i]]
  indices_i <- rep(1:(length(list_i)/12), each = 12)
  b_sumi <- stackApply(b_i, indices_i, fun = sum, na.rm=TRUE)
  b_sum <- stack(b_sum, b_sumi)
}