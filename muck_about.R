


start_year <- '1988'#want this for final data
start_year <- '2015' #test on subset





i_start_test <- which(substring(raster_file_mon, 1,4) == 1891)
#This is making a list of years removing the .monthly_tain.nc
print(i_start_test)
print(raster_file_mon)


test <- substring(raster_file_mon, 1,4)
print(test)



for (i in 1:length(chunks)) {
  list_i <- chunks[[i]]
  b_i <- b_a2[[list_i]]
  indices_i <- rep(1:(length(list_i)/12), each = 12)
  b_sumi <- stackApply(b_i, indices_i, fun = sum, na.rm=TRUE)
  b_sum <- stack(b_sum, b_sumi)
}