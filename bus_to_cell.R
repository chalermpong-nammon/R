base.path.bus = "/Users/santi/Desktop/nammon/bus_with_ridership_"
month.year = "_1_2010.csv"

# path for write file csv
write.bus.path = "/Users/santi/Desktop/nammon/time_interval_with_ridership_"

for(f in 1:31){
  bus.path = paste(base.path.bus,f, sep = "")
  bus.path = paste(bus.path, month.year, sep = "")
  bus <- read.csv(bus.path,  sep = "", quote = "\"'")
  
  level.bus.stop <- unique(bus$bus_stop)
  
  cell.df <- 0
  
  lat.start <- 38.667
  lat.end <- 38.7987
  
  lng.start <- -9.25253
  lng.end <- -9.09408
  
  time.slot <- ISOdatetime(2010,01,01,0,0,0) + seq(0:47)*30*60
  
  time.interval.with.ridership <- data.frame()
  
  
  for(i in 1: length(time.slot) ){
    # subset in time
    if(i == 1){
      data.time_slot <- subset(bus,as.POSIXct(bus$timestamp) <= time.slot[i])
    }else{
      data.time_slot <- subset(bus, as.POSIXct(bus$timestamp) > time.slot[i-1] & as.POSIXct(bus$timestamp) <= time.slot[i])
    }
    
    #create data frame of all bus stop
    bus.30min <- data.frame(level.bus.stop)
    
    #assign time for all bus stop = 00:30
    bus.30min$timeslot <- time.slot[i] 
    
    bus.30min$ridership <- 0
    
    for(j in 1: length(level.bus.stop)){
      bus.stop.id = level.bus.stop[j]
      bus.30min[bus.30min$level.bus.stop == bus.stop.id, ]$ridership <- sum(data.time_slot[ data.time_slot$bus_stop == bus.stop.id, ]$ridership)
    }
    
    
    time.interval.with.ridership <- rbind(time.interval.with.ridership, bus.30min)
    
    print(i)
    
  }
  
  print("day: "+i)
  write.bus.path.dot.csv = paste(write.bus.path, f, sep = "")
  write.bus.path.dot.csv = paste(write.bus.path.dot.csv, month.year, sep = "")
  write.table(bus, file = write.bus.path.dot.csv)

}


# row = 1
# for(i in 1: lengths(level.bus.stop)){
#   
#   
#   for(j in 1: lengths(time.slot)){
#     cell.df[row,]$bus_stop = level.bus.stop[i]
#     cell.df[row,]$time_slot = time.slot[j]
#     row = row + 1;
#   }
# }

# 
# # subset in time
# df1 <- subset(bus, as.POSIXct(bus$timestamp) <= time.slot[2] & as.POSIXct(bus$timestamp) > time.slot[1])
# 
# #create data frame of all bus stop
# bus.30min <- data.frame(level.bus.stop) #or use bus.stop.id <- bus[unique(bus$bus_stop), ]
# 
# #assign time for all bus stop = 00:30
# bus.30min$timeslot <- time.slot[1] 
# 
# bus.30min$ridership <- 0
# 
# ##sum ridership of bus stop
# sum(df1[ df1$bus_stop == level.bus.stop[3],]$ridership)
# 
# cell.with.ridership <- data.frame()
# cell.with.ridership <- rbind(cell.with.ridership, bus.30min)
# 
# 
# # loop for row of cell
# # use seq(38.7987, 38.667, by = -0.0001) or
# # (1, 1317, by = 1)
# for(cell.row in 1:1317){
#   
#   for(cell.col in 1:835){
#     if( lng <= ((cell.col*0.00019)+lng.start) & lat <= ((cell.row*0.0001)+lat.start) ){
#       #cell[]
#     }
#   }
#   
# }