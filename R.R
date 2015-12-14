#bus <- read.csv("/Users/chalermpongsomdulyawat/Desktop/R/bus_stop917.csv")
#ticket <- read.csv("/Users/chalermpongsomdulyawat/Desktop/R/bus_ticker_1M.csv")
#level <- (unique(bus$bus_id))
#strptime(bus$timestamp, "%Y-%m-%d %H:%M:%S")
#strptime(ticket$timestamp, "%Y-%m-%d %H:%M:%S")

bus$ridership <- 0
ticket$used <- 0

debug_bus <- 0
debug_ticket <- 0


for(i in 1:10){
  first.time = Sys.time()
  ticket_timestamp <- ticket[i, ]$timestamp
  ticket_busId <- ticket[i, ]$bus_id
  ticket_busLine <- ticket[i, ]$bus_line
  near_time <- 99999999999999999999999999999999999
  row_bus_ticket_use <- -1
  for(j in 1:nrow(bus)){
    bus_timestamp <- bus[j , ]$timestamp
    if (bus[j ,]$bus_id == ticket_busId & bus[j, ]$bus_line == ticket_busLine){
      diff_timestamp <- as.numeric(difftime(bus_timestamp, ticket_timestamp, units="secs"))
      if(diff_timestamp >= 7200){return}
      diff_timestamp <- abs(diff_timestamp)
      if (diff_timestamp <= near_time){
        row_bus_ticket_use = j;
        near_time = diff_timestamp;
        debug_bus = debug_bus+1;
      }
    }
  }
  if(row_bus_ticket_use != -1){
    bus[row_bus_ticket_use, ]$ridership <- bus[row_bus_ticket_use, ]$ridership + 1
    ticket[i, ]$used= 1
    debug_ticket <- debug_ticket+1
  }
  last.time = Sys.time()
  total.time = last.time-first.time
  print(total.time)
}


for(i in 1:nrow(ticket)){
  bus.from.ticket.i <- bus[bus$bus_line == ticket[i,]$bus_line & bus$bus_id == ticket[i,]$bus_id, ]
  bus.from.ticket.i[as.numeric(difftime(bus.from.ticket.i, ticket[1,]$timestamp)) == min(as.numeric(difftime(bus.from.ticket.i, ticket[1,]$timestamp))),]
  
}