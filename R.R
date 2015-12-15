#bus <- read.csv("/Users/chalermpongsomdulyawat/Desktop/Grad_workspace/bus_stop917.csv")
#ticket <- read.csv("/Users/chalermpongsomdulyawat/Desktop/Grad_workspace/bus_ticker_1M.csv")
#level.busid <- (unique(bus$bus_id))
#level.busline <- unique(bus$bus_line)
#strptime(bus$timestamp, "%Y-%m-%d %H:%M:%S")
#strptime(ticket$timestamp, "%Y-%m-%d %H:%M:%S")

bus$ridership <- 0
ticket$used <- 0

debug_bus <- 0
debug_ticket <- 0

#select ticket only equre busline and busid of busstop
ticket <- ticket[ticket$bus_line %in%  level.busline , ]
ticket <- ticket[ticket$bus_id %in% level.busid, ]

for(i in 1:nrow(ticket)){
  #select bus only equre busline and busid of ticket[i,]
  bus.from.ticket.i <- bus[bus$bus_line == ticket[i,]$bus_line 
                           & bus$bus_id == ticket[i,]$bus_id, ]
  #find index of bus that is min diff time
  row.of.min.diff <- bus.from.ticket.i[abs(as.numeric(difftime(bus.from.ticket.i$timestamp, ticket[i,]$timestamp))) 
                                       == min(abs(as.numeric(difftime(bus.from.ticket.i$timestamp, ticket[i,]$timestamp)))),]
  if(nrow(row.of.min.diff) > 1){ row.of.min.diff = row.of.min.diff[1,]}
  #plus rider ship at row of bus min diff
  bus[as.numeric(rownames(row.of.min.diff)),]$ridership = bus[as.numeric(rownames(row.of.min.diff)),]$ridership+1
  print(i)
}


