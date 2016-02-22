#bus <- read.csv("/Users/chalermpongsomdulyawat/Desktop/Grad_workspace/bus_one_month.csv")
#ticket <- read.csv("/Users/chalermpongsomdulyawat/Desktop/Grad_workspace/ticket_one_month.csv")
#level.busid <- (unique(bus$bus_id))
#level.busline <- unique(bus$bus_line)
#level.working_day <- unique(bus$working_day)
#strptime(bus$timestamp, "%Y-%m-%d %H:%M:%S")
#strptime(ticket$timestamp, "%Y-%m-%d %H:%M:%S")

#creat new colum for collect ridership
bus$ridership <- 0
ticket$used <- 0

bus$date <- as.numeric(as.Date(bus$timestamp))
ticket$date <- as.numeric(as.Date(ticket$timestamp))


#valuable for debug
debug_bus <- 0
debug_ticket <- 0

#select ticket only equre busline and busid of busstop that have in database
ticket <- ticket[ticket$bus_line %in%  level.busline , ]
ticket <- ticket[ticket$bus_id %in% level.busid, ]

for(i in 1:10){
  first.time = Sys.time()
  #select bus only equre busline and busid of ticket[i,]
  bus.from.ticket.i <- bus[bus$bus_line == ticket[i,]$bus_line 
                           & bus$bus_id == ticket[i,]$bus_id
                           & bus$date == ticket[i,]$date, ]
  #find index of bus that is min diff time
  row.of.min.diff <- bus.from.ticket.i[abs(as.numeric(difftime(bus.from.ticket.i$timestamp, ticket[i,]$timestamp))) 
                                       == min(abs(as.numeric(difftime(bus.from.ticket.i$timestamp, ticket[i,]$timestamp)))),]

  if(nrow(row.of.min.diff) > 1){ row.of.min.diff = row.of.min.diff[1,]}
  
  #plus rider ship at row of bus min diff
  bus[as.numeric(rownames(row.of.min.diff)),]$ridership = bus[as.numeric(rownames(row.of.min.diff)),]$ridership+1
  last.time = Sys.time()
  print(last.time-first.time)
  print(i)
}

write.table(bus, file = "/Users/chalermpongsomdulyawat/Desktop/Grad_workspace/bus_one_month_with_ridership.csv")
write.table(ticket, file = "/Users/chalermpongsomdulyawat/Desktop/Grad_workspace/ticket_one_month_with_ridership.csv")


