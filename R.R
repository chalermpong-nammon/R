#bus <- read.csv("/Users/chalermpongsomdulyawat/Desktop/R/bus_stop917.csv")
#ticket <- read.csv("/Users/chalermpongsomdulyawat/Desktop/R/bus_ticker_1M.csv")
#level <- (unique(bus$bus_id))
#strptime(bus$timestamp, "%Y-%m-%d %H:%M:%S")
#strptime(ticket$timestamp, "%Y-%m-%d %H:%M:%S")

bus$ridership <- 0
ticket$used <- 0

debug_bus <- 0
debug_ticket <- 0


for(i in 1:1){
  ticket.i <- ticket[ticket[,"bus_line"] == level.busline,]
  unique(ticket$bus_line)
  bus.from.ticket.i <- bus[bus$bus_line == ticket[i,]$bus_line & bus$bus_id == ticket[i,]$bus_id, ]
  bus.from.ticket.i[as.numeric(difftime(bus.from.ticket.i$timestamp, ticket[1,]$timestamp)) == min(as.numeric(difftime(bus.from.ticket.i$timestamp, ticket[1,]$timestamp))),]
  # get index bus in data frame
  bus[as.numeric(rownames(bus.from.ticket.i[as.numeric(difftime(bus.from.ticket.i$timestamp, ticket[1,]$timestamp)) == min(as.numeric(difftime(bus.from.ticket.i$timestamp, ticket[1,]$timestamp))),]
  )),]$ridership + 1
}
