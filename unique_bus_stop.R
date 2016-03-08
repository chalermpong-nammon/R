#bus <- read.csv("/Users/chalermpongsomdulyawat/Desktop/Grad_workspace/bus_data/bus_with_ridership_4_1_2010.csv", sep = "", quote = "\"'")
bus <- read.csv("/Users/chalermpongsomdulyawat/Desktop/Grad_workspace/busOneMonth.csv")

level.bus.stop <- (unique(bus$bus_stop))

unique.bus.stop <- bus[!duplicated(bus$bus_stop),]

write.table(unique.bus.stop, file = "/Users/chalermpongsomdulyawat/Desktop/Grad_workspace/unique_bus_stop.csv")