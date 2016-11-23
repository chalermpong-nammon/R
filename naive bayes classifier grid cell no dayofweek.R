#Naive Bayes classifier grid cell
library(plyr);
library(plotly);
library(class) ;
library(e1071); 
library(caret)
library(klaR)
library(ggplot2) 
library(RColorBrewer)

month.year = "_1_2010.csv"

#start.time = Sys.time();
cell.path = "/Users/chalermpongsomdulyawat/Desktop/Grad_workspace/cell_with_ridership_1km/cell_with_ridership_1km_"

# path for write file csv
write.cell.path = "/Users/chalermpongsomdulyawat/Desktop/Grad_workspace/naive_bayes_only_time_1km_"
graph <- list();
graph.test <- list();

# include tainnig data
for(f in 4:31){
  
  bus.path = paste(cell.path,f, sep = "")
  bus.path = paste(bus.path, month.year, sep = "")
  cells <- read.csv( bus.path ,  sep = "", quote = "\"'")
  
  timeslot.with.ridership <- ddply(cells, c('timeslot',"cell"), summarise, ridership  = sum(ridership) );
  timeslot.with.ridership$timeslot <- as.POSIXct(timeslot.with.ridership$timeslot)
  timeslot.with.ridership[strftime(timeslot.with.ridership$timeslot, format="%H:%M:%S") == "00:00:00",]$timeslot <- timeslot.with.ridership[strftime(timeslot.with.ridership$timeslot, format="%H:%M:%S") == "00:00:00",]$timeslot -1
  timeslot.with.ridership$timeslot <- strftime(timeslot.with.ridership$timeslot)
  timeslot.with.ridership$day <- weekdays(as.Date(timeslot.with.ridership$timeslot))
  
  
  graph[[f]] <- timeslot.with.ridership
  
}

# 4 week data  1week= 336 3week = 1008
df <- ldply (graph, data.frame)
df$time <- strftime(df$timeslot, format="%H:%M:%S")

df$class <- cut(df$ridership, breaks = seq(min(df$ridership)-20, max(df$ridership), by = 20))

#for map foctor to number
map.to.num <- max(df$ridership)/20

df$time <- as.factor(df$time)



# 3 week
df.training <- df[1:139824,]
df.training <- df.training[,c(2,3,5,6)]

# 1 week
df.test <- df[139825:nrow(df),]
df.test <- df.test[,c(2,3,5,6)]


cell <- unique(df.training$cell)
df.no.dayofweek.accuracy <- data.frame(cell = numeric(0), accuracy = numeric(0))

for(cell.index in 1:length(cell)){
  df.training.cell <- df.training[df.training$cell == cell[cell.index],]
  df.test.cell <- df.training[df.training$cell == cell[cell.index],]
  model <- naiveBayes(class~., data=df.training.cell)
  prediction <- predict(model, df.test.cell)
  df.test.cell$prediction <- prediction
  table.predic <- table(prediction, df.test.cell$class)
  cm <- confusionMatrix(prediction, df.test.cell$class)
  overall <- cm$overall
  overall.accuracy <- overall['Accuracy']
  df.no.dayofweek.accuracy[cell.index,]$cell = cell[cell.index]
  df.no.dayofweek.accuracy[cell.index,]$accuracy = overall.accuracy
  
}


########   plot heat map


