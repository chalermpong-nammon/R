#Naive Bayes classifier grid cell
list.of.packages <- c("plyr", "e1071", "caret", "e1071", "TTR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(plyr);
library(e1071);
library(caret);
library(TTR);
#<------------------------------------ START FUNCTION ---------------------------------------------------->

root.path = "~/Grad_workspace/"
read.file.to.data.frame <- function(cell.size){
  km.extension = "m/cell_with_ridership_"
  km = "m_"
  if(cell.size >= 1000){
    cell.size = cell.size/1000
    km.extension = "km/cell_with_ridership_"
    km = "km_"
  }
  
  month.year = "_1_2010.csv"
  cell.path <- paste0(root.path,
                      "cell_with_ridership_",
                      cell.size,
                      km.extension,
                      cell.size,
                      km
                      , collapse = NULL);
  graph <- list();
  for(f in 4:31){
    bus.path = paste(cell.path, f, sep = "")
    bus.path = paste(bus.path, month.year, sep = "")
    cells <- read.csv( bus.path ,  sep = "", quote = "\"'")
    
    timeslot.with.ridership <- ddply(cells, c('timeslot',"cell"), summarise, ridership  = sum(ridership) );
    timeslot.with.ridership$timeslot <- as.POSIXct(timeslot.with.ridership$timeslot)
    timeslot.with.ridership[strftime(timeslot.with.ridership$timeslot, format="%H:%M:%S") == "00:00:00",]$timeslot <- timeslot.with.ridership[strftime(timeslot.with.ridership$timeslot, format="%H:%M:%S") == "00:00:00",]$timeslot -1
    timeslot.with.ridership$timeslot <- strftime(timeslot.with.ridership$timeslot)
    timeslot.with.ridership$day <- weekdays(as.Date(timeslot.with.ridership$timeslot))
    
    graph[[f]] <- timeslot.with.ridership
  }
  df <- ldply (graph, data.frame)
  df$time <- strftime(df$timeslot, format="%H:%M:%S")
  return(df)
}

make.class <- function(ridership, tolerance){
  return (cut(ridership, breaks = seq(min(ridership)-tolerance, max(ridership)+tolerance, by = tolerance)) )
}

make.factor.for.NB <- function(df){
  #for map foctor to number
  df$time <- as.factor(df$time)
  df$day <- as.factor(df$day)
  df$class <- as.factor(df$class)
  return(df)
}

simple.moving.average.model.prediction <- function(serie.value, previous_){
  SMA <- SMA(serie.value, n=previous_)
  SMA[is.na(SMA)] <- 0
  return(SMA)
}

weigth.moving.average.model.prediction <- function(serie.value, previous_){
  WMA <- WMA(serie.value, n=previous)
  WMA[is.na(WMA)] <- 0
  return(WMA)
}

exponential.moving.average.model.prediction <- function(serie.value, previous_){
  EMA <- EMA(serie.value, n = previous_)
  EMA[is.na(EMA)] <- 0
  return(EMA)
}

process.MA.error <- function(df.cell){
  df.cell$error <- 0
  for(i in 1:nrow(df.cell)){
    if(i != 1 ){
      df.cell[i, ]$error <- df.cell[i, ]$ridership - df.cell[(i-1), ]$MAf
    }
  }
  return(df.cell$error)
}

assign.df.cell.test.colum <- function(df.cell.test){
  df.cell.test$min_predict <- 0
  df.cell.test$max_predict <- 0
  df.cell.test$min_NB_MA <- 0
  df.cell.test$max_NB_MA <- 0
  df.cell.test$predict_NB_MA <- FALSE
  return(df.cell.test)
}

map.MA.to.NB <- function(df.cell.test, nrow.trianing){
  for(j in 1:nrow(df.cell.test)) {
    df.cell.test[j,]$min_predict = as.numeric(sub("\\((.+),.*", "\\1", df.cell.test[j,]$prediction))
    df.cell.test[j,]$max_predict = as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", df.cell.test[j,]$prediction))
    
    if (j == 1) {
      df.cell.test[j,]$min_NB_MA <- df.cell.test[j,]$min_predict + df.cell.trianing[nrow.trianing,]$MA_error
      df.cell.test[j,]$max_NB_MA <- df.cell.test[j,]$max_predict + df.cell.trianing[nrow.trianing,]$MA_error
    }else{
      df.cell.test[j,]$min_NB_MA <- df.cell.test[j,]$min_predict + df.cell.test[(j - 1),]$MA_error
      df.cell.test[j,]$max_NB_MA <- df.cell.test[j,]$max_predict + df.cell.test[(j - 1),]$MA_error
    }
    
    if((df.cell.test[j, ]$ridership > df.cell.test[j, ]$min_NB_MA) & (df.cell.test[j, ]$ridership <= df.cell.test[j, ]$max_NB_MA)){
      df.cell.test[j, ]$predict_NB_MA = TRUE;
    }else{
      df.cell.test[j, ]$predict_NB_MA = FALSE;
    }
  }
  return(df.cell.test)
}

get.end.line.index.trianning = function(df.cell){
  return (tail(which(df.cell$timeslot == "2010-01-24 23:59:59"), n =1));
}

select.trianing <- function(df){
  endline.of.trainning = get.end.line.index.trianning(df);
  df.training <- df[1:endline.of.trainning,]
  return(df.training)
}

select.test <- function(df){
  startline.of.test = get.end.line.index.trianning(df)+1
  df.test <- df[startline.of.test:nrow(df),]
  return(df.test)
}

naive.bayes.model.prediction <- function(df.training, df.test){
  model <- naiveBayes(class~., data = df.training)
  return(predict(model, df.test))
}

accuracy.naive.bayes <- function(prediction, class){
  cm <- confusionMatrix(prediction, class)
  overall <- cm$overall
  return( overall['Accuracy'] )
}

write.accuracy.NB.to.file = function(dataFrame){ 
  write.bus.path <- paste0(root.path, 
                           "Result/Accuracy/NB/sumarize.csv", 
                           collapse = NULL); 
  write.table(dataFrame, file = write.bus.path, row.names = FALSE, quote=c(1),sep = ",") 
} 

save.accuracy.cell.size <- function(accuracy.each.cell){
  df.accuracy.cell.size <- data.frame(Tolerance = seq(5,40, by = 5))
  for(cell.size in c(100,250,500,750,1000,1250,1500)){
    
    v.average.accuracy <- vector('numeric')
    for(tolerance in seq(5,40, by = 5)){
      accuracy.selected <- accuracy.NB.each.cell[accuracy.NB.each.cell$cell_size == cell.size & accuracy.NB.each.cell$tolerance == tolerance,]
      sum.all.accuracy.cell <- sum(accuracy.selected$accuracy_NB)
      num.row <- nrow(accuracy.selected)
      average.accuracy <- sum.all.accuracy.cell/num.row
      v.average.accuracy <- append(v.average.accuracy, average.accuracy)
    }
    df.accuracy.cell.size[toString(cell.size)] <- v.average.accuracy
  }
  return(df.accuracy.cell.size)
}


plot.graph.train.and.test <- function(df, cell.number, prediction){
  cell.of.number <- df[df$cell == cell.number,]
  cell.of.number$timeslot <- as.POSIXct(cell.of.number$timeslot)
  ridership <- as.numeric(cell.of.number$class)*tolerance
  timeslot <- as.POSIXct(cell.of.number$timeslot)
  plot(timeslot, ridership, type="l")
  
  title(main = paste(c("Cell Number ", cell.number), collapse = ""))
  par(new=TRUE)
  lines(cell.of.number$timeslot, (as.numeric(cell.of.number$class)*tolerance),col = "blue")
  par(new=TRUE)
  
  df.test.tmp <- df[(get.end.line.index.trianning(df)+1):nrow(df), ]
  test.cell <- df.test.tmp[df.test.tmp$cell == cell.number, ]
  test.cell.time <- as.POSIXct(test.cell$timeslot)
  lines(test.cell.time, as.numeric(prediction)*tolerance,col="red")
}

#<-------------------------------------------END FUNCTION------------------------------------------------>
