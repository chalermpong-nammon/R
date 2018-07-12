#Naive Bayes classifier grid cell
list.of.packages <- c("plyr", "plotly", "class", "e1071", "caret", "klaR", "ggplot2", "RColorBrewer", "TTR", "zoom")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")
library(plyr);
library(plotly);
library(class) ;
library(e1071); 
library(caret)
library(klaR)
library(ggplot2) 
library(RColorBrewer)
library(TTR)
library(zoom)
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
  return (cut(ridership, breaks = seq(min(ridership)-tolelance, max(ridership)+tolelance, by = tolelance)) )
}

select.trianing <- function(df){
  endline.of.trainning = get.start.line.of.test(df) - 1;
  df.training <- df[1:endline.of.trainning,]
  df.training <- df.training[,c("cell","day","time","class")]
  return(df.training)
}

select.test <- function(df){
  startline.of.test = get.start.line.of.test(df)
  df.test <- df[startline.of.test:nrow(df),]
  df.test <- df.test[,c("cell","day","time","class")]
  return(df.test)
}

get.end.line.trianning.ma = function(df.cell){
  return (tail(which(df.cell$timeslot == "2010-01-24 23:59:59"), n =1));
}

get.start.line.of.test <- function(df){
  return (head(which(df$timeslot == "2010-01-25 00:30:00"), n =1));
}

naive.bayes.model.prediction <- function(df.training.cell, df.test.cell){
  model <- naiveBayes(class~., data=df.training.cell)
  return(predict(model, df.test.cell))
}

moving.average.model.prediction <- function(df.cell, previous_){
  df.cell$MA <- 0
  SMA <- SMA(df.cell[ 1:nrow(df.cell),]$ridership, n=previous_)
  SMA[is.na(SMA)] <- 0
  df.cell$MA <- SMA
  
  df.cell$error <- 0
  for(i in 1:nrow(df.cell)){
    df.cell[i, ]$error = df.cell[i, ]$ridership - df.cell[i, ]$MA
  }  
  return(df.cell)
}

exponential.moving.average.model.prediction <- function(df.cell, previous_){
  df.cell$MA <- 0
  MA <- EMA(df.cell[1:nrow(df.cell),]$ridership, n = previous_)
  MA[is.na(MA)] <- 0
  df.cell$MA <- MA
  
  df.cell$error <- 0
  for (i in 1:nrow(df.cell)) {
    df.cell[i,]$error = df.cell[i,]$ridership - df.cell[i,]$MA
  }
  return(df.cell)
}

assign.df.test.cell.colum <- function(df.test.cell){
  df.test.cell$MA_error <- 0
  df.test.cell$min_class <- 0
  df.test.cell$max_class <- 0
  df.test.cell$min_Naive_MA <- 0
  df.test.cell$max_Naive_MA <- 0
  df.test.cell$predict_Naive_MA <- FALSE
  return(df.test.cell)
}

map.ma.data.to.naive.bayes <- function(df.test.cell, df.cell){
  endline.of.trainning.ma = get.end.line.trianning.ma(df.cell)
  for(j in 1:nrow(df.test.cell)){
    df.test.cell[j, ]$MA_error = df.cell[j+(endline.of.trainning.ma),]$error
    df.test.cell[j, ]$min_class = as.numeric( sub("\\((.+),.*", "\\1", df.test.cell[j, ]$prediction))
    df.test.cell[j, ]$max_class = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", df.test.cell[j, ]$prediction))
    
    if(j > 1 ){
     df.test.cell[j, ]$min_Naive_MA <- df.test.cell[j, ]$min_class + df.test.cell[j-1, ]$MA_error
     df.test.cell[j, ]$max_Naive_MA <- df.test.cell[j, ]$max_class + df.test.cell[j-1, ]$MA_error
    }else{
      df.test.cell[j, ]$min_Naive_MA <- df.test.cell[j, ]$min_class
      df.test.cell[j, ]$max_Naive_MA <- df.test.cell[j, ]$max_class
    }
    line.error <- j
    if(df.test.cell[j, ]$max_Naive_MA <= 0){
      df.test.cell[j, ]$max_Naive_MA = 0
    }
    
    if((df.test.cell[j, ]$ridership > df.test.cell[j, ]$min_Naive_MA) 
       & (df.test.cell[j, ]$ridership <= df.test.cell[j, ]$max_Naive_MA)){
      df.test.cell[j, ]$predict_Naive_MA = TRUE;
    }else{
      df.test.cell[j, ]$predict_Naive_MA = FALSE;
    }
  }
  return(df.test.cell)
}

accuracy.naive.bayes <- function(prediction, class){
  cm <- confusionMatrix(prediction, class)
  overall <- cm$overall
  return( overall['Accuracy'] )
}

#plot graph all ridership of cell
plot.graph.train.and.test <- function(df, cell.number, prediction){
  cell.of.number <- df[df$cell == cell.number,]
  cell.of.number$timeslot <- as.POSIXct(cell.of.number$timeslot)
  ridership <- as.numeric(cell.of.number$class)*tolelance
  timeslot <- as.POSIXct(cell.of.number$timeslot)
  plot(timeslot, ridership, type="l")
  
  title(main = paste(c("Cell Number ", cell.number), collapse = ""))
  par(new=TRUE)
  lines(cell.of.number$timeslot, ((as.numeric(cell.of.number$class)-1)*tolelance),col = "blue")
  par(new=TRUE)
  
  df.test.tmp <- df[get.start.line.of.test(df):nrow(df), ]
  test.cell <- df.test.tmp[df.test.tmp$cell == cell.number, ]
  test.cell.time <- as.POSIXct(test.cell$timeslot)
  lines(test.cell.time, (as.numeric(prediction)-1)*tolelance,col="red")
}

plot.graph.ridership.and.test <- function(df, cell.number, test.cell){
  cell.of.number <- df[df$cell == cell.number,]
  cell.of.number <- cell.of.number[get.start.line.of.test(cell.of.number):nrow(cell.of.number),]
  ridership <- cell.of.number$ridership
  timeslot <- as.POSIXct(cell.of.number$timeslot)
  
  plot(timeslot, ridership, type="o", col = "green")
  
  title(main = paste(c("Cell Number ", cell.number), collapse = ""))
  par(new=TRUE)
  
  lines(timeslot, test.cell$min_class,col = "blue", lty = "dashed")
  par(new=TRUE)
  lines(timeslot, test.cell$max_class,col = "blue")
  par(new=TRUE)
  
  lines(timeslot, test.cell$min_Naive_MA,col = "red", lty = "dashed")
  par(new=TRUE)
  lines(timeslot, test.cell$max_Naive_MA,col = "red")
}

write.accuracy.to.file = function(dataFrame,cellSize, tolelance){
  write.bus.path <- paste0(root.path,
                           "Result/Accuracy/cellsize_",
                           cellSize,
                           "_tolelance_",
                           tolelance,
                           ".csv",
                           collapse = NULL);
  write.table(dataFrame, file = write.bus.path, row.names = FALSE, quote=c(1),sep = ",")
}

write.accuracy.nb.ma.to.file = function(dataFrame, type.ma){
  write.bus.path <- paste0(root.path,
                           "accuracy/NB_plus_",
                           type.ma,
                           ".csv",
                           collapse = NULL);
  write.table(dataFrame, file = write.bus.path, row.names = FALSE, quote=c(1),sep = ",")
}
#<-------------------------------------------END FUNCTION------------------------------------------------>
