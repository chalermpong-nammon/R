#Naive Bayes classifier grid cell
library(plyr);
library(plotly);
library(class) ;
library(e1071); 
library(caret)
library(klaR)
library(ggplot2) 
library(RColorBrewer)
library(TTR)
#<------------------------------------ START FUNCTION ---------------------------------------------------->

#plot graph all ridership of cell
plot.graph.train.and.test <- function(df, cell.number, prediction){
  cell.of.number <- df[df$cell == cell.number,]
  cell.of.number$timeslot <- as.POSIXct(cell.of.number$timeslot)
  ridership <- as.numeric(cell.of.number$class)*tolelance
  timeslot <- as.POSIXct(cell.of.number$timeslot)
  plot(timeslot, ridership, type="l")
  
  title(main = paste(c("Cell Number ", cell.number), collapse = ""))
  par(new=TRUE)
  lines(cell.of.number$timeslot, (as.numeric(cell.of.number$class)*tolelance),col = "blue")
  par(new=TRUE)
  
  df.test.tmp <- df[get.start.line.of.test(df):nrow(df), ]
  test.cell <- df.test.tmp[df.test.tmp$cell == cell.number, ]
  test.cell.time <- as.POSIXct(test.cell$timeslot)
  lines(test.cell.time, as.numeric(prediction)*tolelance,col="red")
}


read.file.to.data.frame <- function(cell.size){
  month.year = "_1_2010.csv"
  cell.path <- paste0("/Users/chalermpongsomdulyawat/Desktop/Grad_workspace/cell_with_ridership_",
                      cell.size,
                      "m/cell_with_ridership_",
                      cell.size,
                      "m_"
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
  df.training <- df.training[,c(2,4,5,6)]
  return(df.training)
}

select.test <- function(df){
  startline.of.test = get.start.line.of.test(df)
  df.test <- df[startline.of.test:nrow(df),]
  df.test <- df.test[,c(2,4,5,6)]
  return(df.test)
}

get.end.line.trianning.ma = function(df.cell){
  return (head(which(df.cell$timeslot == "2010-01-24 23:59:59"), n =1));
}

get.start.line.of.test <- function(df){
  return (head(which(df$timeslot == "2010-01-25 00:30:00"), n =1));
}

naive.bayes.model.prediction <- function(df.training.cell, df.test.cell){
  model <- naiveBayes(class~., data=df.training.cell)
  return(predict(model, df.test.cell))
}

moving.average.model.prediction <- function(df.cell){
  # df.cell <- df[df$cell == cell.number,]
  df.cell$MA <- 0
  SMA <- SMA(df.cell[ 1:nrow(df.cell),]$ridership, n=previous)
  SMA[is.na(SMA)] <- 0
  df.cell$MA <- SMA
  
  df.cell$error <- 0
  for(i in 1:nrow(df.cell)){
    df.cell[i, ]$error = df.cell[i, ]$ridership - df.cell[i, ]$MA
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
    df.test.cell[j, ]$min_class = as.numeric( sub("\\((.+),.*", "\\1", df.test.cell[j, ]$class))
    df.test.cell[j, ]$max_class = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", df.test.cell[j, ]$class))
    
    df.test.cell[j, ]$min_Naive_MA <- df.test.cell[j, ]$min_class + df.test.cell[j, ]$MA_error
    df.test.cell[j, ]$max_Naive_MA <- df.test.cell[j, ]$max_class + df.test.cell[j, ]$MA_error
    
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
  table.predic <- table(prediction, class)
  cm <- confusionMatrix(prediction, class)
  overall <- cm$overall
  return( overall['Accuracy'] )
}


write.accuracy.to.file = function(dataFrame,cellSize, tolelance){
  write.bus.path = "/Users/chalermpongsomdulyawat/Desktop/Grad_workspace/accuracy_data/cellsize_"
  write.bus.path = paste(write.bus.path, cellSize, sep = "")
  write.bus.path = paste(write.bus.path, "_tolelance_", sep = "")
  write.bus.path = paste(write.bus.path, tolelance, sep = "")
  write.bus.path = paste(write.bus.path, ".csv", sep = "")
  write.table(dataFrame, file = write.bus.path, row.names = FALSE, quote=c(1),sep = ",")
}
#<-------------------------------------------END FUNCTION------------------------------------------------>


#<-------------------------------------------Main------------------------------------------------>
for(cell.size in seq(500, 100, by = -100)){
  print(cell.size)
  # tolelance = 20
  # cell.size = 500
  df <- read.file.to.data.frame(cell.size)
  for(tolelance in seq(5, 15, by = 5)){
    
    df$class <- make.class(df$ridership, tolerance)
    
    #for map foctor to number
    df$time <- as.factor(df$time)
    df$day <- as.factor(df$day)
    df$class <- as.factor(df$class)
    
    startline.of.test = get.start.line.of.test(df);
    endline.of.trainning = startline.of.test - 1;
    
    
    df.training <- select.trianing(df)  # 3 week
    df.test <- select.test(df)  # 1 week
    
    cell <- sort(unique(df.training$cell))
    
    df.accuracy <- data.frame(cell = numeric(0), naive_accuracy = numeric(0),  naive_ma_accuracy = numeric(0))
    
    previous = 10
    for(cell.index in 1:length(cell)){
      cell.number = cell[cell.index]
      df.training.cell <- df.training[df.training$cell == cell.number,]
      df.test.cell <- df.test[df.test$cell == cell.number,]
      df.cell <- df[df$cell == cell.number,]
      #for Naive bay
      df.test.cell$prediction <- naive.bayes.model.prediction(df.training.cell, df.test.cell)
      accuracy.na <- accuracy.naive.bayes(df.test.cell$prediction, df.test.cell$class)
      table.predic <- table(df.test.cell$prediction, df.test.cell$class)
      df.accuracy[cell.index,]$cell = cell.number
      df.accuracy[cell.index,]$naive_accuracy = accuracy.na
      
      plot.graph.train.and.test(df, cell.number, df.test.cell$prediction);
  
      
      #for MA
      df.test.tmp <- df[startline.of.test:nrow(df),]
      df.test.cell$ridership <- df.test.tmp[df.test$cell == cell.number,"ridership"]
      df.test.cell <- assign.df.test.cell.colum(df.test.cell)
      df.cell <- moving.average.model.prediction(df.cell)
      if("2010-01-31 00:30:00" %in% df.cell$timeslot){
        df.test.cell <- map.ma.data.to.naive.bayes(df.test.cell, df.cell)
        
        table.predict.naive.ma <- table(df.test.cell$predict_Naive_MA)
        count <- count(df.test.cell, "predict_Naive_MA")
        Naive.MA.accuracy  = (count[count$predict_Naive_MA == "TRUE",]$freq)/sum(count$freq)
        
        df.accuracy[cell.index,]$naive_ma_accuracy = Naive.MA.accuracy
        
    #     print("cell number")
    #     print(cell.number)
    #     print("only naive")
    #     print(accuracy.na)
    #     print("naive with ma")
    #     print(Naive.MA.accuracy)
    #     print(cell.index)
      }
      
    }
    
    #replace Na in accuracy ma with only naive
    for(index in 1:nrow(df.accuracy)){
      if(is.na(df.accuracy[index,]$naive_ma_accuracy)){
        df.accuracy[index, ]$naive_ma_accuracy <- df.accuracy[index, ]$naive_accuracy
      }
    }
    
    print("tolelance")
    print(tolelance)
    
    print("mean of all cell only naive")
    print(mean(df.accuracy$naive_accuracy, na.rm=TRUE))
    
    print("mean of all cell naive with ma")
    print(mean(df.accuracy$naive_ma_accuracy, na.rm=TRUE))
    write.accuracy.to.file(df.accuracy, cell.size, tolelance);
  }
  # rm(list = setdiff(ls(), lsf.str()))
}












