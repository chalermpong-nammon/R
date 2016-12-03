library(plyr);
library(e1071);
library(caret);
library(TTR);

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

# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Above this is function and library ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#   Step: 1. Read file csv to dataframe
#         2. Make class from ridership and tolerance
#         3. Make input to factor for Naive Bayes model
#         4. Seperate trining and test data set of each cell
#         5. Trianing Naibe bayes model
#         6. Test data with model
#         7. Extract accuracy each cell from test data
#         8. Sum accuracy each cell to overall accuracy cell size
root.path = "/Users/chalermpongsomdulyawat/Desktop/Grad_workspace/"
accuracy.NB.each.cell <- data.frame(cell_size = numeric(0),
                                    tolerance = numeric(0),
                                    cell_number = numeric(0),
                                    accuracy_NB = numeric(0))

df.NB.MA.summary <- data.frame(cell_size = numeric(0),
                               tolerance = numeric(0),
                               previous = numeric(0),
                               cell_number = numeric(0),
                               count_true = numeric(0),
                               count_false = numeric(0),
                               NB_MA_accuracy = numeric(0))

accuracy.NB.MA <- data.frame(cell_size = numeric(0),
                             tolerance = numeric(0),
                             previous= numeric(0),
                             NB = numeric(0),
                             NB_MA = numeric(0))



for(cell.size in c(750,500)){
# for(cell.size in c(500)){
  print(cell.size)
  df <- read.file.to.data.frame(cell.size)
  unique.cell <- sort(unique(df$cell))
  
  for(tolerance in seq(5,20, by = 5)){
    print(tolerance)
    # for(tolerance in c(10)){
    df$class <- make.class(df$ridership, tolerance)
    df <- make.factor.for.NB(df);
    
    for(previous in c(2:10)) {
      df.accuracy <- data.frame(cell = numeric(0), NB_accuracy = numeric(0),  NB_MA_accuracy = numeric(0))
      
      for(cell.index in 1:length(unique.cell)) {
        cell.number = unique.cell[cell.index]
        
        df.cell <- df[df$cell == cell.number,]
        if(nrow(df.cell) != 1344) next
  
        df.cell$MAf <- simple.moving.average.model.prediction(df.cell$ridership, previous)
        df.cell$MA_error <- process.MA.error(df.cell)
        
        df.cell.trianing <- select.trianing(df.cell)
        df.cell.test <- select.test(df.cell)
        
        df.cell.test$prediction <- naive.bayes.model.prediction(df.cell.trianing[,c(4,5,6)], df.cell.test[,c(4,5,6)])
        accuracy.NB <- accuracy.naive.bayes(df.cell.test$prediction, df.cell.test$class)
        accuracy.NB.each.cell <- rbind(accuracy.NB.each.cell, data.frame(cell_size=cell.size,tolerance= tolerance ,cell_number = cell.number,accuracy_NB = accuracy.NB))
      
        df.cell.test <- assign.df.cell.test.colum(df.cell.test)
        df.cell.test <- map.MA.to.NB(df.cell.test, nrow(df.cell.trianing))
        
        count.true <- length(which(df.cell.test$predict_NB_MA == "TRUE")) 
        count.false <- length(which(df.cell.test$predict_NB_MA == "FALSE")) 
        NB.MA.accuracy  = count.true/length(df.cell.test$predict_NB_MA)
        
        df.accuracy[cell.index,]$cell = cell.number
        df.accuracy[cell.index,]$NB_accuracy = accuracy.NB
        df.accuracy[cell.index,]$NB_MA_accuracy = NB.MA.accuracy
        
        df.NB.MA.summary <- rbind(df.NB.MA.summary, data.frame(cell_size=cell.size,
                                                               tolerance= tolerance,
                                                               previous = previous,
                                                               cell_number = cell.number,
                                                               count_true = count.true,
                                                               count_false = count.false,
                                                               NB_MA_accuracy = NB.MA.accuracy))
      }
      accuracy.NB.MA <- rbind(accuracy.NB.MA, data.frame(cell_size=cell.size,
                                                         tolerance= tolerance,
                                                         previous= previous,
                                                         NB = mean(df.accuracy$NB_accuracy, na.rm=TRUE),
                                                         NB.MA = mean(df.accuracy$NB_MA_accuracy, na.rm=TRUE)))
      
    }
  }
}

df.NB.accuracy.cell.size <- save.accuracy.cell.size(accuracy.NB.each.cell)
