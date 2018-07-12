#   Step: 1. Read file csv to dataframe
#         2. Make class from ridership and tolerance
#         3. Make input to factor for Naive Bayes model
#         4. Seperate trining and test data set of each cell
#         5. Trianing Naibe bayes model
#         6. Test data with model
#         7. Extract accuracy each cell from test data
#         8. Sum accuracy each cell to overall accuracy cell size

source("function.R")
start_time <- Sys.time()
#<-------------------------------------------Main------------------------------------------------>

cell.size <- 1500

df <- read.file.to.data.frame(cell.size)

tolelance <- c(5)

df$class <- make.class(df$ridership, tolerance)

df$day <- as.factor(df$day)
df$time <- as.factor(df$time)
df$class <- as.factor(df$class)

startline.of.test = get.start.line.of.test(df);
endline.of.trainning = startline.of.test - 1;

df.training <- select.trianing(df)  # 3 week
df.test <- select.test(df)  # 1 week

cell <- sort(unique(df.training$cell))

previous <- c(2)

df.accuracy <- data.frame(cell = numeric(0), naive_accuracy = numeric(0),  naive_ma_accuracy = numeric(0))


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
  
  # plot.graph.train.and.test(df, cell.number, df.test.cell$prediction);
  
  #for MA
  df.test.tmp <- df[startline.of.test:nrow(df),]
  df.test.cell$ridership <- df.test.tmp[df.test$cell == cell.number,"ridership"]
  df.test.cell <- assign.df.test.cell.colum(df.test.cell)
  df.cell <- moving.average.model.prediction(df.cell, previous)
  if("2010-01-31 00:30:00" %in% df.cell$timeslot){
    df.test.cell <- map.ma.data.to.naive.bayes(df.test.cell, df.cell)
    
    table.predict.naive.ma <- table(df.test.cell$predict_Naive_MA)
    count <- count(df.test.cell, "predict_Naive_MA")
    Naive.MA.accuracy  = (count[count$predict_Naive_MA == "TRUE",]$freq)/sum(count$freq)
    
    df.accuracy[cell.index,]$naive_ma_accuracy = Naive.MA.accuracy
    
    ma.df <- rbind(ma.df, data.frame(cell_size=cell.size,tolelance= tolelance ,cell_number = cell.number,previous= previous,naive.ma.accuracy = Naive.MA.accuracy))
    
  }
  
  # plot.graph.train.and.test(df, cell.number, df.test.cell$prediction);
}


#<-------------------------------------------END------------------------------------------------>
end_time <- Sys.time()
print(paste("Running time", end_time - start_time))
