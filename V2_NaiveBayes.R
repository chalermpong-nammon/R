# Overview
#   Step: 1. Read file csv to dataframe
#         2. Make class from ridership and tolerance
#         3. Make input to factor for Naive Bayes model
#         4. Seperate trining and test data set of each cell
#         5. Trianing Naibe bayes model
#         6. Test data with model
#         7. Extract accuracy each cell from test data
#         8. Sum accuracy each cell to overall accuracy cell size

#load libraly
source("function.R")
start_time <- Sys.time()

# cell.size <- c(1500)
for (cell.size in c(1500,1250,1000,750,500)){
  print(paste0("Cell size: ",cell.size))
  df <- read.file.to.data.frame(cell.size)
  
  # tolelance <- c(20)
  for(tolelance in seq(5, 40, by = 5)){
    print(paste0("tolelance is: ",tolelance))
    
    df$class <- make.class(df$ridership, tolerance)
    
    df$day <- as.factor(df$day)
    df$time <- as.factor(df$time)
    df$class <- as.factor(df$class)
    
    startline.of.test = get.start.line.of.test(df);
    endline.of.trainning = startline.of.test - 1;
    
    df.training <- select.trianing(df)  # 3 week
    df.test <- select.test(df)  # 1 week
    
    cell <- sort(unique(df.training$cell))
    df.accuracy <- data.frame(cell = numeric(0), naive_accuracy = numeric(0),  naive_ma_accuracy = numeric(0))
    for(cell.index in 1:length(cell)){
      cell.number = cell[cell.index]
      df.training.cell <- df.training[df.training$cell == cell.number,]
      df.test.cell <- df.test[df.test$cell == cell.number,]
      df.cell <- df[df$cell == cell.number,]
      #for Naive bayes
      df.test.cell$prediction <- naive.bayes.model.prediction(df.training.cell, df.test.cell)
      accuracy.nb <- accuracy.naive.bayes(df.test.cell$prediction, df.test.cell$class)
      table.predic <- table(df.test.cell$prediction, df.test.cell$class)
      
      df.accuracy[cell.index,]$cell = cell.number
      df.accuracy[cell.index,]$naive_accuracy = accuracy.nb
      # plot.graph.train.and.test(df, cell.number, df.test.cell$prediction); 
    }
    print(paste0("Mean value of all cell NaiveBayes: ",mean(df.accuracy$naive_accuracy, na.rm=TRUE)))
    
    df.accuracy <- df.accuracy[,c("cell","naive_accuracy")]
    write.accuracy.to.file(df.accuracy, cell.size, tolelance)
  }
}
end_time <- Sys.time()
print(paste("Running time", end_time - start_time))