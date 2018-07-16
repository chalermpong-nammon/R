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

accuracy.NB.each.cell <- data.frame(cell_size = numeric(0),
                                    tolerance = numeric(0),
                                    cell_number = numeric(0),
                                    accuracy_NB = numeric(0))

accuracy.NB <- data.frame(cell_size = numeric(0),
                             tolerance = numeric(0),
                             NB = numeric(0))

cell.size <- c(1500)
# for (cell.size in c(1500,1250,1000,750,500,250,100)){
  print(paste0("Cell size: ",cell.size))
  df <- read.file.to.data.frame(cell.size)
  unique.cell <- sort(unique(df$cell))
  
  tolerance <- c(20)
  # for(tolerance in seq(5, 40, by = 5)){
    print(paste0("tolerance is: ",tolerance))
    
    df$class <- make.class(df$ridership, tolerance)
    df <- make.factor.for.NB(df);
    
    df.training <- select.trianing(df)  # 3 week
    df.test <- select.test(df)  # 1 week
    
    
    df.accuracy <- data.frame(cell = numeric(0), NB_accuracy = numeric(0))
    for(cell.index in 1:length(unique.cell)){
      cell.number = unique.cell[cell.index]
      
      #prevent no have data in test or train cell
      df.cell <- df[df$cell == cell.number,]
      if(nrow(df.cell) != 1344) next
      
      df.training.cell <- df.training[df.training$cell == cell.number,]
      df.test.cell <- df.test[df.test$cell == cell.number,]
      
      #for Naive bayes
      df.test.cell$prediction <- naive.bayes.model.prediction(df.training.cell[,c("day","time","class")], df.test.cell[,c("day","time")])
      accuracy <- accuracy.naive.bayes(df.test.cell$prediction, df.test.cell$class)
      accuracy.NB.each.cell <- rbind(accuracy.NB.each.cell, data.frame(cell_size=cell.size,tolerance= tolerance ,cell_number = cell.number,accuracy_NB = accuracy))
      # table.predic <- table(df.test.cell$prediction, df.test.cell$class)
      
      df.accuracy[cell.index,]$cell = cell.number
      df.accuracy[cell.index,]$NB_accuracy = accuracy
      # plot.graph.train.and.test(df, cell.number, df.test.cell$prediction); 
    }
    print(paste0("Mean value of all cell NaiveBayes: ",mean(df.accuracy$NB_accuracy, na.rm=TRUE)))
    accuracy.NB <- rbind(accuracy.NB, data.frame(cell_size=cell.size,
                                                    tolerance= tolerance,
                                                    NB = mean(df.accuracy$NB_accuracy, na.rm=TRUE)))
  # }
# }
# write.accuracy.NB.to.file(accuracy.NB)
# df.NB.accuracy.cell.size <- save.accuracy.cell.size(accuracy.NB.each.cell)
end_time <- Sys.time()
print(paste("Running time", end_time - start_time))