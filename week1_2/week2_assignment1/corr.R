corr <- function(directory, threshold = 0) {
  corVect <- c()
    for (i in c(1:332)) {
      fileName <- constructIdFileName(i)
      filePath <- file.path(directory, fileName)
      table <- read.csv(filePath)
      completeTable <- table[complete.cases(table), ]
      if (nrow(completeTable) >= threshold) {
        corVal <- cor(completeTable[[2]], completeTable[[3]])
        corVect <- append(corVect, corVal, after = length(corVect) )
      }
    }
    corVect
  }
  
  constructIdFileName <- function(i) {
    if ( i < 10) { 
      paste('0','0',i,'.csv',sep='')
    } else if (i<100) { 
      paste('0', i, '.csv', sep='')
    } else { 
      paste(i, '.csv', sep='')
    }
  }