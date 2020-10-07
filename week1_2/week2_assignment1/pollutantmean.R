pollutantmean <- function(directory, pollutant, id = 1:332) {
  numRows <- 0
  runningTotal <- 0
  
  for (i in id) {
      fileName <- constructIdFileName(i)
      filePath <- file.path(directory, fileName)
      table <- read.csv(filePath)
      rows <- table[pollutant]
      badRows <- is.na(rows)
      rows <- rows[!badRows]
      numRows <- numRows + length(rows)
      runningTotal <- runningTotal + sum(rows)
  }
  runningTotal/numRows
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