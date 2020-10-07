complete <- function(directory, id = 1:332){
  df <- data.frame(matrix(ncol=2,nrow=0))
  colnames(df) <- c("id", "nobs")
  for (i in id) {
    fileName <- constructIdFileName(i)
    filePath <- file.path(directory, fileName)
    table <- read.csv(filePath)
    completeTable <- table[complete.cases(table), ]
    df <- rbind(df, data.frame(id=i, nobs=nrow(completeTable)))
  }
  print(df)
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