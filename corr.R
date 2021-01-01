corr <- function(directory, threshold = 0){
  dataFiles <- lapply(Sys.glob(file.path('specdata', "*.csv")), read.csv)
  cor <- c()
  for (i in 1:332){
    data <- dataFiles[[i]]
    data['both'] <- data$sulfate + data$nitrate
    data['nan'] <- !is.na(data$both)
    thres <- sum(data$nan)
    if(thres > threshold){
      sul <- data$sulfate[data$nan]
      nit <- data$nitrate[data$nan]
      cor <- c(cor, cor(sul, nit))
    }
  }
  cor
}  
  res <- corr('specdata')
  res
  summary(res)
  
  
  cr <- corr("specdata", 2000)                
  n <- length(cr)                
  cr <- corr("specdata", 1000)                
  cr <- sort(cr)
  print(c(n, round(cr, 4)))
  
  
  
  