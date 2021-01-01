complete <- function(directory, id = 1:332){
  dataFiles <- lapply(Sys.glob(file.path(directory, "*.csv")), read.csv)
  res <- as.data.frame(matrix(vector(),ncol=2))
  for (i in id){
    data <- dataFiles[[i]]
    data['both'] <- data$sulfate + data$nitrate
    data['nan'] <- !is.na(data$both)
    add <- data.frame(i, sum(data$nan))
    res <- rbind(res, add)
  }
  colnames(res) <- c('id', 'nubs')
  res
}
check <- complete("specdata", 54)
check
check$nubs
RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nubs"])
