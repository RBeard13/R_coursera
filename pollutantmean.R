pollutantmean <- function(directory, pollutant, id = 1:332){
  dataFiles <- lapply(Sys.glob(file.path(directory, "*.csv")), read.csv)
  sum <- 0
  quantity <- 0
  check <- c()
  for (i in id){
    data <- dataFiles[[i]]
    v_id <- data[[pollutant]]
    v_id <- v_id[!is.na(v_id)]
    sum <- sum + sum(v_id)
    quantity <- quantity + length(v_id)
  }
  ans <- sum / quantity
  }
check <- pollutantmean("specdata", 'nitrate')
check

