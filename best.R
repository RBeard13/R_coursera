best <- function(state, outcome) {
  data <- read.csv("PA3/outcome-of-care-measures.csv", colClasses = "character")
  arguments <- c("heart attack",  "heart failure", "pneumonia")
  if(!outcome %in% arguments) stop("invalid outcome")
  check <- data$State == state
  if(sum(check) == 0) stop("invalid state")
  
  table <- data[check, c(2, 7, 11, 17, 23)]
  colnames(table) <- c("name","state", "h_a", "h_f", "pn")
  for(i in c(3:5)) table[,i] <- as.numeric(table[,i])
  
  col <- match(outcome, arguments) + 2
  
  search_min <- table[!is.na(table[,col]),col]
  lowest <- min(search_min)
  hospital <- (table[,col] == lowest & (!is.na(table[,col])))
  table[hospital, 1]
}

best("BB", "heart attack")




