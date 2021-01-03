rankhospital <- function(state, outcome, num = "best") {
  data <- read.csv("PA3/outcome-of-care-measures.csv", colClasses = "character")
  arguments <- c("heart attack",  "heart failure", "pneumonia")
  if(!outcome %in% arguments) stop("invalid outcome")
  check <- data$State == state
  if(sum(check) == 0) stop("invalid state")
  
  table <- data[check, c(2, 7, 11, 17, 23)]
  colnames(table) <- c("name","state", "h_a", "h_f", "pn")
  for(i in c(3:5)) table[,i] <- as.numeric(table[,i])
  
  col <- match(outcome, arguments) + 2
  
  sort_col <- sort(table[,col], index.return = TRUE, na.last = TRUE)
  value <- sort_col$x
  index <- sort_col$ix  
  
  not_na_value <- sum(!is.na(value))
  
  if(num == "worst"){
    rank <- not_na_value
    hospital_num <- index[rank]
    return(table[hospital_num, 1])
  }
  
  if(num == "best"){
    hospital_num <- index[1]
    return(table[hospital_num, 1])
  }
  
  if(num <= not_na_value){
    hospital_num <- index[num]
    return(table[hospital_num, 1])
  }
  else return("NA")
}
rankhospital("TX", "heart failure", 5)

