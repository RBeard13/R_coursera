rankall <- function(outcome, num = "best") {
  data <- read.csv("PA3/outcome-of-care-measures.csv", colClasses = "character")
  data$State <- as.factor(data$State) 
  arguments <- c("heart attack",  "heart failure", "pneumonia")
  if(!outcome %in% arguments) stop("invalid outcome")

  disease <- c(11, 17, 23)
  col <- disease[match(outcome, arguments)]
  
  states <- levels(data$State)
  
  res <- as.data.frame(matrix(vector(),ncol=2))
  for(i in states){
    print(i)
    check <- data$State == i
    table <- data[check, c(2, 7, col)]
    colnames(table) <- c("name","state","disease")
    table[,3] <- as.numeric(table[,3])
  
    sort_col <- sort(table[,3], index.return = TRUE, na.last = TRUE)
    value <- sort_col$x
    index <- sort_col$ix  
  
    not_na_value <- sum(!is.na(value))
    
    if(num == "worst"){
      rank <- not_na_value
      hospital_num <- index[rank]
      add <- data.frame(table[hospital_num, 1], table[hospital_num, 2])
      res <- rbind(res, add)      
    }
  
    if(num == "best"){
      hospital_num <- index[1]
      add <- data.frame(table[hospital_num, 1], table[hospital_num, 2])
      res <- rbind(res, add) 
    }
  
    if(num <= not_na_value){
      hospital_num <- index[num]
      add <- data.frame(table[hospital_num, 1], table[hospital_num, 2])
      res <- rbind(res, add) 
    }
    else print(data.frame("NA", i))
      #res <- rbind(res, data.frame("NA", i))
  }
  res
}

rankall("heart failure", 17)
