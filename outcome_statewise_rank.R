rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  Outcome = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% Outcome) { stop("invalid outcome")}
  
  State = unique(outcome_data[,7])
  if (!state %in% State) stop("invalid state")
  
  subdata <- subset(outcome_data, State == state)
  
  if (outcome == "heart attack") {
    outcome_col <- 11
  }
  else if (outcome == "heart failure") {
    outcome_col <- 17
  }
  else {
    outcome_col <- 23
  }
  
  finaldata <- as.numeric(subdata[,outcome_col])
  bad <- is.na(finaldata)
  finaldata <- subdata[!bad, ]
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  outcolname <- names(finaldata)[outcome_col]
  hosp_colname <- names(finaldata)[2]
  x <- with(finaldata, order(finaldata[outcolname], finaldata[hosp_colname]))
  ordered_finaldata <- finaldata[x, ]
  
  if (is.character(num) == TRUE) {
    if (num == "best") {
      num = 1
    }
    else if (num == "worst") {
      num = length(ordered_finaldata[, outcome_col])
    }
  }
  ordered_finaldata[num, 2]
}
