best <- function(state, outcome) {
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
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  desiredcol <- as.numeric(finaldata[, outcome_col])
  desiredrows <- which(desiredcol == min(desiredcol))
  hospitalnames <- finaldata[desiredrows, 2]
  
  if (length(hospitalnames) > 1) {
    sortedlist <- sort(hospitalnames)
    sortedlist[1]
  }
  else {
    hospitalnames
  }
  
  }
