rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv")
  
  outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  colnum <- c(11, 17, 23)
  
  if (!outcome %in% outcomes) stop("invalid outcome")
  
  
  i <- colnum[match(outcome, outcomes)]
  hospitals <- outcome_data[, c(2, 7, i)]
  hospitals[, 3] <- as.numeric(as.character(hospitals[, 3]))
  hospitals <- na.omit(hospitals)
  names(hospitals) <- c("hospital", "state", "rate")
  # Check that state and outcome are valid
  if (num == "best") {
    num <- 1
  } else if (num == "worst") {
  } else {
    num <- as.numeric(num)
    if (is.na(num)) {
      stop("invalid num")
    } else if (num > nrow(hospitals)) {
      return(NA)
    }
  }
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  df <- NULL
  
  for(state in levels(hospitals$state)) {
    hospitals_state <- hospitals[hospitals$state == state, ]
    
    if (num == "worst") {
      n <- nrow(hospitals_state)
    } else {
      n <- num
    }
    result <- hospitals_state[order(hospitals_state$rate, hospitals_state$hospital), c(1, 2)][n, ]
    result$state <- rep(state, nrow(result))
    df <- rbind(df, result)  
  }
  
  rownames(df) <- NULL
  
  return(df)
}
