#read data from a .csv file into data frame
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#to convert data of column 11(contains 30 day mortality rates),
#that was originally read as character into numeric
outcome[,11]<- as.numeric(outcome[,11])
#in built function for creating a histogram
hist(outcome[,11],col="gray",main='30-day mortality rates')
