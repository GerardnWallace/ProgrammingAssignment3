# The function rankhospital retreives the hospital with the given 30-day 
# mortality rank (lowest to highest) for a given state and outcome
#         Args 
#           state: The two character abbreviation of a US state.  
#           List of abbreviations can be found in state.abb
#           
#           outcome: A character string specifying "heart failure", "heart 
#           attack" or "pneumonia".
#           
#           num: A number specifying the desired rank, "best" and "worst" are
#           are accepted values
#
#           Returns
#           A character vector with the name of the hopsital with the given
#           30-day mortality for the desired state.  Ties in 30-day mortality 
#           are broken by alphabetical sorting of hospital names.


rankhospital <- function(state, outcome, num = "best") {
        data <- read.csv("outcome-of-care-measures.csv",
                         na.strings = "Not Available",
                         stringsAsFactors = FALSE)
        
        #Checking Arguement Validity
        if(!any(state == unique(data$State))) {
                stop("invalid state")
        }
        valid.outcomes <-c("heart failure", "heart attack", "pneumonia")
        if(!any(outcome == valid.outcomes)) {
                stop("invalid outcome")
        }
        
        #Setting an index to specify desired outcome 30-day mortality
        switch(outcome,
               "heart failure" = {col_index <- 17},
               "heart attack" = {col_index <- 11},
               "pneumonia" = {col_index <- 23})
        
        #Select data by state
        data <- data[data$State == state,]
        
        #Order by hopsitals by outcome and name, ommitting NAs
        order_index <- order(data[,col_index], data[,2], na.last = NA)        
        hospitals <- data[order_index, 2]
        
        #Setting num for best and worst cases
        if (num == "best") {
                num <- 1
        }
        if (num == "worst") {
                num <- length(hospitals)
        }
        
        #Return Best hospital by outcome
        hospitals[num]
}