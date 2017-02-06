# The function rankal retreives the hospital with the given 30-day 
# mortality rank (lowest to highest) for each state
#         Args 
#           outcome: A character string specifying "heart failure", "heart 
#           attack" or "pneumonia".
#           
#           num: A number specifying the desired rank, "best" and "worst" are
#           are accepted values
#
#           Returns
#           A dataframe with the name of the hopsital with the given
#           30-day mortality for the desired state.  Ties in 30-day mortality 
#           are broken by alphabetical sorting of hospital names.

rankall <- function(outcome, num = "best") {
        data <- read.csv("outcome-of-care-measures.csv",
                         na.strings = "Not Available",
                         stringsAsFactors = FALSE)
        
        #Checking Arguement Validity
        valid.outcomes <-c("heart failure", "heart attack", "pneumonia")
        if(!any(outcome == valid.outcomes)) {
                stop("invalid outcome")
        }
        
        rank.list <- by(data, data[,7], function(x) {
                
        #Setting an index to specify desired outcome 30-day mortality
                switch(outcome,
                       "heart failure" = {col_index <- 17},
                       "heart attack" = {col_index <- 11},
                       "pneumonia" = {col_index <- 23})
                
        #Order by hopsitals by outcome and name, ommitting NAs
                order_index <- order(x[,col_index], x[,2], na.last = NA)        
                hospitals <- x[order_index, c(2, 7)]
                names(hospitals) <- c("hospital", "state")
                
        #Setting num for best and worst cases
                if (num == "best") {
                        num <- 1
                }
                if (num == "worst") {
                        num <- nrow(hospitals)
                }
        
        #Return Best hospital by outcome
                hospitals[num, ]
        })
        do.call(rbind, rank.list)
}