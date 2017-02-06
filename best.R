# The function best retreives the hospital with the lowest 30-day mortality for a 
# given state and outcome
#         Args 
#           state: The two character abbreviation of a US state.  
#           List of abbreviations can be found in state.abb
#           
#           outcome: A character string specifying "heart failure", "heart 
#           attack" or "pneumonia".
#       
#           Returns
#           A character vector with the name of the hopsital with the lowest 
#           30-day mortality for the desired state.  Ties in 30-day mortality 
#           are broken by alphabetical sorting of hospital names.

best <- function(state, outcome) {
                data <- read.csv("outcome-of-care-measures.csv",
                                 colClasses = "character")
                
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
                
        #Converting Mortality Rates to numeric for sorting
                data[,col_index] <- as.numeric(data[,col_index])
        
        #Order by hopsitals by outcome, ommitting NAs
               hospitals <- data[order(data[,col_index], na.last = NA), 2]
               
        #Return Best hospital by outcome
               hospitals[1]
        }
        
                

                 
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with lowest 30-day death
        ## rate
}

