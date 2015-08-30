best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    ## Filter by given outcome
    if (outcome == "heart attack") { 
        outcome_field = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        outcome_col = 11
    }
    else if (outcome == "heart failure") {
        outcome_field = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        outcome_col = 17
    }
    else if (outcome == "pneumonia") {
        outcome_field = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        outcome_col = 23
    }
    else { stop("invalid outcome") }
    
    ## Read data and filter by given state
    data = read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings='Not Available')
    data = na.omit(data[data$State==state, c(2,11,17,23)])
    
    ## If no rows found for given state => error
    if (nrow(data) == 0) stop("invalid state")
    
    ## Convert to numeric all required values to compute the minimum
    data[outcome_field] = lapply(data[outcome_field], as.numeric)
    
    ## Sort data by name
    data = data[order(data$Hospital.Name), ]
    
    ## Return the best (lowest outcome_field) hospital's name
    minimum = min(data[outcome_field])
    best_hospital = data[outcome_field] == minimum
    return (data$Hospital.Name[best_hospital])
}