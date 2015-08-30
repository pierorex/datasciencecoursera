rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
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
    data = read.csv("outcome-of-care-measures.csv", colClasses = "character", 
                    na.strings='Not Available')
    data = na.omit(data[data$State==state, c(2, outcome_col)])
    
    ## If no rows found for given state => error
    if (nrow(data) == 0) stop("invalid state")
    
    ## Convert to numeric all required values to compute the minimum
    data[outcome_field] = lapply(data[outcome_field], as.numeric)
    
    ## Sort data by name
    data = data[order(data$Hospital.Name), ]
    ## Sort data by outcome_field
    data = data[order(data[outcome_field]), ]
    
    ## Special cases for 'best' and 'worst'
    if (num == 'best') return (data$Hospital.Name[1])
    if (num == 'worst') return (data$Hospital.Name[nrow(data)])
    
    ## Return the num-th best (by lowest outcome_field) hospital's name
    return (data$Hospital.Name[num])
}