rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
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
    fulldata = read.csv("outcome-of-care-measures.csv", colClasses = "character", 
                    na.strings='Not Available')
    fulldata = fulldata[order(fulldata$State), ]
    unique_states = unique(fulldata$State)
    hospitals = c()
    
    for (state in unique_states) {
        data = na.omit(fulldata[fulldata$State==state, c(2, outcome_col)])

        ## Convert to numeric all required values to compute the minimum
        data[outcome_field] = lapply(data[outcome_field], as.numeric)
        
        ## Sort data by name
        data = data[order(data$Hospital.Name), ]
        ## Sort data by outcome_field
        data = data[order(data[outcome_field]), ]
        
        ## Special cases for 'best' and 'worst'
        if (num == 'best') { 
            hospitals = c(hospitals, data$Hospital.Name[1])
        }
        else if (num == 'worst') {
            hospitals = c(hospitals, data$Hospital.Name[nrow(data)])
        }
        else {
            hospitals = c(hospitals, data$Hospital.Name[num])
        }
    }
    return (data.frame(hospital=hospitals, state=unique_states))
}