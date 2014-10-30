best <- function(state, outcome) {
    
    ## Read outcome data
    outcome_data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
    
    ## Check that state and outcome are valid
    states <- unique(outcome_data$State)
    outcomes <- c('heart attack', 'heart failure', 'pneumonia')
    
    if (!(state %in% states)) {
        stop('invalid state')
    }
    if (!(outcome %in% outcomes)) {
        stop('invalid outcome')
    }
    
    ## Select hospitals in that state
    outcome_state <- outcome_data[outcome_data$State == state, c(2, 11, 17, 23)]
    
    ## Outcome logic
    columnNum <- if (outcome == 'heart attack') {
        2 # 11
    } else if (outcome == 'heart failure') {
        3 # 17
    } else {
        4 # 23
    }
    
    ## Consider only hospitals that have data for outcome
    outcome_state <- outcome_state[!is.na(as.numeric(outcome_state[, columnNum])), ]
    
    ## Minimal value
    minimalVal <- min(as.numeric(outcome_state[, columnNum]))
    
    ## Best hospitals
    best_hospitals <- outcome_state[as.numeric(outcome_state[, columnNum]) == minimalVal, ]
    
    ## Return hospital name in that state with lowest 30-day death rate
    best_hospitals[order(best_hospitals$Hospital.Name), ][1, 1]
}