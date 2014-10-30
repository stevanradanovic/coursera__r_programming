rankhospital <- function(state, outcome, num = 'best') {
    
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
    
    ## Map 'best' and 'worst'
    rank <- if (num == 'best') {
        1
    } else if (num == 'worst') {
        length(outcome_state$Hospital.Name)
    } else {
        as.numeric(num)
    }
    
    ## Return hospital name in that state with the given rank 30-day death rate
    outcome_state[order(as.numeric(outcome_state[, columnNum]), outcome_state[, 1]), ][rank, 1]
}