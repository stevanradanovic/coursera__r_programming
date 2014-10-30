rankall <- function(outcome, num = 'best') {
    
    ## Read outcome data
    outcome_data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
    
    ## Check that outcome is valid
    outcomes <- c('heart attack', 'heart failure', 'pneumonia')
    if (!(outcome %in% outcomes)) {
        stop('invalid outcome')
    }
    columnNum <- if (outcome == outcomes[1]) {
        11
    } else if (outcome == outcomes[2]) {
        17
    } else if (outcome == outcomes[3]) {
        23
    }
    
    ## Split data by state
    outcome_data <- outcome_data[c(2, 7, columnNum)]
    outcome_by_state <- split(outcome_data, outcome_data$State)
    
    ## Make empty matrix for storing data
    hospitals_of_rank_matrix <- matrix(character(),
                                       nrow = length(outcome_by_state),
                                       ncol = 2)
    
    rank <- numeric(length(outcome_by_state))
    
    ## Handle num parameter
    if (num == 'worst') {        
        for (i in 1:length(outcome_by_state)) {
            rank[i] <- length(outcome_by_state[[i]][, 3][!is.na(as.numeric(outcome_by_state[[i]][, 3]))])
        }    
    } else if (num == 'best') {        
        rank <- rep(1, length(outcome_by_state))    
    } else {        
        rank <- rep(as.numeric(num), length(outcome_by_state))        
    }
    
    ## Rank hospitals
    i <- 1
    for (state in outcome_by_state) {
        hospitals_of_rank_matrix[i, 1] <- state[order(as.numeric(state[, 3]), state[, 1]), ][rank[i], 1]
        i <- i + 1
    }
    hospitals_of_rank_matrix[, 2] <- names(outcome_by_state)
    
    ## Make data-frame
    res <- data.frame(hospitals_of_rank_matrix, row.names = names(outcome_by_state))
    colnames(res) <- c('hospital', 'state')
    
    res
}