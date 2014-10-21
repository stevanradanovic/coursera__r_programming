complete <- function(directory, id = 1:332) {
    
    complete_data <- data.frame()
    
    for (i in id) {
        
        file_csv <- ''
        if (i < 10) {
            file_csv <- '00'
        } else if (i < 100) {
            file_csv <- '0'
        }
        
        file_csv <- paste(file_csv, as.character(i), sep = '')
        file_csv <- paste(file_csv, 'csv', sep = '.')
        file_csv <- paste(directory, file_csv, sep = '/')
        
        pollution_data <- read.csv(file_csv)
        
        complete_data <- rbind(complete_data, c(i, sum(complete.cases(pollution_data))))
    }
    
    colnames(complete_data) <- c('id', 'nobs')
    complete_data
}