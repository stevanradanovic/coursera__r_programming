corr <- function(directory, threshold = 0) {
    
    corr_data <- numeric(0)
    
    for (i in 1:332) {
        
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
        
        if (sum(complete.cases(pollution_data)) >= threshold) {
            corr_data <- c(corr_data, cor(pollution_data$nitrate, pollution_data$sulfate, use = 'na.or.complete'))
        }
    }
    
    corr_data
}