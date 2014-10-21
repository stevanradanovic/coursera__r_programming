pollutantmean <- function(directory, pollutant, id = 1:332) {
    whole_sum <- 0
    whole_num <- 0
    
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
        
        whole_sum <- whole_sum + sum(pollution_data[[pollutant]], na.rm = TRUE)
        whole_num <- whole_num + sum(!is.na(pollution_data[[pollutant]]))
    }
    
    whole_sum / whole_num
}