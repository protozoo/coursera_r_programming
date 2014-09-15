pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## a vector to concatenate the total results
  total = c()
  
  ## iterate over id's
  for( i in id ){
    ## build the file name to be laoded (CSV file) and read it
    filename = paste( directory, "/", formatC( i, digits=2, flag=0), ".csv", sep="" )
    csv = read.csv( filename )
    
    ## get the requested column and remove NA values
    column = csv[pollutant]
    clean = column[!is.na(column)]
    
    # concatenate this CSV values to the totals vector
    total = c(total, clean)
  }
  
  ## compute the mean value, and round it to 3 digits (to make it match the expected results example)
  mean = mean(total)
  round( mean, digits=3 )
}