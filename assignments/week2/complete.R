complete <- function(directory, id = 1:332) 
{
  ## create an emtpy data frame to store the results
  df = data.frame(id=integer(), nobs=integer())
  
  ## iterate over id's
  for( i in id ){
    ## build the file name and file path
    ## (this time i did it differently, based on some examples found in the internet, 
    ## since one of the unit tests didn't pass for some unknown reason)
    filename <- sprintf("%03d.csv", i)
    filepath <- paste(directory, filename, sep="/")
    
    ## read the data file and remove NA values
    csv = read.csv( filepath )
    clean <- csv[complete.cases(csv), ]
    
    ## get number of rows and store it along the file ID in the results dataframe
    n = nrow(clean)
    df <- rbind( df, c( id=i, nobs=n))
  }
  
  ## make sure the data frame columns are correctly named
  ## (looks like rbind removes it, not sure why...)
  colnames(df) <- c("id", "nobs")
  
  ## return the dataframe
  df
}
