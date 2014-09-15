corr <- function(directory, threshold = 0) 
{  
  ## create a vector to hold the results
  correlations <- c()
  
  ## get all files
  all <- complete( directory, 1:332 )
  
  ## filter to get only the ones above threshold
  completed <- subset( all, nobs > threshold )  
  
  ## iterate over the ID column
  for(i in completed$id ) 
  {
    ## build file name and read it
    filename = paste( directory, "/", formatC( i, digits=2, flag=0), ".csv", sep="" )
    csv <- read.csv( filename )
    
    ## find the complete monitors & number of rows
    completeMonitors <- csv[complete.cases(csv),]
    numComplete <- nrow( completeMonitors )
    
    ## if above the threshold, add correlation value to the results vector
    if( numComplete >= threshold ) {
      correlations <- c(correlations, cor( completeMonitors$nitrate, completeMonitors$sulfate ) )
    }
  }
  
  ## return results vector
  correlations
}

