rankall <- function( outcome, num = "best" ) {
    ## Read data
    rawinput <- read.csv( 'outcome-of-care-measures.csv', colClasses = "character" )
    
    ## grab just the requried fields
    input <- rawinput[ c(2,7,11,17,23) ]
    
    ## shorten column names
    colnames(input) <- c("name", "state", "heart attack", "heart failure", "pneumonia")   
    
    ## Check that outcome is valid    
    outcomes <- c( "heart attack", "heart failure", "pneumonia" )
    if( outcome %in% outcomes == F ) 
        stop( "invalid outcome" )
    
    ## clean invalid data
    input <- input[ input[ outcome ] != 'Not Available', ]
    
    ## get it sorted
    input[ outcome ] <- as.data.frame( sapply( input[ outcome ], as.numeric ) )
    input <- input[ order( input$name, decreasing = F), ]
    input <- input[ order( input[outcome ], decreasing = F), ]
    
    ## parse the num arg
    rankedHospital <- function(df, s, n) {
        df <- df[df$state==s, ]
        vals <- df[, outcome]
        if( n == "best" ) {
            rowNum <- which.min(vals)
        } else if( n == "worst" ) {
            rowNum <- which.max(vals)
        } else {
            rowNum <- n
        }
        df[rowNum, ]$name
    }
    
    ## iterate states and find the hospital at provided index
    states <- input[, 2]
    states <- unique( states )
    newdata <- data.frame("hospital"=character(), "state"=character())
    for(st in states) {
        hosp <- rankedHospital(input, st, num)
        newdata <- rbind( newdata, data.frame( hospital=hosp, state=st ))
    }
    
    ## Return df with names and states
    newdata <- newdata[order(newdata['state'], decreasing = FALSE), ]
    newdata
    
}




## best( "TX", "heart attack")