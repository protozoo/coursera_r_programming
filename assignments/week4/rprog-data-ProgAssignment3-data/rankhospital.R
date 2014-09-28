rankhospital <- function(state, outcome, num = "best" ) {
    ## Read outcome data
    input <- read.csv( 'outcome-of-care-measures.csv', colClasses = "character" )
    
    ## Check that state and outcome are valid
    ## Get the column index for the rquested outcome
    ## If not present, return error
    if( outcome == "heart attack"){
        outcomeIndex <- 11
        reducedOutcomeIndex = 3
    }else if( outcome == "heart failure"){
        outcomeIndex <- 17
        reducedOutcomeIndex = 4
    }else if( outcome == "pneumonia"){
        outcomeIndex <- 23
        reducedOutcomeIndex = 5
    }else{
        stop( "invalid outcome")
    }
    
    ## filter by state
    input <- input[ input$State == state, ]
    if( nrow( input ) <= 0 )
        stop( "invalid state" )
    ## print( paste("step 0, filter states: ", nrow(input)) )
    
    ## grab just the requried fields
    input <- input[,c(2,7,11,17,23)]
    ## print( paste("step 1, reduce columns: ", nrow(input)) )
    
    ## convert to numeric
    input[,3] <- as.numeric(input[,3])
    input[,4] <- as.numeric(input[,4])
    input[,5] <- as.numeric(input[,5])
    
    ## shorten column names
    colnames(input) <- c("name", "state", "attack", "failure", "pneumo")
    
    ## remove hospitals with holes
    input <- input[complete.cases(input), ]    
    ## print( paste("step 2, remove holes: ", nrow(input)) )
    
    ## sort by requested outcome
    sorted <- input[ order( input[,reducedOutcomeIndex], input[,"name"], decreasing=F),]
    
    # return the requested value (best, worst, or index)
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    ## head( sorted )
    if( num == "best")
        result <- sorted[1,]$name
    else if( num == "worst")
        result <- sorted[nrow(sorted),]$name
    else
        result <- sorted[num,]$name
    result
    
   
}





## best( "TX", "heart attack")