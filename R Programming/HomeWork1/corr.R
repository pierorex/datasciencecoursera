corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    id = sapply(dir(directory), 
                function(filename) {
                    as.numeric(substr(filename,1,3))
                })

    df = complete(directory, id)
    cors = c()
    for (i in 1:nrow(df)) {
        if (df[i,2] > threshold) {
            if (df[i,1] < 10)
                id_ = paste("00",paste(df[i,1],".csv",sep=""),sep="")
            else if (df[i,1] < 100)
                id_ = paste("0",paste(df[i,1],".csv",sep=""),sep="")
            else id_ = paste(df[i,1],".csv",sep="")
            
            csv = read.csv(paste(directory,id_,sep="/"))
            bad = is.na(csv$nitrate) | is.na(csv$sulfate) |
                  is.na(csv$Date) | is.na(csv$ID)
            csv_sulfate = csv[!bad,"sulfate"]
            csv_nitrate = csv[!bad,"nitrate"]
            cors = c(cors, cor(csv_sulfate, csv_nitrate))
        }
    }
    cors
}