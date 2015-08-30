complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used

    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    df = data.frame(id=id, nobs=id)
    
    for (i in seq_along(id)){
        if (id[i] < 10)
            id_ = paste("00",paste(id[i],".csv",sep=""),sep="")
        else if (id[i] < 100)
            id_ = paste("0",paste(id[i],".csv",sep=""),sep="")
        else id_ = paste(id[i],".csv",sep="")

        csv = read.csv(paste(directory,id_,sep="/"))

        df[i,"id"] = id[i]
        df[i,"nobs"] = length(csv[!is.na(csv$nitrate) & !is.na(csv$sulfate),
                                  "ID"])
    }
    df
}