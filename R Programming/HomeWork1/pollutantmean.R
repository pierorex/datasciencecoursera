pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
    data = c()
    for (i in seq_along(id)){
        if (id[i] < 10)
            id_ = paste("00",paste(id[i],".csv",sep=""),sep="")
        else if (id[i] < 100)
            id_ = paste("0",paste(id[i],".csv",sep=""),sep="")
        else id_ = paste(id[i],".csv",sep="")
        
        csv = read.csv(paste(directory,id_,sep="/"))
        data = c(data, unlist(na.omit(csv[pollutant])))
    }
    mean(data)
}