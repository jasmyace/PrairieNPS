
#   ---- Read-in raw data. 
readData <- function(dataStem,file,col.names){
  
  read.table(file=paste0(dataStem,file),
             header=TRUE,
             sep=",",
             col.names=col.names,
             stringsAsFactors=FALSE)
}