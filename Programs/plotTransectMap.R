plotTransectMap <- function(locCode){
  
  transects <- STransects
  
  #   ---- Get the ith transect by itself, and make a nice buffer.
  t <- transects[transects@data$LocCode == as.character(droplevels(locCode)),]
  bt <- gUnaryUnion(gBuffer(t,width=50,byid=TRUE))

  #   ---- Make a simple plot identifying the ith transect. 
  plot(1,1,axes=FALSE,xlim=transects@bbox[1,],ylim=transects@bbox[2,],xlab="",ylab="")
  plot(bt,col="yellow",add=TRUE)
  plot(t,col="red",add=TRUE)
  plot(transects,add=TRUE)
}